
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Lib
    ( startApp
    ) where

import           Control.Concurrent           (forkIO, threadDelay)
import           Control.Monad                (when)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Except   (ExceptT)
import           Control.Monad.Trans.Resource
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Bson.Generic
import qualified Data.ByteString.Lazy         as L
import qualified Data.List                    as DL
import           Data.Maybe                   (catMaybes)
import           Data.Text                    (pack, unpack)
import           Data.Time.Clock              (UTCTime, getCurrentTime)
import           Data.Time.Format             (defaultTimeLocale, formatTime)
import           Database.MongoDB
import           GHC.Generics
import           Network.HTTP.Client          (defaultManagerSettings,
                                               newManager)
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Logger
import           RestClient
import           Servant
import qualified Servant.API                  as SC
import qualified Servant.Client               as SC
import           System.Environment           (getArgs, getProgName, lookupEnv)
import           System.Log.Formatter
import           System.Log.Handler           (setFormatter)
import           System.Log.Handler.Simple
import           System.Log.Handler.Syslog
import           System.Log.Logger
import           FileSystemDirectoryServerAPI

startApp :: IO ()    -- set up wai logger for service to output apache style logging for rest calls
startApp = withLogging $ \ aplogger -> do
  warnLog "Starting Directory Server."

  --NOTE task scheduler frequency increased to cater for performing cache retrievals
  --don't want to do this as part of resolution request as it would slow down response on that
  --request
  forkIO $ taskScheduler 5

  let settings = setPort 8084 $ setLogger aplogger defaultSettings
  runSettings settings app

taskScheduler :: Int -> IO ()
taskScheduler delay = do
  warnLog $ "Task scheduler operating."

  threadDelay $ delay * 1000000
  taskScheduler delay -- tail recursion

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server =  resolveFile
          :<|> insertServerRecord
          :<|> insertFileRecord
  where
    -- The logic here is as follows:
    --  -> The client intends to READ a file
    --    -> the file exists
    --      -> send the FileRecord for the primary server to the client
    --      -> add weight to that file in the cache (add to cache)
    --    -> the file doesn't exist
    --      -> return False
    --
    --  -> The client intends to WRITE to a file
    --    -> the file exists
    --      -> send the FileRecord for the primary server to the client
    --      -> invalidate the cache record
    --    -> the file doesn't exist
    --      -> return a FileRecord for a file server based on the recent load on the servers
    --
    -- Note - adding things to the cache simply adds an empty 
    resolveFile :: ResolutionRequest -> Handler ResolutionResponse
    resolveFile rr@(ResolutionRequest name intention) = liftIO $ do

      -- search for the primary record for the current file
      primaryRecord <- getPrimaryRecord name

      -- what does the client intend to do to this file?
      case intention of
        "read"    -> do
          
          case primaryRecord of
            Just record   ->  do
              --cachePromote record
              return $ ResolutionResponse True record

            Nothing       -> do   --this is an ERROR state
              errorLog "This joker is trying to read a file that doesn't exist"
              return $ nullResolutionResponse

        "write"   ->
          
          case primaryRecord of
            Just record   -> do
              --cacheInvalidate record
              return $ ResolutionResponse True record 

            Nothing       -> do   -- add new primary record to fileRecords
              bestFS <- selectAppropriateFileServer
              let newRecord = FileRecord "PRIMARY" name "1"  bestFS
              addRecord newRecord
              return $ ResolutionResponse True newRecord

        _         -> do  --this is an ERROR state
          
          errorLog "This joker doesn't know whether he's reading or writing"
          return $ nullResolutionResponse
        
    --used by the file servers to insert secondary records as they propogate
    insertFileRecord :: FileRecord -> Handler Bool
    insertFileRecord _ = return True


    insertServerRecord :: FileServerRecord -> Handler Bool
    insertServerRecord fsr@(FileServerRecord host _ _ _) = liftIO $ do
      withMongoDbConnection $ upsert (select ["fsHost" =: host] "fileServers") $ toBSON fsr
      return True
    

addRecord :: FileRecord -> IO ()
addRecord r@(FileRecord _ name _ _) = do
  withMongoDbConnection $ upsert (select ["fileName" =: name] "fileRecord") $ toBSON r

--Selects a file server for the new primary record based on the load currently (most recently)
--on the server. This is currently based on smallest current size
selectAppropriateFileServer :: IO FileServerRecord
selectAppropriateFileServer = do
  fileServers <- withMongoDbConnection $ do
    docs <- find (select [] "fileServers") >>= drainCursor
    return $ catMaybes $ DL.map (\ b -> fromBSON b :: Maybe FileServerRecord) docs

  return $ filterSmallestSize fileServers (head fileServers)

filterSmallestSize :: [FileServerRecord] -> FileServerRecord -> FileServerRecord
filterSmallestSize (x:xs) currentSmallest 
  | (read (currentSize x) :: Int) > (read (currentSize currentSmallest) :: Int) = filterSmallestSize xs x
  | otherwise = filterSmallestSize xs currentSmallest

filterSmallestSize [] currentSmallest = currentSmallest
  
--attempts to return the primary record for the file
getPrimaryRecord :: String -> IO (Maybe FileRecord)
getPrimaryRecord name = do
  files <- withMongoDbConnection $ do
    let primaryIdentifier = "PRIMARY" :: String       --TODO MOVE THIS TO API
    docs <- find (select ["fileName" =: name, "recordType" =: primaryIdentifier] "fileRecords") >>= drainCursor
    return $ catMaybes $ DL.map (\ b -> fromBSON b :: Maybe FileRecord) docs

  case (length files) of
    0 -> return Nothing 
    _ -> return (Just (head files))

-- | error stuff
custom404Error msg = err404 { errBody = msg }


-- | Logging stuff
iso8601 :: UTCTime -> String
iso8601 = formatTime defaultTimeLocale "%FT%T%q%z"

-- global loggin functions
debugLog, warnLog, errorLog :: String -> IO ()
debugLog = doLog debugM
warnLog  = doLog warningM
errorLog = doLog errorM
noticeLog = doLog noticeM

doLog f s = getProgName >>= \ p -> do
                t <- getCurrentTime
                f p $ (iso8601 t) ++ " " ++ s

withLogging act = withStdoutLogger $ \aplogger -> do

  lname  <- getProgName
  llevel <- logLevel
  updateGlobalLogger lname
                     (setLevel $ case llevel of
                                  "WARNING" -> WARNING
                                  "ERROR"   -> ERROR
                                  _         -> DEBUG)
  act aplogger


-- | Mongodb helpers...

withMongoDbConnection :: Action IO a -> IO a
withMongoDbConnection act  = do
  ip <- mongoDbIp
  port <- mongoDbPort
  database <- mongoDbDatabase
  pipe <- connect (host ip)
  ret <- runResourceT $ liftIO $ access pipe master (pack database) act
  close pipe
  return ret

drainCursor :: Cursor -> Action IO [Document]
drainCursor cur = drainCursor' cur []
  where
    drainCursor' cur res  = do
      batch <- nextBatch cur
      if null batch
        then return res
        else drainCursor' cur (res ++ batch)

-- | Environment variable functions, that return the environment variable if set, or
-- default values if not set.

-- | The IP address of the mongoDB database that devnostics-rest uses to store and access data
mongoDbIp :: IO String
mongoDbIp = defEnv "MONGODB_IP" id "database" True

-- | The port number of the mongoDB database that devnostics-rest uses to store and access data
mongoDbPort :: IO Integer
mongoDbPort = defEnv "MONGODB_PORT" read 27017 False -- 27017 is the default mongodb port

-- | The name of the mongoDB database that devnostics-rest uses to store and access data
mongoDbDatabase :: IO String
mongoDbDatabase = defEnv "MONGODB_DATABASE" id "USEHASKELLDB" True

-- | Determines log reporting level. Set to "DEBUG", "WARNING" or "ERROR" as preferred. Loggin is
-- provided by the hslogger library.
logLevel :: IO String
logLevel = defEnv "LOG_LEVEL" id "DEBUG" True

defEnv :: Show a
              => String        -- Environment Variable name
              -> (String -> a)  -- function to process variable string (set as 'id' if not needed)
              -> a             -- default value to use if environment variable is not set
              -> Bool          -- True if we should warn if environment variable is not set
              -> IO a
defEnv env fn def doWarn = lookupEnv env >>= \ e -> case e of
      Just s  -> return $ fn s
      Nothing -> do
        when doWarn (doLog warningM $ "Environment variable: " ++ env ++
                                      " is not set. Defaulting to " ++ (show def))
        return def




