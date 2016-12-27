
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
import           FileSystemAuthServerAPI hiding (Lib, API)

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

  -- TODO - grab cache data
  -- TODO inform file servers of eachothers existence
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
    -- Resolve A File Location
    -- The logic here is as follows:
    --  -> The client intends to READ a file
    --    -> the file exists
    --      -> if cache hit then send cached otherwise send the FileRecord for the primary server to the client
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
    resolveFile rr@(ResolutionRequest name intention token) = liftIO $ do
      noticeLog $ "Resolving " ++ name
      -- search for the primary record for the current file
      primaryRecord <- getPrimaryRecord name

      -- what does the client intend to do to this file?
      case intention of
        "READ"    -> do
          
          case primaryRecord of
            Just record   ->  do

              cacheHit <- cachePromote record

              case cacheHit of
                Just cached   -> do

                  let rToken = read (decryptString (cacheData cached) key2Seed) :: ReceiverToken 
                  let encFile = encryptString (cacheData cached) (recKey1Seed rToken)
                  return $ ResolutionResponse True record True encFile

                Nothing       -> return $ ResolutionResponse True record False ""

            Nothing       -> do   --this is an ERROR state
              errorLog "This joker is trying to read a file that doesn't exist"
              return $ negativeResolutionResponse

        "WRITE"   ->
          
          case primaryRecord of
            Just record   -> do
              cacheInvalidate record
              return $ ResolutionResponse True record False "" 

            Nothing       -> do   -- add new primary record to fileRecords
              bestFS <- selectAppropriateFileServer
              let newRecord = FileRecord "PRIMARY" name "1"  bestFS
              addRecord newRecord
              return $ ResolutionResponse True newRecord False ""

        _         -> do  --this is an ERROR state
          
          errorLog "This joker doesn't know whether he's reading or writing"
          return $ negativeResolutionResponse
        
    --used by the file servers to insert secondary records as they propogate
    --TODO check that it is inserting the right thing
    insertFileRecord :: FileRecord -> Handler Bool
    insertFileRecord record = liftIO $ do 
      addRecord record
      return True


    insertServerRecord :: FileServerRecord -> Handler Bool
    insertServerRecord fsr@(FileServerRecord host port _ _) = liftIO $ do
      withMongoDbConnection $ upsert (select ["fsHost" =: host, "fsPort" =: port] "fileServers") $ toBSON fsr
      return True

--TODO change all the dbs string names in withMongoDB calls to variables

-- Promotes the file in the cache. This means the following:
--  -> If there is no cache entry, an empty cache record is stored in the DB.
--    This empty record will be filled with file data by the task scheduler on the next
--    opportunity. Nothing will be returned in this case. 
--  -> If there is a cache entry and this entry is filled with data, it's weight will be increased
--    and Just CacheRecord will be returned
--  -> If there is a cache entry but the entry is not filled with data, it's weight will be increased
--    and Nothing will be returned

cachePromote :: FileRecord -> IO (Maybe CacheRecord)
cachePromote (FileRecord _ name version  _) = do
  cacheRecords <- withMongoDbConnection $ do
    docs <- find (select ["cacheName" =: name] "cacheRecords") >>= drainCursor
    return $ catMaybes $ DL.map (\ b -> fromBSON b :: Maybe CacheRecord) docs

  case (length cacheRecords) of
    1 -> do
      let record = head cacheRecords

      --update weight
      let updatedRecord = record { cacheWeight = (incrementWeight (cacheWeight record)) } 
      withMongoDbConnection $ upsert (select ["cacheName" =: name] "cacheRecords") $ toBSON updatedRecord

      if cacheFilled record
      then return $ Just record
      else return Nothing

    0 -> do 
      let record = CacheRecord name version  "" False "0" 
      withMongoDbConnection $ upsert (select ["cacheName" =: name] "cacheRecords") $ toBSON record
      return Nothing
      

cacheInvalidate :: FileRecord -> IO ()
cacheInvalidate (FileRecord _ name _ _) = do
  withMongoDbConnection $ do
    delete (select ["cacheName" =: name] "cacheRecords")


incrementWeight :: String -> String
incrementWeight w = show ((read w :: Int) + 1)

addRecord :: FileRecord -> IO ()
addRecord r@(FileRecord _ name _ _) = do
  withMongoDbConnection $ upsert (select ["fileRecordName" =: name] "fileRecords") $ toBSON r

--Selects a file server for the new primary record based on the load currently (most recently)
--on the server. This is currently based on smallest current size
--TODO ERROR CHECKING
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
    docs <- find (select ["fileRecordName" =: name, "recordType" =: primaryIdentifier] "fileRecords") >>= drainCursor
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




