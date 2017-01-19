
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
import           FileSystemFileServerAPI
import           FileSystemAuthServerAPI  hiding (API)
import           FileSystemDirectoryServerAPI  hiding (API, fileVersion)


startApp :: IO ()    -- set up wai logger for service to output apache style logging for rest calls
startApp = withLogging $ \ aplogger -> do
  warnLog "Starting File Server."

  forkIO $ taskScheduler 5

  let settings = setPort 8083 $ setLogger aplogger defaultSettings
  runSettings settings app

taskScheduler :: Int -> IO ()
taskScheduler delay = do
  warnLog $ "Task scheduler operating."

  threadDelay $ delay * 1000000
  -- TODO - look through secondary records - check DIR_INFORM flag to see if the directory service has been informed - if not inform it
  -- TODO look through secondary records for REPL flag to check any files that havent been replicated and replicate them to at least one other server
  taskScheduler delay -- tail recursion

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server =  writeToFile
          :<|> readFromFile
          :<|> notify
          :<|> duplicate
  where
    writeToFile :: WriteFileReq -> Handler WriteFileResp
    writeToFile wr@(WriteFileReq token name contents) = liftIO $ do
      files <- searchFiles name

      case (length files) of
        1 -> do   --updating existing file
          let newVersion = (getIncrementedFileVersion (head files))
          let updatedFile = DBFile name newVersion (extractFileData wr) True True (duplicated (head files))
          overwriteFile updatedFile
          return $ WriteFileResp True newVersion

        0 -> do -- this is a new file
          overwriteFile $ DBFile name "1" (extractFileData wr) False False [] 
          return $ WriteFileResp True "1"

        _ -> do   --error
          errorLog $ "more than 1 file found for " ++ name
          return $ WriteFileResp False "-1"

    readFromFile :: ReadFileReq -> Handler ReadFileResp
    readFromFile rfr@(ReadFileReq token name) = liftIO $ do
      files <- searchFiles name
      case (length files) of
        1 -> do
          let file = head files
          let encFileData = encryptString (fileData file) (getSeedFromToken rfr)
          return $ ReadFileResp True "ALL G" encFileData (fileVersion file)

        0 -> do
          return $ ReadFileResp False ("No record for " ++ name) "NOTHING" "-1"

        _ -> do
          errorLog $ "Search for " ++ name ++ " yielded " ++ (show (length files)) ++ " results, expected 1 or 0"
          return $ ReadFileResp False "FUBAR - check file server logs" "Nothing" "-1" 

    notify :: Notification -> Handler Bool
    notify listUpdate = liftIO $ do
      clearFileServerLocations
      updateFileServerLocations (fileServerRecords ((notification listUpdate)))
      return True

    duplicate :: DBFile -> Handler Bool
    duplicate dbFile = liftIO $ do
      overwriteFile dbFile { duplicate = True}
      return True

clearFileServerLocations :: IO ()
clearFileServerLocations = do
  withMongoDbConnection $ delete (select [] "fileServerLocations")

-- Store list of file server records
updateFileServerLocations :: [FileServerRecord] -> IO ()
updateFileServerLocations (record@(FileServerRecord h p load size):records) = do
  withMongoDbConnection $ upsert (select ["fsHost" =: h, "fsPort" =: p] "fileServerLocations") $ toBSON record
  updateFileServerLocations records

updateFileServerLocations [] = return ()

-- store or update a file
overwriteFile :: DBFile -> IO ()
overwriteFile file@(DBFile name _ _ _ _ _) = do
  withMongoDbConnection $ upsert (select ["fileName" =: name] "files") $ toBSON file
  
-- return all files matching "fileName"
searchFiles :: String -> IO [DBFile]
searchFiles name = do
  files <- withMongoDbConnection $ do
    docs <- find (select ["fileName" =: name] "files") >>= drainCursor
    return $ catMaybes $ DL.map (\ b -> fromBSON b :: Maybe DBFile) docs
  return files

getIncrementedFileVersion :: DBFile -> String 
getIncrementedFileVersion (DBFile _ version _  _ _ _) = show ((read version :: Int) + 1)

extractFileData :: WriteFileReq -> String
extractFileData wfr@(WriteFileReq token name encData) = decryptString encData (recKey1Seed (getReceiverToken wfr))
  
getReceiverToken :: WriteFileReq -> ReceiverToken
getReceiverToken (WriteFileReq token _ _) = read (decryptString token key2Seed) :: ReceiverToken

getSeedFromToken :: ReadFileReq -> String
getSeedFromToken (ReadFileReq token _) = recKey1Seed (read (decryptString token key2Seed) :: ReceiverToken)

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




