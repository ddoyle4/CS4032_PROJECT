
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
import           Network.HTTP.Simple hiding (Proxy)

-- All instances of file server will be compiled with
-- a unique identifier
uniqueIdentifier = "FILE_SERVER_2"

startApp :: IO ()    -- set up wai logger for service to output apache style logging for rest calls
startApp = withLogging $ \ aplogger -> do
  warnLog "Starting File Server."

  forkIO $ taskScheduler 10

  let settings = setPort 8083 $ setLogger aplogger defaultSettings
  runSettings settings app

taskScheduler :: Int -> IO ()
taskScheduler delay = do
  warnLog $ "Task scheduler operating."

  threadDelay $ delay * 1000000

  unregisteredFiles <- getUnregisteredFiles
  updateDirectoryServer unregisteredFiles

  files <- allDBFiles
  replicateFiles files
  noticeLog $ "Finished Replicating All Files"
  taskScheduler delay -- tail recursion


-------- BACKGROUND TASKS --------

-- Returns a list of files that have yet to be registered with the Directory Server
getUnregisteredFiles :: IO [DBFile]
getUnregisteredFiles = do
  files <- withMongoDbConnection $ do
    docs <- find (select ["registered" =: False] "files") >>= drainCursor
    return $ catMaybes $ DL.map (\ b -> fromBSON b :: Maybe DBFile) docs
  return files
  
-- Inserts a secondary file recored into the Directory Servers's directory of files
updateDirectoryServer :: [DBFile] -> IO Bool
updateDirectoryServer (file@(DBFile name version _ _ _ _ _):files) = do
  myRecord <- getServerRecord uniqueIdentifier 
  let thisServer = FileServerRecord (fsHost myRecord) (fsPort myRecord) "0" "0"
      record = FileRecord "SECONDARY" name version thisServer
  dirServer <- getServerRecord "DIR_SERVER"
  performDirServerUpdate record dirServer
  overwriteFile $ file { registered = True }
  updateDirectoryServer files

updateDirectoryServer [] = return True

-- Performs the acutal POST call for Directory Server Updates
performDirServerUpdate :: FileRecord -> FileServerRecord -> IO Bool
performDirServerUpdate req (FileServerRecord h p _ _) = do
  let str = "POST http://" ++ h ++  ":" ++ p ++ "/insertFileRecord"
  let initReq = parseRequest_ str
  let request = setRequestBodyJSON req $ initReq
  response <- httpJSON request
  let ret = (getResponseBody response :: Bool)
  return ret
 
-- Returns the location of a specified file server in the system
getServerRecord :: String -> IO FileServerRecord
getServerRecord name = do
  records <- withMongoDbConnection $ do
    docs <- find (select ["serverName" =: name] "systemServerRecords") >>= drainCursor
    return $ catMaybes $ DL.map (\ b -> fromBSON b :: Maybe FileSystemServerRecord) docs

  return $ ( serverLocation (head records))

-- Replicates a list of files on suitable servers and upates the files in the
-- current database with the locations of their replications
replicateFiles :: [DBFile] -> IO ()
replicateFiles (file@(DBFile name _ _ isDuplicated isDirty copies _):files) = do
  servers <- leastLoadServers
  if isDirty
  then do
    replicateFile file copies
    overwriteFile $ file { duplicateDirty = False }
  else when ((not isDuplicated) && ((length copies) == 0)) $ do
    noticeLog $ "going to replicate " ++ name
    let replicationServers = take idealNumberDuplicated servers
    replicateFile file replicationServers 
    overwriteFile $ file { duplicated = replicationServers}
    noticeLog $ "replicated " ++ name ++  " on " ++ (show servers)
  replicateFiles files

replicateFiles [] = return ()

-- Replicates a given file on the given file server
replicateFile :: DBFile -> [FileServerRecord] -> IO ()
replicateFile file (server:servers) = do
  sendFile file server
  replicateFile file servers

replicateFile _ [] = return ()

-- The POST call for duplicating files between file servers
sendFile :: DBFile -> FileServerRecord -> IO Bool
sendFile file (FileServerRecord h p _ _) = do
  let str = "POST http://" ++ h ++  ":" ++ p ++ "/duplicate"
  let initReq = parseRequest_ str
  let request = setRequestBodyJSON file $ initReq
  response <- httpJSON request
  let ret = (getResponseBody response :: Bool)
  return ret
 

-------- THE MAIN APPLICATION --------

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server =  writeToFile
          :<|> readFromFile
          :<|> notify
          :<|> duplicate
          :<|> writeShadow
          :<|> commitShadow
          :<|> abortShadow
          :<|> discovery
  where
    -- Updates the value of a file on the file server
    writeToFile :: WriteFileReq -> Handler WriteFileResp
    writeToFile wr@(WriteFileReq token name contents) = liftIO $ do
      files <- searchFiles name

      case (length files) of
        1 -> do   --updating existing file
          let newVersion = (getIncrementedFileVersion (head files))
          let updatedFile = DBFile name newVersion (extractFileData wr) False True (duplicated (head files)) True
          overwriteFile updatedFile
          return $ WriteFileResp True newVersion

        0 -> do -- this is a new file
          overwriteFile $ DBFile name "1" (extractFileData wr) False False [] True
          return $ WriteFileResp True "1"

        _ -> do   --error
          errorLog $ "more than 1 file found for " ++ name
          return $ WriteFileResp False "-1"

    -- Returns the value from a file on the server
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

    -- Allows this server to be notified of the presence of other file servers. Used during replication.
    notify :: Notification -> Handler Bool
    notify listUpdate = liftIO $ do
      clearFileServerLocations
      updateFileServerLocations (fileServerRecords ((notification listUpdate)))
      return True

    -- Allows other file servers to insert duplicated copies of files
    duplicate :: DBFile -> Handler Bool
    duplicate dbFile = liftIO $ do
      overwriteFile dbFile { duplicate = True, registered = False}
      return True

    -- Allows the transaction server to update a shadow file
    writeShadow :: WriteShadowReq -> Handler Bool
    writeShadow wsr@(WriteShadowReq token name encFile v actID transActID) = liftIO $ do
      errorLog $ "before: " ++ (show wsr)
      let decFile = extractShadowFile wsr
      errorLog $ "after: " ++ decFile

      let sf = ShadowFile actID transActID name decFile v "BUILDING"
      withMongoDbConnection $ upsert (select ["shadowAID" =: actID] "shadowFiles") $ toBSON sf
      return True

    -- Allows the transaction server to instruct the current server to discard a
    -- shadow file
    abortShadow :: AbortShadowReq -> Handler Bool
    abortShadow asr@(AbortShadowReq id) = liftIO $ do
      withMongoDbConnection $ delete (select ["shadowAID" =: id] "shadowFiles")
      return True

    -- Allows the transaction server to instruct the current server to write the shadow
    -- file into the file system
    commitShadow :: CommitShadowReq -> Handler Bool
    commitShadow  csr@(CommitShadowReq id) = liftIO $ do
      (ShadowFile aid tid name value v _) <- getShadowFile id
      errorLog $ "found file: " ++ name
      duplicatedLocations <- getDuplicatedLocations name
      let updatedFile = DBFile name v value False True duplicatedLocations True
      overwriteFile updatedFile
      deleteShadow id
      return True

    -- Allows the directory server update the current server on the presence all of the other 
    -- servers in the system
    discovery :: FileSystemServerRecord -> Handler Bool
    discovery record = liftIO $ do
      let name = serverName record
      withMongoDbConnection $ upsert (select ["serverName" =: name] "systemServerRecords") $ toBSON record
      return True
 

deleteShadow :: String -> IO ()
deleteShadow id = withMongoDbConnection $ delete (select ["shadowAID" =: id] "shadowFiles")

-- Returns a list of all of the other servers to which a given file has 
-- been replicated
getDuplicatedLocations :: String -> IO [FileServerRecord]
getDuplicatedLocations name = do
  file <- getFile name
  case file of
    Nothing -> return []
    Just f -> return $ duplicated f

-- Uses the token to decrypt the write shadow request
extractShadowFile :: WriteShadowReq -> String
extractShadowFile wsr@(WriteShadowReq token _ encData _ _ _) = decryptString encData (recKey1Seed (getShadowToken wsr))

getShadowToken :: WriteShadowReq -> ReceiverToken
getShadowToken (WriteShadowReq token _ _ _ _ _) = read (decryptString token key2Seed) :: ReceiverToken

-- Returns a shadow file based on action ID
getShadowFile :: String -> IO ShadowFile
getShadowFile id = do
  files <- withMongoDbConnection $ do
    docs <- find (select ["shadowAID" =: id] "shadowFiles") >>= drainCursor
    return $ catMaybes $ DL.map (\ b -> fromBSON b :: Maybe ShadowFile) docs
  return $ head files

-- Clears the list of other server location in the system
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
overwriteFile file@(DBFile name _ _ _ _ _ _) = do
  withMongoDbConnection $ upsert (select ["fileName" =: name] "files") $ toBSON file
  
-- return all files matching "fileName"
searchFiles :: String -> IO [DBFile]
searchFiles name = do
  files <- withMongoDbConnection $ do
    docs <- find (select ["fileName" =: name] "files") >>= drainCursor
    return $ catMaybes $ DL.map (\ b -> fromBSON b :: Maybe DBFile) docs
  return files

-- For cases where there may or may not be a file present
getFile :: String -> IO (Maybe DBFile)
getFile name = do
  files <- withMongoDbConnection $ do
    docs <- find (select ["fileName" =: name] "files") >>= drainCursor
    return $ catMaybes $ DL.map (\ b -> fromBSON b :: Maybe DBFile) docs

  case (length files) of
    0 -> return Nothing
    _ -> return $ Just $ head files

getIncrementedFileVersion :: DBFile -> String 
getIncrementedFileVersion (DBFile _ version _ _ _ _ _) = show ((read version :: Int) + 1)

extractFileData :: WriteFileReq -> String
extractFileData wfr@(WriteFileReq token name encData) = decryptString encData (recKey1Seed (getReceiverToken wfr))
  
getReceiverToken :: WriteFileReq -> ReceiverToken
getReceiverToken (WriteFileReq token _ _) = read (decryptString token key2Seed) :: ReceiverToken

getSeedFromToken :: ReadFileReq -> String
getSeedFromToken (ReadFileReq token _) = recKey1Seed (read (decryptString token key2Seed) :: ReceiverToken)

-- Returns all of the Files in the DB
allDBFiles :: IO [DBFile]
allDBFiles = do
  files <- withMongoDbConnection $ do
    docs <- find (select [] "files") >>= drainCursor
    return $ catMaybes $ DL.map (\ b -> fromBSON b :: Maybe DBFile) docs
  return files
  
-- Returns list of all of the file servers in operation in the system
allFileServers :: IO [FileServerRecord]
allFileServers = do
  servers <- withMongoDbConnection $ do
    docs <- find (select [] "fileServerLocations") >>= drainCursor
    return $ catMaybes $ DL.map (\ b -> fromBSON b :: Maybe FileServerRecord) docs
  return servers

-- Returns a list of file servers in the system sorted by least load
leastLoadServers :: IO [FileServerRecord]
leastLoadServers = do
  servers <- allFileServers
  return $ sortServerLoad servers

--TODO implement this 
sortServerLoad :: [FileServerRecord] -> [FileServerRecord]
sortServerLoad servers = servers
sortServerLoad [] = []

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
mongoDbDatabase = defEnv "MONGODB_DATABASE" id "FILESYSTEMDB" True

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




