
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
import           FileSystemFileServerAPI hiding (Lib, API, fileVersion)
import           Network.HTTP.Simple hiding (Proxy)

startApp :: IO ()    -- set up wai logger for service to output apache style logging for rest calls
startApp = withLogging $ \ aplogger -> do
  warnLog "Starting Directory Server."

  --NOTE task scheduler frequency increased to cater for performing cache retrievals
  --don't want to do this as part of resolution request as it would slow down response on that
  --request
  forkIO $ taskScheduler 15

  let settings = setPort 8084 $ setLogger aplogger defaultSettings
  runSettings settings app

taskScheduler :: Int -> IO ()
taskScheduler delay = do
  warnLog $ "Task scheduler operating."

  records <- allCacheRecords
  updateCacheEntries records
  noticeLog $ "all cache entries updated"

  servers <- allFileServerRecords
  noticeLog $ "THE LIST IS: " ++ (show servers)
  --notifyFileServers servers (FileServerNotification servers)
  notifyFileServers servers servers
  noticeLog $ "servers notified of their brethren"

  -- TODO00000000d0dododoODODODOD request load stats
  threadDelay $ delay * 1000000
  taskScheduler delay -- tail recursion


-- Task Scheduler Tasks --

-- Notifying File Servers --
-- Use to tell each file server where the others are
notifyFileServers :: [FileServerRecord] -> [FileServerRecord] -> IO ()
notifyFileServers (serverRecord@(FileServerRecord h p load size):serverRecords) list = do
  let filterFunc = \r -> not ((fsHost r) == h && (fsPort r) == p)   -- no need to inform a server about itself!
  noticeLog $ "to " ++ h ++ ":" ++ p ++ " I send " ++ (show (filter (filterFunc) list))
  performNotification (Notification (FileServerNotification (filter (filterFunc) list))) serverRecord
  notifyFileServers serverRecords list

notifyFileServers [] _ = return ()

performNotification :: Notification -> FileServerRecord -> IO Bool
performNotification req record = do
  let str = "POST http://" ++ (fsHost record) ++  ":" ++ (fsPort record) ++ "/notify"
  let initReq = parseRequest_ str
  let request = setRequestBodyJSON req $ initReq
  noticeLog $ "before call " ++ str
  response <- httpJSON request
  noticeLog $ "after call"
  let ret = (getResponseBody response :: Bool)
  return ret


-- The cache filling logic...
-- cacheFilled = true   && cacheDirty = true    -- fetch until have version on record - set T | F
-- cacheFilled = true   && cacheDirty = false   -- do nothing
-- cacheFilled = false  && cacheDirty = true    -- fetch until have version on record - set T | F
-- cacheFilled = false  && cacheDirty = false   -- grab file ignore version - set T | F
updateCacheEntries :: [CacheRecord] -> IO ()
updateCacheEntries (cacheRecord@(CacheRecord name version _ filled dirty _) : records) = do
  noticeLog $ "all cache entries updated"
  if dirty
  then do
    resp <- getFile $ cacheName cacheRecord
    when (version == (currentFileVersion resp)) $ updateCache name (decryptString (encryptedResult resp) key3Seed)
    noticeLog $ "Filled cache for " ++ name
  else do
    when (not filled) $ do
      resp <- getFile $ cacheName cacheRecord
      updateCache name (decryptString (encryptedResult resp) key3Seed)
      noticeLog $ "Filled cache for " ++ name

  updateCacheEntries records

updateCacheEntries [] = return ()

-- performs "read file" request on a file server to get file contents into cache
getFile :: String -> IO ReadFileResp
getFile name = do
  serverRecord <- locateBestCopyFile name
  let readRequest = ReadFileReq genToken name
  resp <- performReadRequest readRequest serverRecord
  return resp
  
-- Returns the server record for the file server containing the file with the least load
locateBestCopyFile :: String -> IO FileServerRecord
locateBestCopyFile name = do
  allServerRecords <- allFileLocations name
  allServerRecords' <- updateLoadInfo allServerRecords []
  return $ filterSmallestSize allServerRecords' (head allServerRecords')

-- updates the load information to most up-to-date for a list of server records
updateLoadInfo :: [FileServerRecord] -> [FileServerRecord] -> IO [FileServerRecord]
updateLoadInfo (record@(FileServerRecord h p load size):records) updated = do
  currentRec <- getFileServerRecord h p
  let updatedRec = record { fiveMinLoad = (fiveMinLoad currentRec), currentSize = (currentSize currentRec) }
  updateLoadInfo records (updated ++ [updatedRec])

updateLoadInfo [] updated = return updated

-- generates token for secure communication
genToken :: String
genToken = encryptString (show (ReceiverToken key3Seed "DIR_SERVER")) key2Seed

performReadRequest :: ReadFileReq -> FileServerRecord -> IO ReadFileResp
performReadRequest req record = do
  let str = "POST http://" ++ (fsHost record) ++  ":" ++ (fsPort record) ++ "/readFromFile"
  let initReq = parseRequest_ str
  let request = setRequestBodyJSON req $ initReq
  noticeLog $ "before call " ++ str
  response <- httpJSON request
  noticeLog $ "after call"
  let ret = (getResponseBody response :: ReadFileResp)
  return ret
 
updateCache :: String -> String -> IO ()
updateCache name newData = do
	currentCache <- getCacheRecord name
	let updatedCache = currentCache {cacheData = newData, cacheFilled = True, cacheDirty = False}
	withMongoDbConnection $ upsert (select ["cacheName" =: name] "cacheRecords") $ toBSON updatedCache
	return ()
  
-- Main Application -- 

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
    --      -> set dirty flag for that cache record
    --    -> the file doesn't exist
    --      -> return a FileRecord for a file server based on the recent load on the servers
    --      -> create empty cache entry for that file
    --
    -- Note - adding things to the cache simply adds an empty 
    resolveFile :: ResolutionRequest -> Handler ResolutionResponse
    resolveFile rr@(ResolutionRequest name intention token) = liftIO $ do
      noticeLog $ "Resolving " ++ name
      -- search for the primary record for the current file
      primaryRecord <- getPrimaryRecord name

      -- what does the client intend to do to this file?
      -- TODO only get primary record for writes, any record for reads
      case intention of
        "READ"    -> do
          
          case primaryRecord of
            Just record   ->  do

              cacheHit <- cachePromote record       --if cache exists, increase its age value, return cache record.
              noticeLog $ "reading file"
              noticeLog $ show rr

              case cacheHit of
                Just cached   -> do

                  let rToken = read (decryptString token key2Seed) :: ReceiverToken 
                  let encFile = encryptString (cacheData cached) (recKey1Seed rToken)
                  return $ ResolutionResponse True record True encFile

                Nothing       -> return $ ResolutionResponse True record False ""

            Nothing       -> do             --this is an ERROR state
              errorLog "This joker is trying to read a file that doesn't exist"
              return $ negativeResolutionResponse

        "WRITE"   -> do
          
          case primaryRecord of
            Just record   -> do
              incrementFileVersion record
              dirtyCache record
              -- TODO delete secondary file records saved
              return $ ResolutionResponse True record False "" 

            Nothing       -> do             -- add new primary record to fileRecords
              bestFS <- selectAppropriateFileServer
              let newRecord = FileRecord "PRIMARY" name "1"  bestFS
              addRecord newRecord
              dirtyCache newRecord     
              return $ ResolutionResponse True newRecord False ""

        _         -> do                     --this is an ERROR state
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
      let updatedRecord = record { cacheAge = (incrementAge (cacheAge record)) } 
      withMongoDbConnection $ upsert (select ["cacheName" =: name] "cacheRecords") $ toBSON updatedRecord

      if cacheFilled record && not (cacheDirty record)
      then return $ Just record
      else return Nothing

    0 -> do 
      maxAge <- getNewCacheAge
      let record = CacheRecord name version  "" False False maxAge 
      withMongoDbConnection $ upsert (select ["cacheName" =: name] "cacheRecords") $ toBSON record
      return Nothing


dirtyCache :: FileRecord -> IO ()
dirtyCache fr@(FileRecord _ name _ _) = do
  maxAge <- getNewCacheAge
  newVersion <- getVersion fr
  let record = CacheRecord name newVersion  "" False True maxAge 
  withMongoDbConnection $ upsert (select ["cacheName" =: name] "cacheRecords") $ toBSON record
  
    
-- Returns most up-to-date version on record for a file
getVersion :: FileRecord -> IO String
getVersion (FileRecord _ name _ _) = do
  possibleRecord <- getPrimaryRecord name

  case possibleRecord of
    Just record -> return $ fileVersion record
    Nothing -> return "0"
    
-- increments current version of file on record
incrementFileVersion :: FileRecord -> IO ()
incrementFileVersion record@(FileRecord _ name version _) = do
  let updatedRecord = record { fileVersion =  (show ((read version :: Int) + 1)) }
  withMongoDbConnection $ upsert (select ["fileRecordName" =: name] "fileRecords") $ toBSON updatedRecord

-- Calculates an age value for a new cache entry. This new age will be (max(cacheRecords.age) + 1)
getNewCacheAge :: IO String 
getNewCacheAge = do
  cacheRecords <- withMongoDbConnection $ do
    docs <- find (select [] "cacheRecords") >>= drainCursor
    return $ catMaybes $ DL.map (\ b -> fromBSON b :: Maybe CacheRecord) docs

  let topAge = getTopCache cacheRecords "0"
  return topAge

--returns a string value representing (max(cacheRecords.age) + 1)
getTopCache :: [CacheRecord] -> String -> String
getTopCache (cr:crs) max =
	if ( read (cacheAge cr) :: Int) > ( read max :: Int)
	then getTopCache crs (cacheAge cr)
	else getTopCache crs max

getTopCache [] max = show ((read max :: Int) + 1) 

incrementAge :: String -> String
incrementAge w = show ((read w :: Int) + 1)

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
  
--attempts to return the primary record for a file
getPrimaryRecord :: String -> IO (Maybe FileRecord)
getPrimaryRecord name = do
  files <- withMongoDbConnection $ do
    let primaryIdentifier = "PRIMARY" :: String       --TODO MOVE THIS TO API
    docs <- find (select ["fileRecordName" =: name, "recordType" =: primaryIdentifier] "fileRecords") >>= drainCursor
    return $ catMaybes $ DL.map (\ b -> fromBSON b :: Maybe FileRecord) docs

  case (length files) of
    0 -> return Nothing 
    _ -> return (Just (head files))

getCacheRecord :: String -> IO CacheRecord
getCacheRecord name = do
	cacheRecords <- withMongoDbConnection $ do
		docs <- find (select ["cacheName" =: name] "cacheRecords") >>= drainCursor
		return $ catMaybes $ DL.map (\ b -> fromBSON b :: Maybe CacheRecord) docs

	return $ head cacheRecords 	--will always be one


-- Returns current cache listing
allCacheRecords :: IO [CacheRecord]
allCacheRecords = do
  cacheRecords <- withMongoDbConnection $ do
    docs <- find (select [] "cacheRecords") >>= drainCursor
    return $ catMaybes $ DL.map (\ b -> fromBSON b :: Maybe CacheRecord) docs
  return cacheRecords
  
-- Returns server record for all servers this file currently resides
allFileLocations :: String -> IO [FileServerRecord]
allFileLocations name = do
  files <- withMongoDbConnection $ do
    docs <- find (select ["fileRecordName" =: name] "fileRecords") >>= drainCursor
    return $ catMaybes $ DL.map (\ b -> fromBSON b :: Maybe FileRecord) docs

  return $ map (serverRecord) files
  

-- TODO add error checking
getFileServerRecord :: String -> String -> IO FileServerRecord
getFileServerRecord h p = do
  fileServers <- withMongoDbConnection $ do
    docs <- find (select ["fsHost" =: h, "fsPort" =: p] "fileServers") >>= drainCursor
    return $ catMaybes $ DL.map (\ b -> fromBSON b :: Maybe FileServerRecord) docs

  return $ head fileServers       -- this will be one always

allFileServerRecords :: IO [FileServerRecord]
allFileServerRecords = do
  fileServers <- withMongoDbConnection $ do
    docs <- find (select [] "fileServers") >>= drainCursor
    return $ catMaybes $ DL.map (\ b -> fromBSON b :: Maybe FileServerRecord) docs

  return fileServers



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




