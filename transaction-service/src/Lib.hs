
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
import           Servant
import qualified Servant.API                  as SC
import qualified Servant.Client               as SC
import           System.Environment           (getArgs, getProgName, lookupEnv)
import           System.Log.Formatter
import           System.Log.Handler           (setFormatter)
import           System.Log.Handler.Simple
import           System.Log.Handler.Syslog
import           System.Log.Logger
import           FileSystemTransactionServerAPI
import           FileSystemDirectoryServerAPI hiding (API)
import           FileSystemFileServerAPI hiding (API, fileVersion)
import           FileSystemAuthServerAPI hiding (API)
import           FileSystemLockServerAPI hiding (API)
import           Network.HTTP.Simple hiding (Proxy)

startApp :: IO ()    -- set up wai logger for service to output apache style logging for rest calls
startApp = withLogging $ \ aplogger -> do
  warnLog "Starting transaction service."

  forkIO $ taskScheduler 5

  let settings = setPort 8081 $ setLogger aplogger defaultSettings
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
server =  initTransaction
          :<|> commitTransaction
          :<|> abortTransaction
          :<|> action
          :<|> discovery

    where
      -- Initialises a new transaction to write actions against
      -- See "transactions" database
      initTransaction :: InitTransReq -> Handler InitTransResp
      initTransaction req = liftIO $ do
        newID <- genNewTransaction
        case newID of 
          Just id -> return $ InitTransResp id True
          Nothing -> do
            errorLog $ "Init transaction failed"
            return $ InitTransResp "" False
      
      -- Commits a specified transaction. This will involve pushing
      -- all shadow files on the various file servers corresponding to
      -- the current transaction ID into the main system. From here
      -- they will be replicated onto other file servers. This will 
      -- also perform a standard file resolution request with the 
      -- directory server to ensure that the file record is correct
      -- and is cached appropriately
      commitTransaction :: CommitReq -> Handler CommitResp
      commitTransaction req = liftIO $ do
        let transID = commitReqTransID req
        success <- performCommitTransaction transID
        if success
        then return $ CommitResp transID True
        else do 
          errorLog $ "Failed to commit transaction " ++ transID
          return $ CommitResp transID False

      -- Aborts a given transaction. Informs file servers
      -- to discard relevant shadow files
      abortTransaction :: AbortReq -> Handler AbortResp
      abortTransaction req = liftIO $ do
        let id = abortReqTransID req
        success <- performAbortTransaction id
        if success
        then return $ AbortResp id True
        else do 
          errorLog $ "Failed to abort transaction " ++ id
          return $ AbortResp id False

      -- Handles a given action for the specified transaction.
      -- Pushes shadow files to relevant file servers. Uses
      -- a special endpoint on directory server to resolve 
      -- files without causing errors in cache/replication state.
      action :: ActionReq -> Handler ActionResp
      action req = liftIO $ do
        let id = actionReqTransID req
        success <- registerAction req
        if success
        then return $ ActionResp id True 
        else do 
          errorLog $ "Failed to register action " ++ (show req)
          return $ ActionResp id False

      -- Used to inform all servers about each other
      discovery :: FileSystemServerRecord -> Handler Bool
      discovery record = liftIO $ do
        let name = serverName record
        withMongoDbConnection $ upsert (select ["serverName" =: name] "systemServerRecords") $ toBSON record
        return True

-- Informs all relevant file servers to discard the shadow files
-- and updates the current transaction to "ABORTED" status
performAbortTransaction :: String -> IO Bool
performAbortTransaction transID = do
  actions <- getActionList transID
  success <- performAbortAction actions
  oldTrans <- getTransaction transID
  let updatedTrans = oldTrans { transactionStatus = "ABORTED" }
  withMongoDbConnection $ upsert (select ["transactionID" =: transID] "transactions") $ toBSON updatedTrans
  return success

-- Informs all relevant file servers to write shadow files to 
-- the main files and updates the directory server with information
-- about the newly introduced file so that cache and replication can
-- be maintained correctly.
performCommitTransaction :: String -> IO Bool
performCommitTransaction transID = do
  actions <- getActionList transID
  success <- performCommitAction actions
  return success

-- Returns all actions associated with a given transaction
getActionList :: String -> IO [TransAction]
getActionList transID = do
  actions <- withMongoDbConnection $ do
    docs <- find (select ["actionTransID" =: transID] "actions") >>= drainCursor
    return $ catMaybes $ DL.map (\ b -> fromBSON b :: Maybe TransAction) docs

  return actions

-- Informs a file server to discard a shadow file and updates the
-- action to "ABORTED" status
performAbortAction :: [TransAction] -> IO Bool
performAbortAction (action@(TransAction id _ server _ name _ _ _):actions) = do
  performAbortShadow (AbortShadowReq id) server
  let updatedAction = action { actionStatus = "ABORTED" }
  withMongoDbConnection $ upsert (select ["actionID" =: id] "actions") $ toBSON updatedAction
  unLockFile name

  performCommitAction actions

performAbortAction [] = return True

-- Informs a file server to write a shadow file to the main file systems and
-- updates the action to "COMMITTED" status
performCommitAction :: [TransAction] -> IO Bool
performCommitAction (action@(TransAction id _ server _ name _ _ _):actions) = do
  performCommitShadow (CommitShadowReq id) server
  updateDirServer action
  let updatedAction = action { actionStatus = "COMMITTED" }
  withMongoDbConnection $ upsert (select ["actionID" =: id] "actions") $ toBSON updatedAction
  unLockFile name

  performCommitAction actions

performCommitAction [] = return True

-- Generates a token to be used for secure communication with file servers
genToken :: String
genToken = encryptString (show (ReceiverToken key3Seed "TRANSACTION_SERVER")) key2Seed

-- This performs a standard file resolution request. The resolution response
-- is irrelevant but the request is performed in order to update the internal
-- state of the directory server and ensure correct file versions, cache integrity
-- and replication is preserved
updateDirServer :: TransAction -> IO Bool
updateDirServer (TransAction _ _ server typeAction name _ _ _) = do
  let token = genToken
  server  <- getServerRecord "DIR_SERVER"
  performStandardResReq (ResolutionRequest name typeAction token) server
  return True


-- The actual POST call to the abort shadow endpoint on a file server
performAbortShadow :: AbortShadowReq -> FileServerRecord -> IO Bool
performAbortShadow req (FileServerRecord h p _ _) = do
  let str = "POST http://" ++ h ++  ":" ++ p ++ "/abortShadow"
  let initReq = parseRequest_ str
  let request = setRequestBodyJSON req $ initReq
  response <- httpJSON request
  let ret = (getResponseBody response :: Bool)
  return ret


-- The actual POST call to the commit shadow endpoint on a file server
performCommitShadow :: CommitShadowReq -> FileServerRecord -> IO Bool
performCommitShadow req (FileServerRecord h p _ _) = do 
  let str = "POST http://" ++ h ++  ":" ++ p ++ "/commitShadow"
  let initReq = parseRequest_ str
  let request = setRequestBodyJSON req $ initReq
  response <- httpJSON request
  let ret = (getResponseBody response :: Bool)
  return ret

  
-- Initialises a new transaction record in the DB and generates an appropriate
-- transaction ID
genNewTransaction :: IO (Maybe String)
genNewTransaction = do
  newID <- genNewID
  let newTrans = Transaction newID (show Building)
  withMongoDbConnection $ upsert (select ["transactionID" =: newID] "transactions") $ toBSON newTrans
  return $ Just newID

-- Generates appropriate transaction ID
genNewID :: IO String
genNewID = do
  transactions <- allTransactions
  let maxID = getCurrentMaxID transactions transactionID "0"
  return $ show ((read maxID :: Int) + 1)

-- Helper method to get the max ID from list of transactions
getCurrentMaxID :: [a] -> (a -> String) -> String -> String
getCurrentMaxID (t:ts) extractorFunc max
  | (read (extractorFunc t) :: Int) > (read max :: Int) = getCurrentMaxID ts extractorFunc (extractorFunc t)
  | otherwise = getCurrentMaxID ts extractorFunc max

getCurrentMaxID [] _ max = max


-- Registers a new action on behalf of the client. This will update 
-- the DB to include this desired action as part of the transaction, 
-- resolves the file location with the directory server using specialised 
-- endpoint, and creates a new shadow file entry on the appropriate file
-- server
registerAction :: ActionReq -> IO Bool
registerAction req@(ActionReq transID _ name encData token) = do
  case (actionReqType req) of
    "WRITE" -> do
      let decData     = extractFileData req
      let token       =  genToken
      newActionID     <- genNewActionID transID
      dirServer       <- getServerRecord "DIR_SERVER"
      res             <- performTransResReq (ResolutionRequest name "WRITE" token) dirServer
      let rec         = resolution res
      version         <- determineFileVersion name
      let newAction   = TransAction newActionID transID (serverRecord rec) "WRITE" name decData (strInc version) (show Building)
      updateAction newAction
      updateShadowFile newAction
      return True

    _ -> do
      errorLog $ "Unrecognised action type " ++ (actionReqType req)
      return False

-- Returns the file version for a given file currently
-- in the DB. This is useful/interesting because repeated writes
-- to a file as part of a transaction will increment the file version
-- accordingly and this must be maintained.
determineFileVersion :: String -> IO String
determineFileVersion name = do
  action <- getAction name
  case action of
    Just a -> return $ actionFileVersion a 
    Nothing -> return "0"

-- Performs a special file resolution with directory server that DOESN'T update
-- cache/replication state
performTransResReq :: ResolutionRequest -> FileServerRecord -> IO ResolutionResponse
performTransResReq req server = do
  res <- performResolution req server "transResolveFile"
  return res

-- Performs standaard file resolution with directory server that DOES updaate 
-- cache/replication state
performStandardResReq :: ResolutionRequest -> FileServerRecord -> IO ResolutionResponse
performStandardResReq req server = do
  res <- performResolution req server "resolveFile"
  return res

-- The actual resolution POST call
performResolution :: ResolutionRequest -> FileServerRecord -> String -> IO ResolutionResponse
performResolution req (FileServerRecord h p _ _) endpoint = do
  let str = "POST http://" ++ h ++  ":" ++ p ++ "/" ++ endpoint
  let initReq = parseRequest_ str
  let request = setRequestBodyJSON req $ initReq
  response <- httpJSON request
  let ret = (getResponseBody response :: ResolutionResponse)
  return ret

-- Either creates a new action or writes to an existing one
updateAction :: TransAction -> IO ()
updateAction action@(TransAction id _ _ _ name _ _ _) = do
  withMongoDbConnection $ upsert (select ["actionFileName" =: name, "actionID" =: id] "actions") $ toBSON action

-- Updates the shadow file stored on a file server.
-- Does the following to accomplish this task:
--  1) Locks file
--  2) Updates the shadow file for this file name stored on file server
-- TODO add error handling
updateShadowFile :: TransAction -> IO ()
updateShadowFile action@(TransAction actionid actiontransid server _ name fileValue v _) = do
  let token = genToken
  let encValue = encryptString (fileValue) key3Seed
  lockFile $ actionFileName action
  performUpdateShadowFile (WriteShadowReq token name encValue v actionid actiontransid) server
  return ()

-- The actual shadow update POST call
performUpdateShadowFile :: WriteShadowReq -> FileServerRecord -> IO Bool
performUpdateShadowFile req (FileServerRecord h p _ _) = do
  let str = "POST http://" ++ h ++  ":" ++ p ++ "/writeShadow"
  let initReq = parseRequest_ str
  let request = setRequestBodyJSON req $ initReq
  response <- httpJSON request
  let ret = (getResponseBody response :: Bool)
  return ret

-- Retrieves an action from DB based on file name
getAction :: String -> IO (Maybe TransAction)
getAction name = do
  actions <- withMongoDbConnection $ do
    docs <- find (select ["actionFileName" =: name] "actions") >>= drainCursor
    return $ catMaybes $ DL.map (\ b -> fromBSON b :: Maybe TransAction) docs

  case (length actions) of
    0 -> return Nothing
    _ -> return $ Just (head actions)
  
-- Generates appropriate action ID
genNewActionID :: String -> IO String
genNewActionID transID = do
  actions <- allActionsForTrans transID
  let maxID = getCurrentMaxID actions actionID "0"
  return $ show ((read maxID :: Int) + 1)

-- Returns a list of all transactions in DB, regardless of state
allTransactions :: IO [Transaction]
allTransactions = do
  transactions <- withMongoDbConnection $ do
    docs <- find (select [] "transactions") >>= drainCursor
    return $ catMaybes $ DL.map (\ b -> fromBSON b :: Maybe Transaction) docs

  return transactions

-- Returns a list of all actions in DB regardless of state
allActions :: IO [TransAction]
allActions = do
  actions <- withMongoDbConnection $ do
    docs <- find (select [] "actions") >>= drainCursor
    return $ catMaybes $ DL.map (\ b -> fromBSON b :: Maybe TransAction) docs

  return actions

-- Returns all actions associated with a given Transaction
allActionsForTrans :: String -> IO [TransAction]
allActionsForTrans transID = do
  actions <- withMongoDbConnection $ do
    docs <- find (select ["actionTransID" =: transID] "actions") >>= drainCursor
    return $ catMaybes $ DL.map (\ b -> fromBSON b :: Maybe TransAction) docs

  return actions

-- Retrieves transaction from DB based on transaction ID
getTransaction :: String -> IO Transaction
getTransaction id = do
  transactions <- withMongoDbConnection $ do
    docs <- find (select ["transactionID" =: id] "transactions") >>= drainCursor
    return $ catMaybes $ DL.map (\ b -> fromBSON b :: Maybe Transaction) docs

  return $ head transactions

-- Extracts encrypted file data from an action request
extractFileData :: ActionReq -> String
extractFileData ar@(ActionReq _ _ _ encData token) = decryptString encData (recKey1Seed (getReceiverToken ar))

-- Extract a receviver token from an action request
getReceiverToken :: ActionReq -> ReceiverToken
getReceiverToken (ActionReq _ _ _ _ token) = read (decryptString token key2Seed) :: ReceiverToken

-- Locks a file
lockFile :: String -> IO Bool
lockFile name = do
  lockServer <- getServerRecord "LOCK_SERVER"
  performLockFile (LockFileReq name) lockServer
  return True

-- Actual POST call for the lock
performLockFile :: LockFileReq -> FileServerRecord -> IO Bool
performLockFile req (FileServerRecord h p _ _) = do
  let str = "POST http://" ++ h ++  ":" ++ p ++ "/lockFile"
  let initReq = parseRequest_ str
  let request = setRequestBodyJSON req $ initReq
  response <- httpJSON request
  let ret = (getResponseBody response :: Bool)
  return ret

-- Unlocks a file
unLockFile :: String -> IO Bool
unLockFile name = do
  lockServer <- getServerRecord "LOCK_SERVER"
  performUnlockFile (UnlockFileReq name) lockServer

-- Actual POST call for the unclock
performUnlockFile :: UnlockFileReq -> FileServerRecord -> IO Bool
performUnlockFile req (FileServerRecord h p _ _) = do
  let str = "POST http://" ++ h ++  ":" ++ p ++ "/unlockFile"
  let initReq = parseRequest_ str
  let request = setRequestBodyJSON req $ initReq
  response <- httpJSON request
  let ret = (getResponseBody response :: Bool)
  return ret

-- Returns a server record based on it's name. This record can
-- point to one of the other main servers (Directory, Locking Server, etc.)
getServerRecord :: String -> IO FileServerRecord
getServerRecord name = do
  records <- withMongoDbConnection $ do
    docs <- find (select ["serverName" =: name] "systemServerRecords") >>= drainCursor
    return $ catMaybes $ DL.map (\ b -> fromBSON b :: Maybe FileSystemServerRecord) docs

  return $ ( serverLocation (head records))

-- Due to issues storing Int values, this is necessary
strInc :: String -> String
strInc v = show ((read v :: Int) + 1)

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




