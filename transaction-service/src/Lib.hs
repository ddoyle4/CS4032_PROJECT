
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
import           FileSystemTransactionServerAPI

startApp :: IO ()    -- set up wai logger for service to output apache style logging for rest calls
startApp = withLogging $ \ aplogger -> do
  warnLog "Starting lock service."

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

    where
      initTransaction :: InitTransReq -> Handler InitTransResp
      initTransaction req = liftIO $ do
        newID <- genNewTransaction
        case newID of 
          Just id -> return $ InitTransResp id True
          Nothing -> do
            errorLog $ "Init transaction failed"
            return $ InitTransResp "" False

        commitTransaction :: CommitReq -> Handler CommitResp
        commitTransaction req = liftIO $ do
          let id = commitReqTransID req
          success <- performCommitTransaction id
          if success
          then return $ CommitResp id True
          else do 
            errorLog $ "Failed to commit transaction " ++ id
            return $ CommitResp id False

        abortTransaction :: AbortReq -> Handler AbortResp
        abortTransaction req = liftIO $ do
          let id = abortReqTransID req
          success <- performAbortTransaction id
          if success
          then return $ AbortResp id True
          else do 
            errorLog $ "Failed to abort transaction " ++ id
            return $ AbortResp id False


        action :: ActionReq -> Handler ActionResp
        action req = liftIO $ do
          let id = actionReqTransID req
          success <- registerAction req
          if success
          then return $ ActionResp id True 
          else do 
            errorLog $ "Failed to register action " ++ (show req)
            return $ ActionResp id False


genNewTransaction :: IO String
genNewTransaction = do
  newID <- genNewID
  let newTrans = Transaction newID (show Building)
  withMongoDbConnection $ upsert (select ["transactionID" =: newID] transactionDBName) $ toBSON newTrans
  return newID

genNewID :: IO String
genNewID = do
  transactions <- allTransactions
  maxID <- getCurrentMaxID transactions "0"
  return $ show ((read maxID :: Int) + 1)

getCurrentMaxID :: [Transaction] -> String -> String
getCurrentMaxID (t:ts) max
  | (read (transactionID t) :: Int) > (read max :: Int) = getCurrentMaxID ts (transactionID t)
  | otherwise = getCurrentMaxID ts max

getCurrentMaxID [] max = max
  


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




