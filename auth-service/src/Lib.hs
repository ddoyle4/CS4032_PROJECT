{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}



module Lib
    ( startApp
    ) where

import Data.Aeson
import Data.Aeson.TH
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Data.Aeson.TH
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Trans.Either
import Control.Monad.Trans.Resource
import Data.Text                    (pack, unpack)
import Data.Aeson
import Data.Aeson.Types
import Data.Attoparsec.ByteString
import Data.ByteString (ByteString)
import Data.Int
import Data.List
import Data.String.Conversions
import Data.Time.Calendar
import GHC.Generics
import Lucid
import Network.HTTP.Media ((//), (/:))
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import System.Directory
import Text.Blaze
import Text.Blaze.Html.Renderer.Utf8
import qualified Data.Aeson.Parser
import qualified Text.Blaze.Html
import Database.MongoDB
import Data.Bson.Generic
import Control.Monad                (when)
import Control.Monad.IO.Class
import Control.Monad.Trans.Except   (ExceptT)
import Control.Monad.Trans.Resource
import Data.Aeson
import Data.Aeson.TH
import Data.Bson.Generic
import qualified Data.ByteString.Lazy         as L
import qualified Data.List                    as DL
import Data.Maybe                   (catMaybes)
import Data.Text                    (pack, unpack)
import Data.Time.Clock              (UTCTime, getCurrentTime)
import Data.Time.Format             (defaultTimeLocale, formatTime)
import Database.MongoDB
import GHC.Generics
import Network.HTTP.Client          (defaultManagerSettings, newManager)
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Logger
import Servant
import qualified Servant.API                  as SC
import qualified Servant.Client               as SC
import System.Environment           (getArgs, getProgName, lookupEnv)
import System.Log.Formatter
import System.Log.Handler           (setFormatter)
import System.Log.Handler.Simple
import System.Log.Handler.Syslog
import System.Log.Logger

type API = "position" :> Capture "x" Int :> Capture "y" Int :> Get '[JSON] Position
      :<|> "hello" :> QueryParam "name" String :> Get '[JSON] HelloMessage
      :<|> "marketing" :> ReqBody '[JSON] ClientInfo :> Post '[JSON] Email
      :<|> "save" :> Capture "name" String :> Capture "age" Int :> Get '[JSON] Person

data Person = Person
  { name :: String
  , age :: Int
  } deriving Generic

instance ToJSON Person
instance FromJSON Person
instance ToBSON Person

data Position = Position
  { x :: Int
  , y :: Int
  } deriving Generic

instance ToJSON Position

newtype HelloMessage = HelloMessage { msg :: String }
  deriving Generic

instance ToJSON HelloMessage

data ClientInfo = ClientInfo
  { clientName :: String
  , clientEmail :: String
  , clientAge :: Int
  , clientInterestedIn :: [String]
  } deriving Generic

instance FromJSON ClientInfo
instance ToJSON ClientInfo

data Email = Email
  { from :: String
  , to :: String
  , subject :: String
  , body :: String
  } deriving Generic

instance ToJSON Email

emailForClient :: ClientInfo -> Email
emailForClient c = Email from' to' subject' body'

  where from'    = "great@company.com"
        to'      = clientEmail c
        subject' = "Hey " ++ clientName c ++ ", we miss you!"
        body'    = "Hi " ++ clientName c ++ ",\n\n"
                ++ "Since you've recently turned " ++ show (clientAge c)
                ++ ", have you checked out our latest "
                ++ intercalate ", " (clientInterestedIn c)
                ++ " products? Give us a visit!"


server3 :: Server API
server3 = position
     :<|> hello
     :<|> marketing
     :<|> save
  where position :: Int -> Int -> Handler Position
        position x y = return (Position x y)

        hello :: Maybe String -> Handler HelloMessage
        hello mname = return . HelloMessage $ case mname of
          Nothing -> "Hello, anonymous coward"
          Just n  -> "Hello, " ++ n

        marketing :: ClientInfo -> Handler Email
        marketing clientinfo = return (emailForClient clientinfo)

        save :: String -> Int -> Handler Person
        save name age = liftIO $ do
          let newEntry = Person name age
          -- upsert creates a new record if the identified record does not exist, or if
          -- it does exist, it updates the record with the passed document content
          -- As you can see, this takes a single line of code
          withMongoDbConnection $ upsert (select ["name" =: name] "saved_persons") $ toBSON newEntry
          return (Person name age)




userAPI :: Proxy API
userAPI = Proxy

-- 'serve' comes from servant and hands you a WAI Application,
-- which you can think of as an "abstract" web application,
-- not yet a webserver.
app1 :: Application
app1 = serve userAPI server3

startApp :: IO ()
startApp = run 8081 app1


-- | Mongodb helpers...

-- | helper to open connection to mongo database and run action
-- generally run as follows:
--        withMongoDbConnection $ do ...
withMongoDbConnection :: Action IO a -> IO a
withMongoDbConnection act  = do
  ip <- mongoDbIp
  port <- mongoDbPort
  database <- mongoDbDatabase
  pipe <- connect (host ip)
  ret <- runResourceT $ liftIO $ access pipe master (pack database) act
  close pipe
  return ret

-- | helper method to ensure we force extraction of all results
-- note how it is defined recursively - meaning that draincursor' calls itself.
-- the purpose is to iterate through all documents returned if the connection is
-- returning the documents in batch mode, meaning in batches of retruned results with more
-- to come on each call. The function recurses until there are no results left, building an
-- array of returned [Document]
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

-- | Helper function to simplify the setting of environment variables
-- function that looks up environment variable and returns the result of running funtion fn over it
-- or if the environment variable does not exist, returns the value def. The function will optionally log a
-- warning based on Boolean tag
defEnv :: Show a
              => String        -- Environment Variable name
              -> (String -> a)  -- function to process variable string (set as 'id' if not needed)
              -> a             -- default value to use if environment variable is not set
              -> Bool          -- True if we should warn if environment variable is not set
              -> IO a
defEnv env fn def doWarn = lookupEnv env >>= \ e -> case e of
      Just s  -> return $ fn s
      Nothing -> return def


























