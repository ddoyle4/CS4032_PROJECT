
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
import           FileSystemAuthServerAPI

startApp :: IO ()    -- set up wai logger for service to output apache style logging for rest calls
startApp = withLogging $ \ aplogger -> do
  warnLog "Starting use-haskell."

  forkIO $ taskScheduler 5

  let settings = setPort 8080 $ setLogger aplogger defaultSettings
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
server = loadEnvironmentVariable
    :<|> getREADME
    :<|> storeMessage
    :<|> searchMessage
    :<|> performRESTCall
    :<|> debugSaveUser

  where
    -- | where is just a way of ensuring that the following functions are scoped to the server function. Each function
    -- below hasa type matching the end point parameters, with the return type being of type `Handler a`, where a is the
    -- type of data returned by the endpoint. Handler is a Monad. This may strike terror - it should not, but th great
    -- news is that we don't have to understand Monads at all really, providing we follow some simple programming
    -- conventions when writing handlers.
    --
    -- Now we will explain each handler in turn, along with any helper functions we write to help us implement the
    -- various endpoints.
    loadEnvironmentVariable :: Maybe String -> Handler ResponseData
    loadEnvironmentVariable ms = liftIO $ do
      warnLog $ "request to load environment variable: " ++ show ms
      -- Something that is of type Maybe String will either have a value Nothing
      -- or a value Just s, where s is a String. The following case statement allows us to
      -- distinguish between the two possibilities and act accordingly. If we get nothing in this case,
      -- we throw an exception. Otherwise, we look for the environment variable, throwing an exception
      -- if it is not set (probably not sensible program behaviour but this is just a demonstration).
      --
      -- Note that this function has deliberately been written in an empirical style that would be familiar to
      -- Java of C or C++ programmers, essentially as a kind of long drawn out if/else structure (although in fact
      -- written with a case statement structure). See the searchMessage implementation below for a more elegant and
      -- therefore less complex implementation, that takes some advantage of Haskell coding style.
      case ms of
        Nothing -> do
          warnLog "No environment variable requested"
          return $ ResponseData "WAT? No environment variable requested."
        Just s  -> liftIO $ do
          e <- lookupEnv s -- lookupEnv is an IO function, so we must use the `<-` notation
          case e of
            -- If the environment variable is not set, then create a lof entry and return and exception
            Nothing -> do
              warnLog $ "Environment variable " ++ s ++ " is not set."
              return $ ResponseData $  "Environment variable " ++ s ++ " is not set."

            -- Otherwise, return the envrionment variable. Note how variable names can use ', eg. x', x''
            -- Haskell programmers often use this format for variables that are essentially referring to the same thing
            Just e' -> return $ ResponseData e'

    -- |  One can do File IO with the standard functions:
    --                 readFile  :: FilePath -> IO String
    --                 writeFile :: FilePath -> String -> IO ()
    -- we shall set the location of the README file to return using a command line argument
    -- So our first task is to get the README file location, then open it and return its contents to the client.


    -- the getREAME function below is equialent to the following
    -- pseudo-java euivalent type:
    --   ResponseData getREADME ( ){
    --         // code goes here
    --   }

    getREADME, getREADME' :: Handler ResponseData -- fns with no input, second getREADME' is for demo below
    getREADME = liftIO $ do
      [rPath] <- getArgs         -- alternatively (rPath:xs) <- getArgs
      s       <- readFile rPath
      return $ ResponseData s

    -- here is an alternative implementation of getREADME, more in keeping the Haskell style
    -- takes a bit of practice, but very easy to read, if yuo understand the symbols, and very hard to get wrong.
    -- in english, read file idenfitied by the head of the argument list and return as a ResponseData structure
    getREADME' = liftIO $ ResponseData <$> (readFile . head =<< getArgs)

    -- Here, as a comparison or relative code complexity,
    -- is an example of how to perform the equivalent of the
    -- Haskell readFile method in Java, but remember that this
    -- implementation would break for very large files. You will achieve nothing like the getREADME' implementation
    -- in Java without great skill, if at all.
    --
    -- static String readFile(String path, Charset encoding)
    --    throws IOException
    -- {
    --   byte[] encoded = Files.readAllBytes(Paths.get(path));
    --   return new String(encoded, encoding);
    -- }

    -- | Next we will do Database manipulation and search. We shall use MongoDb as the database. This gives us the
    -- advantage of not having to do any database configuration - we can start writing data into the data store
    -- immediately. Note how we extract the parameter elements directly in the method definition via pattern
    -- matching. nice. Ps. '_' means we don't care what value is.
    storeMessage :: Message -> Handler Bool
    storeMessage msg@(Message key _) = liftIO $ do
      warnLog $ "Storing message under key " ++ key ++ "."

      -- upsert creates a new record if the identified record does not exist, or if
      -- it does exist, it updates the record with the passed document content
      -- As you can see, this takes a single line of code
      withMongoDbConnection $ upsert (select ["name" =: key] "MESSAGE_RECORD") $ toBSON msg

      return True  -- as this is a simple demo I'm not checking anything

    searchMessage :: Maybe String -> Handler [Message]
    searchMessage (Just key) = liftIO $ do
      warnLog $ "Searching for value for key: " ++ key

      -- Find the relevant documents from the DB, take the returned data records and convert the relevant data to
      -- ResponseData records, eliminating any failures to convert, compacting to a returned simple array
      -- done in fewer lines than it takes to describe (and with fully idomatic Haskell, perhaps a single line of code).
      --
      -- 'map' take a function that processes each element in a list, and a list, and returns a list that results from
      -- processing each element of a list. The function passed calls fromBSON, that takes the Document element from the
      -- list and generates the ResponseData data structure if possible, return 'Just s', or on failure returns Nothing.
      --
      -- catMaybes is a standard filtering library function that can take a set of Maybe type (Nothing or Just something)
      -- and return an array of the contained elements. There is no magic to catMaybes - its entire implementation is as
      -- follows:
      --            catMaybes :: [Maybe a] -> [a]
      --            catMaybes ls = [x | Just x <- ls]

      withMongoDbConnection $ do
        docs <- find (select ["name" =: key] "MESSAGE_RECORD") >>= drainCursor
        --warnLog $ "retrieved data: " ++ show docs
        return $ catMaybes $ DL.map (\ b -> fromBSON b :: Maybe Message) docs

      -- notice the structure of the first line of code: fn1 >>= fn2
      -- An alternative way to write this is:
      --        a <- fn1
      --        b <- fn2 a
      -- but for short IO function chains, it can be easier to use >>= to chain them.
      --
      -- In fact, the code above can be compressed further, although it is a question of style as to which is
      -- preferable:
      --
      --     withMongoDbConnection $ find (select ["name" =: key] "MESSAGE_RECORD") >>= drainCursor >>=
      --                                return . catMaybes . DL.map (\ b -> fromBSON b :: Maybe ResponseData)
      --
      -- The effect of the '.' is to chain a set of functions A . B . C into a single function X, so that when one calls
      -- X p, C is called on the parameter p first, followed by B on the result, followed by A of the result of
      -- that. This is exactly equivalent in effect to a function \p ->(A (B (C p))).

    -- This second version of searchMessage will only be called if the parameter passed has the Value 'Nothing'
    -- in other words, we are stripping away the case where we are passed nonsense from the primary implementation
    -- above, which can now be focussed on just handing the normal case where we get a valid input. This simplifies the
    -- code, making it far easier to maintain. Also, in cases where the input can be more complex than just a Maybe
    -- type, it allows you as a programming style to separate out the complexity of working out what you have been
    -- given. In the Socket progrmming, if you adopted a decent style, you probably spent quite a lot of time writing
    -- handler functions that did exactly this kind of plumbing. No need in Haskell - you get it for free, and entirely
    -- type checked. This is a big win in practice.
    searchMessage Nothing = liftIO $ do
      warnLog $ "No key for searching."
      return $ ([] :: [Message])

    -- | Performing a REST call
    -- The following function performs a REST call to a remote service 'hackage.haskell.org'. This remote service is a
    -- searchable documentation server. The API to the service is accessible at http://hackage.haskell.org
    performRESTCall :: Maybe String -> Handler ResponseData
    performRESTCall (Just filt) = liftIO $ do
      warnLog $ "recieved request to perform REST call with param " ++ filt
      doRest $ DL.filter (DL.isInfixOf filt)

    -- | An implementation when no parameter is passed, no filtering so.
    performRESTCall Nothing = liftIO $ do
      warnLog $ "recieved request to perform REST call, but no param "
      doRest id

    -- | the performRESTCall is delegated to this function, with a filtering function passed as a parameter
    doRest :: ([String] -> [String]) -> IO ResponseData
    doRest flt = do
      -- first we perform the call to hackage.org, then we will extract the package names and filter
      -- to include only package names matching the 'filt' parameter, returning a comma separated string of those
      -- packages.
      res <- SC.runClientM getPackages =<< env   -- the actual REST call
      case res of
        Left err -> do
          warnLog $ "Rest call failed with error: " ++ show err
          return $ ResponseData $ "Rest call failed with error: " ++ show err
        Right pkgs -> do
          return $ ResponseData $ DL.intercalate ", " $                          -- reduce to comma separated string
                                  flt $                                          -- run the filtering function
                                  DL.map (unpack . RestClient.packageName) pkgs  -- extract name and convert to string
      where env = do
             manager <- newManager defaultManagerSettings
             return (SC.ClientEnv manager (SC.BaseUrl SC.Http "hackage.haskell.org" 80 ""))

    debugSaveUser :: User -> Handler Bool
    debugSaveUser u@(User n p) = liftIO $ do 
      withMongoDbConnection $ upsert (select ["name" =: n] "users") $ toBSON u
      return True  -- this is a simple debug method - no need for error checking

{-
    storeMessage msg@(Message key _) = liftIO $ do
      warnLog $ "Storing message under key " ++ key ++ "."

      -- upsert creates a new record if the identified record does not exist, or if
      -- it does exist, it updates the record with the passed document content
      -- As you can see, this takes a single line of code
      withMongoDbConnection $ upsert (select ["name" =: key] "MESSAGE_RECORD") $ toBSON msg

      return True  -- as this is a simple demo I'm not checking anything
-}


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




