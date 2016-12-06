{-# LANGUAGE OverloadedStrings #-}

module Client where


import System.Environment
import Control.Monad.IO.Class
import System.Directory
import Control.Monad
import Data.List.Split
import System.IO.Streams (InputStream, OutputStream, stdout)
import qualified System.IO.Streams as Streams
import qualified Data.ByteString as S
import           Data.Aeson
import qualified Data.ByteString.Char8 as S8
import qualified Data.Yaml             as Yaml
import           Network.HTTP.Simple
import FileSystemAuthServerAPI hiding (Lib)

etcDirname :: String
etcDirname = "/.haskell_client"

data FileSystemServer = AuthServer | FileServer 




-- GET / POST request methods

performAuthUser :: User -> ConnectionInformation -> IO ()
performAuthUser user info = do
  let str = "POST http://" ++ (hostAddr info) ++  ":" ++ (hostPort info) ++ "/authUser"
  let initReq = parseRequest_ str
  let request = setRequestBodyJSON user $ initReq
  response <- httpJSON request
  let ret = (getResponseBody response :: AuthResponse)
  return ()

performAddUser :: User -> ConnectionInformation -> IO ()
performAddUser user info = do
  let str = "POST http://" ++ (hostAddr info) ++  ":" ++ (hostPort info) ++ "/debugSaveUser"
  initReq <- parseRequest str
  let request = setRequestBodyJSON user $ initReq
  response <- httpJSON request
  S8.putStrLn $ Yaml.encode (getResponseBody response :: Value)



-- Connection Information Data Types & Methods
-- These help with storing / retrieving locations of servers
instance Show FileSystemServer where
  show FileServer = "The File Server"
  show AuthServer = "The Authentification Server"

class ETCFile a where
  etcfile :: a -> String

instance ETCFile FileSystemServer where
  etcfile AuthServer = "auth_server_ci"
  etcfile FileServer = "file_server_ci"

data ConnectionInformation = ConnectionInformation
  { hostAddr :: String
  , hostPort :: String
  } deriving Show

readConnectionInfo :: FileSystemServer -> IO ConnectionInformation
readConnectionInfo fss = do
  etcDirectory <- etcDir
  rawData <- readFile (etcDirectory ++ (etcfile fss))
  --putStrLn $ "filepath: " ++ (etcDirectory ++ (etcfile fss))
  let ls = splitOn "\n" rawData
  return $ ConnectionInformation (ls !! 0) (ls !! 1)

writeConnectionInfo :: FileSystemServer -> IO ConnectionInformation
writeConnectionInfo fss = do
  etcDirectory <- etcDir
  putStrLn $ "No connection info on file for " ++ (show fss) ++ ". Enter host:" 
  host <- getLine
  putStrLn $ "Enter port:"
  port <- getLine
  writeFile (etcDirectory ++ (etcfile fss)) (host ++ "\n" ++ port)
  return $ ConnectionInformation host port

connectionInfoExists :: FileSystemServer -> IO Bool
connectionInfoExists fss = do
  etcDirectory <- etcDir
  fileExists <- doesFileExist (etcDirectory ++ (etcfile fss))
  return fileExists

getConnectionInfo :: FileSystemServer -> IO ConnectionInformation
getConnectionInfo fss = do
  infoExists <- connectionInfoExists fss
  if infoExists
  then do
    cnxnInfo <- readConnectionInfo fss 
    return cnxnInfo
  else do
    cnxnInfo <- writeConnectionInfo fss
    return cnxnInfo

-- Processing of Arguements - where stuff actually gets done
-- User Authentication
authenticate :: [String] -> IO ()
authenticate params = do
  cxnInfo <- getConnectionInfo AuthServer
  --putStrLn $ show cxnInfo
  performAuthUser (User "dave" (encryptString "dave" "password")) cxnInfo
  putStrLn "done"
  return ()

-- Add a User to the database off the authentication server
addUser :: [String] -> IO ()
addUser params = do
  cnxnInfo <- getConnectionInfo AuthServer
  let name = (params !! 0)
  let pass = (params !! 1)
  performAddUser (User name pass) cnxnInfo
  putStrLn $ "Added " ++ name ++ ":" ++ pass
  return ()



-- be rude not to
helloWorld :: IO ()
helloWorld = liftIO $ do
  putStrLn "Hello, world."

processArgs :: [String] -> IO ()
processArgs (x:xs) = liftIO $ do
  case x of
    "hello" 					-> helloWorld
    "clean-etc"				-> removeETCDir
    "auth"  					-> authenticate xs 
    "addUser"         -> addUser xs

processArgs [] = liftIO $ do
  putStrLn "You didn't provide any args"


etcDir :: IO String
etcDir = do
  h <- getHomeDirectory 
  return (h ++ etcDirname ++ "/")

--used to reset all of the ETC files
removeETCDir :: IO ()
removeETCDir = do
	d <- etcDir
	removeDirectoryRecursive d

ensureHomeDir :: IO ()
ensureHomeDir = do
  d <- etcDir
  ex <- doesDirectoryExist d
  when (not ex) (do 
    putStrLn ("Creating new haskell client directory:" ++ d)
    createDirectory d)

someFunc :: IO ()
someFunc = do
  args <- getArgs
  ensureHomeDir
  processArgs args

