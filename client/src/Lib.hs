module Lib  where


import System.Environment
import Control.Monad.IO.Class
import System.Directory
import Control.Monad
import Data.List.Split


data Server = AuthServer | FileServer 

instance Show Server where
  show FileServer = "The File Server"
  show AuthServer = "The Authentification Server"


--reads connection info from an ETC Connection Info File
readEtcConnectionFile :: Server -> IO [String]
readEtcConnectionFile s = do
  d <- etcDir
  let pathEtc = d ++ etcFileName
  ensureEtcExists s pathEtc
  fileContents <- readFile pathEtc
  return (splitOn "\n" fileContents)
  where
  etcFileName = case s of
    AuthServer -> authServerEtc
    FileServer -> fileServerEtc

--retrieves host:port info save on file, or requests from user if absent
getConnectionInfo :: Server -> IO ConnectionInfo  
getConnectionInfo s = do
  details <- readEtcConnectionFile s
  return (ConnectionInfo (details !! 0) (details !! 1))



etcFilename :: String
etcFilename = "/.haskell_client"

authServerEtc :: String
authServerEtc = "auth_server_ci"

fileServerEtc :: String
fileServerEtc = "file_server_ci"

data ConnectionInfo = ConnectionInfo 
  { hostAddr :: String
  , hostPort :: String
  } deriving Show


helloWorld :: IO ()
helloWorld = liftIO $ do
  putStrLn "Hello, world."

processArgs :: [String] -> IO ()
processArgs (x:xs) = liftIO $ do
  case x of
    "hello" -> helloWorld

processArgs [] = liftIO $ do
  putStrLn "You didn't provide any args"


etcDir :: IO String
etcDir = do
  h <- getHomeDirectory 
  return (h ++ etcFilename)


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




