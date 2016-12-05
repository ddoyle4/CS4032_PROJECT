module Lib  where


import System.Environment
import Control.Monad.IO.Class
import System.Directory
import Control.Monad
import Data.List.Split

etcFilename :: String
etcFilename = "/.haskell_client"

authServerEtc :: String
authServerEtc = "auth_server_ci"

fileServerEtc :: String
fileServerEtc = "file_server_ci"

data Server = AuthServer | FileServer 

instance Show Server where
  show FileServer = "The File Server"
  show AuthServer = "The Authentification Server"


parseConnectionInfo :: String -> IO ConnectionInfo
parseConnectionInfo str = do
  let spl = splitOn "\n" str
  return (ConnectionInfo (spl !! 0) (spl !! 1))

writeEtcFile :: Server -> ConnectionInfo -> IO ()
writeEtcFile p ci = do
  let writeString = (hostAddr ci) ++ "\n" ++ (hostPort ci)
  writeFile p writeString

--queries user to create ETC file if it doesn't exist already
ensureEtcExists :: Server -> String -> IO ()
ensureEtcExists server fullPath = do
  ex <- doesFileExist fullPath
  when (not ex) (do
    putStrLn ("Must enter information for" ++ (show server))
    putStrLn "Please enter hostname:"
    hostName <- getLine
    putStrLn "Please enter port number:"
    portNumber <- getLine
    let ci = ConnectionInfo hostName portNumber
    writeEtcFile fullPath ci)



getConnectionInfo :: Server -> IO ConnectionInfo  
getConnectionInfo s = do
  basePath <- etcDir

  fullPath <- 
    case s of
      AuthServer -> return (basePath ++ authServerEtc)
      FileServer -> return (basePath ++ fileServerEtc)

  ensureEtcExists s fullPath

  fc <- readFile fullPath
  ci <- parseConnectionInfo fc
  return ci


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

