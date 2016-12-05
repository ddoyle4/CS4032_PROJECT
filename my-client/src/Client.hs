module Client where


import System.Environment
import Control.Monad.IO.Class
import System.Directory
import Control.Monad
import Data.List.Split
import FileSystemEncryption hiding (Lib)
import System.IO.Streams (InputStream, OutputStream, stdout)
import qualified System.IO.Streams as Streams
import qualified Data.ByteString as S
import Network.HTTP.Client

etcDirname :: String
etcDirname = "/.haskell_client"

data FileSystemServer = AuthServer | FileServer 




-- GET / POST request methods
sendPostRes = do
  manager <- newManager defaultManagerSettings

  request <- parseRequest "http://httpbin.org/post"
  response <- httpLbs request manager

  --putStrLn $ "The status code was: " ++ (show $ statusCode $ responseStatus response)
  print $ responseBody response


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
  putStrLn $ "filepath: " ++ (etcDirectory ++ (etcfile fss))
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
authenticate :: [String] -> IO ()
authenticate params = do
  cxnInfo <- getConnectionInfo AuthServer
  putStrLn $ show cxnInfo
  sendPostRes
  putStrLn "done"
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

