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
import qualified Data.Aeson as Dave
import qualified Data.ByteString.Char8 as S8
import qualified Data.Yaml             as Yaml
import           Network.HTTP.Simple
import FileSystemAuthServerAPI hiding (Lib)
import FileSystemFileServerAPI hiding (Lib)
import FileSystemDirectoryServerAPI hiding (Lib)
import FileSystemLockServerAPI hiding (Lib)
import FileSystemTransactionServerAPI hiding (Lib)

etcDirname :: String
etcDirname = "/.haskell_client"

tokenFilename :: String
tokenFilename = "token"

seedFilename :: String
seedFilename = "encyption_key_seed"

data FileSystemServer = AuthServer | FileServer | DirectoryServer | LockServer | TransactionServer

-- GET / POST request methods

performLockFile :: LockFileReq -> ConnectionInformation -> IO Bool
performLockFile req info = do
  let str = "POST http://" ++ (hostAddr info) ++  ":" ++ (hostPort info) ++ "/lockFile"
  let initReq = parseRequest_ str
  let request = setRequestBodyJSON req $ initReq
  response <- httpJSON request
  let ret = (getResponseBody response :: Bool)
  return ret

performUnlockFile :: UnlockFileReq -> ConnectionInformation -> IO Bool
performUnlockFile req info = do
  let str = "POST http://" ++ (hostAddr info) ++  ":" ++ (hostPort info) ++ "/unlockFile"
  let initReq = parseRequest_ str
  let request = setRequestBodyJSON req $ initReq
  response <- httpJSON request
  let ret = (getResponseBody response :: Bool)
  return ret

resolveFile :: ResolutionRequest -> ConnectionInformation -> IO ResolutionResponse
resolveFile req info = do
  let str = "POST http://" ++ (hostAddr info) ++  ":" ++ (hostPort info) ++ "/resolveFile"
  putStrLn str
  putStrLn $ show req
  let initReq = parseRequest_ str
  let request = setRequestBodyJSON req $ initReq
  response <- httpJSON request
  let ret = (getResponseBody response :: ResolutionResponse)
  return ret

performAddFileServer :: FileServerRecord -> ConnectionInformation -> IO Bool
performAddFileServer record info = do
  let str = "POST http://" ++ (hostAddr info) ++  ":" ++ (hostPort info) ++ "/insertServerRecord"
  let initReq = parseRequest_ str
  let request = setRequestBodyJSON record $ initReq
  response <- httpJSON request
  let ret = (getResponseBody response :: Bool)
  return ret

performRetrieveFile :: ReadFileReq -> ConnectionInformation -> IO ReadFileResp
performRetrieveFile rfr info = do
  let str = "POST http://" ++ (hostAddr info) ++  ":" ++ (hostPort info) ++ "/readFromFile"
  let initReq = parseRequest_ str
  let request = setRequestBodyJSON rfr $ initReq
  response <- httpJSON request
  let ret = (getResponseBody response :: ReadFileResp)
  return ret

performStoreFile :: WriteFileReq -> ConnectionInformation -> IO WriteFileResp
performStoreFile wfreq info = do
  let str = "POST http://" ++ (hostAddr info) ++  ":" ++ (hostPort info) ++ "/writeToFile"
  let initReq = parseRequest_ str
  let request = setRequestBodyJSON wfreq $ initReq
  response <- httpJSON request
  let ret = (getResponseBody response :: WriteFileResp)
  return ret

performAuthUser :: User -> ConnectionInformation -> IO AuthResponse
performAuthUser user info = do
  let str = "POST http://" ++ (hostAddr info) ++  ":" ++ (hostPort info) ++ "/authUser"
  let initReq = parseRequest_ str
  let request = setRequestBodyJSON user $ initReq
  response <- httpJSON request
  let ret = (getResponseBody response :: AuthResponse)
  return ret

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
  show FileServer = "FILE_SERVER"
  show AuthServer = "AUTH_SERVER"
  show DirectoryServer = "DIR_SERVER"
  show LockServer = "LOCK_SERVER"
  show TransactionServer = "TRANS_SERVER"

class ETCFile a where
  etcfile :: a -> String

instance ETCFile FileSystemServer where
  etcfile AuthServer = "auth_server_ci"
  etcfile FileServer = "file_server_ci"
  etcfile DirectoryServer = "directory_server_ci"
  etcfile LockServer = "lock_server_ci"
  etcfile TransactionServer = "transaction_server_ci"

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
    updateDirServer cnxnInfo (show fss)
    return cnxnInfo

updateDirServer :: ConnectionInformation -> String -> IO Bool
updateDirServer info@(ConnectionInformation host port) name = do
  dirServer <- getConnectionInfo DirectoryServer 
  let serverRec = FileServerRecord host port "0" "0"
      fsServerRec = FileSystemServerRecord name serverRec
  performUpdateDirServer fsServerRec dirServer

performUpdateDirServer :: FileSystemServerRecord -> ConnectionInformation -> IO Bool
performUpdateDirServer fss cnxnInfo@(ConnectionInformation h p) = do
  let str = "POST http://" ++ h ++  ":" ++ p ++ "/addServer"
  let initReq = parseRequest_ str
  let request = setRequestBodyJSON fss $ initReq
  response <- httpJSON request
  let ret = (getResponseBody response :: Bool)
  return ret

saveAuthToken :: String -> IO ()
saveAuthToken token = do
  etcDirectory <- etcDir
  writeFile (etcDirectory ++ tokenFilename) token

getAuthToken :: IO String
getAuthToken = do
  etcDirectory <- etcDir
  contents <- readFile (etcDirectory ++ tokenFilename)
  return contents
  
saveEncryptionKeySeed :: String -> IO ()
saveEncryptionKeySeed seed = do
  etcDirectory <- etcDir
  writeFile (etcDirectory ++ seedFilename) seed

getEncryptionKeySeed :: IO String
getEncryptionKeySeed = do
  etcDirectory <- etcDir
  contents <- readFile (etcDirectory ++ seedFilename)
  return contents

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

-- Processing of Arguements - where stuff actually gets done

lockFile :: String -> IO Bool
lockFile name = do
  cnxnInfo <- getConnectionInfo LockServer
  let req = LockFileReq name 
  isLocked <- performLockFile req cnxnInfo
  return isLocked

unlockFile :: String -> IO Bool
unlockFile name = do
  cnxnInfo <- getConnectionInfo LockServer
  let req = UnlockFileReq name 
  isUnlocked <- performUnlockFile req cnxnInfo
  return isUnlocked

-- User Authentication
authenticate :: [String] -> IO ()
authenticate params = do
  let name = (params !! 0)
  let pass = (params !! 1)
  cxnInfo <- getConnectionInfo AuthServer
  authResp <- performAuthUser (User name (encryptString name pass )) cxnInfo
  let senToken = read (decryptString (encSenderToken authResp) pass) :: SenderToken
  saveAuthToken $ encReceiverToken senToken
  saveEncryptionKeySeed $ senKey1Seed senToken
  putStrLn ("username: " ++ name ++ " authorised with password " ++ pass)
  return ()

-- Add a User to the database of the authentication server
addUser :: [String] -> IO ()
addUser params = do
  cnxnInfo <- getConnectionInfo AuthServer
  let name = (params !! 0)
  let pass = (params !! 1)
  performAddUser (User name pass) cnxnInfo
  putStrLn $ "Added " ++ name ++ ":" ++ pass
  return ()

--storing a file in the file system
-- This intends to write to a file on the system.
-- Attempts to lock file with Lock Server, then gets location of primary file record (a file server address),
-- then writes to the file server
-- This will invalidate any cache copy the directory server has for this file
storeFile :: [String] -> IO ()
storeFile params = do
  let path = (params !! 0)
  token <- getAuthToken
  key1 <- getEncryptionKeySeed

  --lock it
  isLocked <- lockFile path

  --TODO added repeated tries with exponential back-off
  if not isLocked 
  then do
    putStrLn "Couldn't lock file"
    return ()
  else do
    
    --find location
    cnxnInfo <- getConnectionInfo DirectoryServer
    putStrLn ("resolving " ++ path ++ " for writting")
    -- this will always return a file server record - either an existing record or a newly create one
    resResponse <- resolveFile (ResolutionRequest path "WRITE" token) cnxnInfo 


    --update it at location
    let serverRec = serverRecord (resolution resResponse) 
        cnxnInfo = ConnectionInformation (fsHost serverRec) (fsPort serverRec)
    fileData <- readFile path
    let req = WriteFileReq token path (encryptString fileData key1)
    resp <- performStoreFile req cnxnInfo
    putStrLn $ show resp

    unlockFile path
    return ()
  
-- Looks up location of a file with the directory server and 
-- fetches it from file server (unless the directory server had a cache copy)
-- Doesn't require locking as not updating file at this time.
retrieveFile :: [String] -> IO ()
retrieveFile params = do
  let path = (params !! 0)
  token <- getAuthToken
  key1 <- getEncryptionKeySeed
  cnxnInfo <- getConnectionInfo DirectoryServer
  putStrLn ("going to resolve file")
  resResponse <- resolveFile (ResolutionRequest path "READ" token) cnxnInfo 
  
  case (resolutionStatus resResponse) of
    True -> do
      let fileRec = resolution resResponse
      putStrLn $ "true response"
      case (cacheHit resResponse) of
        True -> do 
          putStrLn $ "Cache HIT!!! WUHOO!"
          writeFile (fileRecordName fileRec)  (decryptString (cachedData resResponse) key1)

        False -> do
          putStrLn $ "Cache MISS!!! Doh!" 
          let serverRec = serverRecord fileRec 
              cnxnInfo = ConnectionInformation (fsHost serverRec) (fsPort serverRec)
              req = ReadFileReq token path 

          fileRO <- performRetrieveFile req cnxnInfo
          writeFile path (decryptString (encryptedResult fileRO) key1)
           
          putStrLn "File Retrieved"

    False -> do
      putStrLn "File resolution failed - file doesn't exist!!!"
  
  return ()

addFileServer :: [String] -> IO ()
addFileServer params = do
  let host = (params !! 0)
  let port = (params !! 1)
  let name = (params !! 2)
  cnxnInfo <- getConnectionInfo DirectoryServer
  let record = FileServerRecord host port "0" "0"
  resp <- performAddFileServer record cnxnInfo
  putStrLn $ show resp
  updateDirServer (ConnectionInformation host port) name
  return ()  


-- TRANSACTION SERVER 
--transStart :: IO ()
--transStart 



-- Establishes the location of all server from the user
-- and sends these to the directory server to be propagated
-- throughout the system
configureFileSystem :: IO ()
configureFileSystem = do
  getConnectionInfo DirectoryServer
  getConnectionInfo LockServer
  getConnectionInfo AuthServer
  getConnectionInfo TransactionServer
  putStrLn $ "All servers have been located"

-- be rude not to
helloWorld :: IO ()
helloWorld = liftIO $ do
  putStrLn "Hello, world."

processArgs :: [String] -> IO ()
processArgs (x:xs) = liftIO $ do
  case x of
    "hello" 					-> helloWorld             --simple test to ensure running
    "clean-etc"				-> removeETCDir           --cleans config files from etc directory
    "auth"  					-> authenticate xs      -- name, pass  
    "add-user"        -> addUser xs           -- name, pass
    "write-file"      -> storeFile xs         -- file_path
    "read-file"       -> retrieveFile xs      -- file_path
    "add-file-server" -> addFileServer xs     -- host, port, uniqueIdentifier (see file service Lib.hs)
--    "trans-start"     -> transStart xs
--    "trans-commit"    -> transCommit xs
--    "trans-abort"     -> transAbort xs
--    "trans-write"     -> transWrite xs
    "configure"       -> configureFileSystem  -- guided set up


processArgs [] = liftIO $ do
  putStrLn "You didn't provide any args"



someFunc :: IO ()
someFunc = do
  args <- getArgs
  ensureHomeDir
  processArgs args

