module Lib  where


import System.Environment
import Control.Monad.IO.Class
import System.Directory


host :: String
host = "http://localhost"

port :: String
port = "8081"

{-
saveEntry :: [String] -> IO ()
saveEntry params = liftIO $ do
  c <- openConnection host port
  let q = buildRequest1 $ do
		http GET "/"
		setAccept "text/html"

  sendRequest c q emptyBody
	receiveResponse c (\p i -> do
		xm <- Streams.read i
		case xm of
				Just x    -> S.putStr x
				Nothing   -> "")

	closeConnection c
-}

helloWorld :: IO ()
helloWorld = liftIO $ do
  putStrLn "Hello, world."

processArgs :: [String] -> IO ()
processArgs (x:xs) = liftIO $ do
  case x of
    "hello" -> helloWorld

processArgs [] = liftIO $ do
  putStrLn "You didn't provide any args"


ensureHomeDir :: IO ()
ensureHomeDir = liftIO $ do
	homeDir <- getHomeDirectory
	exists <- doesDirectoryExist (homeDir ++ ".haskell_client") 
	if not exists
	then return () 
    --createDirectory homeDir ++ ".haskell_client"
    --return ()
	else return ()
	
someFunc :: IO ()
someFunc = do
  args <- getArgs
  processArgs args



