module Main where

import Data.Char
import System.IO
import Control.Monad.IO.Class
import Data.List
import System.Console.ANSI


data Contents = Contents {
  before :: [Char]
  , after :: [Char]
  , cursor :: Int
}


main :: IO ()
main = do
  putStrLn "hello world"
  hSetEcho stdin False
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  let emptyContents = Contents [] [] 0
  clearScreen
  run emptyContents

run :: Contents -> IO ()
run contents = do
  input <- getChar
  newContents <- processInput input contents
  render newContents
  run newContents

render :: Contents -> IO ()
render (Contents bs as c) = do
  clearScreen
  cursorBackward c
  renderList (reverse bs)
  renderList as

renderList :: [Char] -> IO ()
renderList (x:xs) = do
  putChar x
  renderList xs

renderList [] = return ()

--processInput :: Char -> Contents -> Contents
processInput char (Contents (bs) (as) c) = do
  let before = char:bs
  let after = as
  let newC = c + 1
  return (Contents before after newC)







