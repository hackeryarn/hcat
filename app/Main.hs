module Main where

import System.Environment (getArgs)

main :: IO ()
main = do
  let
    getFilename :: IO FilePath
    getFilename = do
      args <- getArgs
      return (head args)
  fileName <- getFilename
  contents <- readFile fileName
  putStrLn contents
