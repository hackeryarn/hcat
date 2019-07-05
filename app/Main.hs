module Main where

import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import System.Environment (getArgs)

main :: IO ()
main =
  let fname = (head <$> getArgs)
  in fname
  >>= readFile
  >>= putStrLn
