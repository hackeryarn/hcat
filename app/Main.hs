module Main where

import           App
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import           System.Environment (getArgs)

main :: IO ()
main = do
  filename  <- head <$> getArgs
  (Cfg w h) <- defaultConfig
  contents  <- Text.readFile filename
  let wrapped = wordWrap w contents
  foldMap Text.putStrLn wrapped
