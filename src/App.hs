module App where

import Control.Monad.Except
import Control.Monad.Fail
import Control.Monad.Reader
import Data.Maybe as Maybe
import Data.Text as Text
import System.Process as Process

data Cfg = Cfg
  { cfgTermWidth :: Int
  , cfgTermHeight :: Int
  } deriving (Show)

defaultConfig :: IO Cfg
defaultConfig = Cfg <$> term "lines" <*> term "cols"
  where
    term cmd = read <$> Process.readProcess "tput" [cmd] ""

wordWrap :: Int -> Text.Text -> [Text.Text]
wordWrap w txt =
  if Text.length txt < w
    then [txt]
    else let myOffset = Maybe.fromMaybe w (boundryOffset w txt)
             (thisLine, rest) = Text.splitAt myOffset txt
          in thisLine : wordWrap w rest

boundryOffset :: Int -> Text.Text -> Maybe Int
boundryOffset 0 _ = Nothing
boundryOffset idx text =
  if Text.index text idx == ' '
    then Just idx
    else boundryOffset (pred idx) text
