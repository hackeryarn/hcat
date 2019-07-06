module App where

import           Control.Monad.Except
import           Control.Monad.Fail
import           Control.Monad.Reader
import           Data.Monoid as Monoid
import           Data.Char as Char
import           Data.List as List
import           Data.Maybe as Maybe
import qualified Data.Text as Text
import           System.FilePath as FilePath
import           System.Process as Process
import           Text.Printf as Printf

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

pagesOf :: Int -> [a] -> [[a]]
pagesOf cnt lst = reverse $ pagesOf' [] cnt lst
  where
    pagesOf' :: [[a]] -> Int -> [a] -> [[a]]
    pagesOf' carry cnt [] = carry
    pagesOf' carry cnt lst =
      let (hd, tl) = splitAt cnt lst
       in pagesOf' (hd : carry) cnt tl

termSeq :: Char -> [Int] -> Text.Text
termSeq c codes =
  let codes' = List.intercalate ";" . map show $ codes
      escape = Char.chr 27
   in Text.pack $ Printf.printf "%c[%s%c" escape codes' c

faceSeq :: [Int] -> Text.Text
faceSeq = termSeq 'm'

screenSeq :: [Int] -> Text.Text
screenSeq = termSeq 'J'

statusBar :: FilePath -> Int -> Int -> Int -> Text.Text
statusBar filename width maxPages currentPage =
  let startSeq = faceSeq [7]
      endSeq = faceSeq [0]
      ctrlLen = Text.length $ startSeq <> endSeq
      width' = width + ctrlLen
      pageCounter = Text.pack $ Printf.printf "(%d/%d)" currentPage maxPages
      bname = Text.pack . FilePath.takeBaseName $ filename
      barWidth = Text.length pageCounter + Text.length bname
      paddingAmount = width - (min width barWidth)
      padding = Text.replicate paddingAmount " "
   in Text.take width' $
      Monoid.mconcat [startSeq, bname, padding, pageCounter, endSeq]

mkSparsePage :: Int -> [Text.Text] -> Text.Text
mkSparsePage pageSize pageLines =
  let lineCount = length pageLines
      extraLines = Text.replicate (pageSize - lineCount) "\n"
      joined = mconcat $ (List.intersperse "\n" pageLines)
  in joined <> extraLines

type MyApp = ReaderT Cfg

newtype AppT m a = AppT
  { runAppT :: MyApp m a
  } deriving (Functor, Applicative, Monad, MonadIO, MonadReader Cfg)

runApp :: AppT m a -> Cfg -> m a
runApp = runReaderT . runAppT
