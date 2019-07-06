module Main where

import           App
import           Control.Monad.Reader
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import           System.Environment (getArgs)

main :: IO ()
main = defaultConfig >>= runApp main'
  where
      main' = AppT $ do
        (Cfg width height) <- ask
        fname   <- liftIO $ head <$> getArgs 
        txt     <- liftIO . Text.readFile $ fname
        let textHeight = height - 2
            inputLines = Text.lines txt
            wrapped = concatMap (wordWrap width) inputLines
            paginated = pagesOf textHeight wrapped
            pages = map (mkSparsePage textHeight) paginated
            pageCnt = length pages
            statusBars = map (statusBar fname width pageCnt) [1..]
            addBar page bar = Text.unlines [page, bar]
            pagesWithStatusBar = zipWith addBar pages statusBars
        liftIO $ print textHeight
        liftIO $ mapM_ Text.putStrLn pagesWithStatusBar
