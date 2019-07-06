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
      (Cfg w h) <- ask
      liftIO $ do
        filename <- head <$> getArgs
        contents <- Text.readFile filename
        let wrapped = wordWrap w contents
        mapM_ Text.putStrLn wrapped
