module Lib (libMain) where

import App
import qualified Control.Monad.IO.Class as IO

libMain :: IO ()
libMain = do
  print "Hello World"
