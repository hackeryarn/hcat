module Main where

import System.Environment (getArgs)

main :: IO ()
main = (head <$> getArgs) >>= readFile >>= putStrLn
