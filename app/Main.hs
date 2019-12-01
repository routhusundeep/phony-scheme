module Main where

import           Control.Monad
import           PSREPL
import           System.Environment

main :: IO ()
main = do args <- getArgs
          if null args then runRepl else runOne $ args
