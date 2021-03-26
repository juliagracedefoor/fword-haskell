module Main where

import qualified BF
import           Control.Monad
import           System.Exit
import           System.IO
import           System.Environment

usage :: String
usage = "usage: fuckhaskell [FILENAME]"

main :: IO ()
main = do
  args <- getArgs
  when (null args) (die usage)
  contents <- readFile $ head args
  BF.eval contents

