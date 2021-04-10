module Main where

import qualified BF
import qualified BF.Parsing                    as BF
import           Control.Monad                  ( when )
import           System.Exit                    ( die )
import           System.Environment             ( getArgs )

usage :: String
usage = "usage: fuckhaskell [FILENAME]"

main :: IO ()
main = do
  args <- getArgs
  when (null args) (die usage)
  instructions <- BF.parseFile $ head args
  either print BF.eval instructions

