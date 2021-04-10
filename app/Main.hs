module Main where

import qualified BF.Stateful                   as BF
import qualified BF.Parsing                    as BF
import           Control.Monad                  ( when )
import           System.Environment             ( getArgs )
import           System.Exit                    ( die )

usage :: String
usage = "usage: fuckhaskell [FILENAME]"

main :: IO ()
main = do
  args <- getArgs
  when (null args) (die usage)
  instructions <- BF.parseFile $ head args
  either print BF.eval instructions
