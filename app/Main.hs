module Main where

import qualified BF
import           Parsing                        ( bfParseFile )
import           Control.Monad                  ( when )
import           System.Exit                    ( die )
import           System.Environment             ( getArgs )

usage :: String
usage = "usage: fuckhaskell [FILENAME]"

main :: IO ()
main = do
  args <- getArgs
  when (null args) (die usage)
  instructions <- bfParseFile $ head args
  either print BF.eval instructions

