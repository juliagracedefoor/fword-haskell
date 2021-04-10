module Parsing
  ( bfParseFile
  , bfParse
  , peval
  , pexec
  )
where

import           Text.Parsec
import           Control.Monad                  ( (<=<) )
import qualified BF

-- peval == "parse and evaluate"
peval :: String -> IO ()
peval = either print BF.eval . bfParse ""

-- pexec == "parse and execute"
pexec :: String -> IO ()
pexec = either print (print <=< BF.exec) . bfParse ""

bfParseFile :: String -> IO (Either ParseError [BF.Symbol])
bfParseFile name = bfParse name <$> readFile name

bfParse :: String -> String -> Either ParseError [BF.Symbol]
bfParse = parse (symbolsTill eof)

symbolsTill :: Parsec String () a -> Parsec String () [BF.Symbol]
symbolsTill = manyTill symbol
  where symbol = (BF.Instructions <$> instructions) <|> (BF.Loop <$> loop)

instructions :: Parsec String () String
instructions = filter (`elem` BF.characters) <$> many1 (noneOf "[]")

loop :: Parsec String () [BF.Symbol]
loop = between (char '[') (char ']') (symbolsTill endOfLoop)
  where endOfLoop = lookAhead $ char ']'


