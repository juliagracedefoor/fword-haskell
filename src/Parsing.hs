module Parsing
  ( bfParseFile
  , bfParse
  , peval
  , pexec
  )
where

import           Text.Parsec
import           Data.Functor                   ( (<$) )
import           Control.Monad                  ( (<=<) )
import qualified BF

-- peval == "parse and evaluate"
peval :: String -> IO ()
peval = either print BF.eval . bfParse ""

-- pexec == "parse and execute"
pexec :: String -> IO ()
pexec = either print (print <=< BF.exec) . bfParse ""

bfParseFile :: String -> IO (Either ParseError [BF.Symbol])
bfParseFile fp = bfParse fp <$> readFile fp

bfParse :: String -> String -> Either ParseError [BF.Symbol]
bfParse = parse (bfCode eof)

bfCode :: Parsec String () a -> Parsec String () [BF.Symbol]
bfCode terminator = do
  skipMany fluff
  result <- nextSymbol
  continuation result
 where
  nextSymbol   = (Just <$> symbol) <|> (Nothing <$ terminator)
  continuation = maybe (return []) (\s -> fmap (s :) (bfCode terminator))

symbol :: Parsec String () BF.Symbol
symbol = (BF.Instructions <$> instructions) <|> (BF.Loop <$> loop)

instructions :: Parsec String () String
instructions = filter (`elem` BF.characters) <$> many1 (noneOf "[]")

loop :: Parsec String () [BF.Symbol]
loop = between (char '[') (char ']') (bfCode endOfLoop)
  where endOfLoop = lookAhead $ char ']'

fluff :: Parsec String () Char
fluff = noneOf BF.characters


