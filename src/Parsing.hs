module Parsing
  ( bfParseFile
  , bfParse
  )
where

import           Text.Parsec
import           Data.Functor                   ( (<$) )
import qualified BF

bfParse :: String -> Either ParseError [BF.Symbol]
bfParse = parse bfParser ""

bfParseFile :: String -> IO (Either ParseError [BF.Symbol])
bfParseFile fp = parse bfParser fp <$> readFile fp

bfParser :: Parsec String () [BF.Symbol]
bfParser = bfParserWith eof

bfParserWith :: Parsec String () a -> Parsec String () [BF.Symbol]
bfParserWith endParser = do
  symbol <-
    (Just <$> instructionsParser)
    <|> (Just <$> loopParser)
    <|> (Nothing <$ endParser)
  case symbol of
    Just s  -> fmap (s :) (bfParserWith endParser)
    Nothing -> return []

instructionsParser :: Parsec String () BF.Symbol
instructionsParser =
  BF.Instructions . filter (`elem` BF.characters) <$> many1 (noneOf "[]")

loopParser :: Parsec String () BF.Symbol
loopParser = BF.Loop
  <$> between (char '[') (char ']') (bfParserWith . lookAhead $ char ']')



