module Parsing where

import           Text.Parsec
import           Data.Functor                   ( (<$)
                                                , void
                                                )
import           Control.Arrow                  ( ArrowChoice(left) )
import           BF                             ( instructions )

data BFSymbol = Instructions String | Loop [BFSymbol]

instance Show BFSymbol where
  show (Instructions s ) = "I " ++ "|" ++ s ++ "|"
  show (Loop         xs) = "L " ++ show xs

bfParseFile :: String -> IO (Either ParseError [BFSymbol])
bfParseFile fp = parse bfParser fp <$> readFile fp

bfParser :: Parsec String () [BFSymbol]
bfParser = bfParserWith eof

bfParserWith :: Parsec String () a -> Parsec String () [BFSymbol]
bfParserWith endParser = do
  symbol <-
    (Just <$> instructionsParser)
    <|> (Just <$> loopParser)
    <|> (Nothing <$ endParser)
  case symbol of
    Just s  -> fmap (s :) (bfParserWith endParser)
    Nothing -> return []

instructionsParser :: Parsec String () BFSymbol
instructionsParser =
  Instructions . filter (`elem` instructions) <$> many1 (noneOf "[]")

loopParser :: Parsec String () BFSymbol
loopParser =
  Loop <$> between (char '[') (char ']') (bfParserWith . lookAhead $ char ']')



