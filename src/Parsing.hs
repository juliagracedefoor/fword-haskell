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
import           Data.Maybe                     ( catMaybes )

-- peval == "parse and evaluate"
peval :: String -> IO ()
peval = either print BF.eval . bfParse ""

-- pexec == "parse and execute"
pexec :: String -> IO ()
pexec = either print (print <=< BF.exec) . bfParse ""

bfParseFile :: String -> IO (Either ParseError [BF.Instruction])
bfParseFile name = bfParse name <$> readFile name

bfParse :: String -> String -> Either ParseError [BF.Instruction]
bfParse = parse (instructionsTill eof)

loop :: Parsec String () [BF.Instruction]
loop = between (char '[') (char ']') innards
  where innards = instructionsTill $ lookAhead (char ']')

instructionsTill :: Parsec String () end -> Parsec String () [BF.Instruction]
instructionsTill = fmap catMaybes . manyTill symbol
  where symbol = (Just <$> instruction) <|> (Nothing <$ fluffChar)

instruction :: Parsec String () BF.Instruction
instruction =
  (BF.MoveLeft <$ char '<')
    <|> (BF.MoveRight <$ char '>')
    <|> (BF.AddOne <$ char '+')
    <|> (BF.SubtractOne <$ char '-')
    <|> (BF.WriteChar <$ char '.')
    <|> (BF.ReadChar <$ char ',')
    <|> (BF.Loop <$> loop)

fluffChar :: Parsec String () Char
fluffChar = noneOf "<>+-.,[]"
