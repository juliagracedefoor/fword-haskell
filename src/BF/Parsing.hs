module BF.Parsing
  ( parseFile
  , parseString
  , peval
  , pexec
  )
where

import           Text.Parsec             hiding ( parse )
import qualified Text.Parsec                   as Parsec
import           Control.Monad                  ( (<=<) )
import qualified BF
import           Data.Maybe                     ( catMaybes )

-- peval == "parse and evaluate"
peval :: String -> IO ()
peval = either print BF.eval . parseString ""

-- pexec == "parse and execute"
pexec :: String -> IO ()
pexec = either print (print <=< BF.exec) . parseString ""

parseFile :: String -> IO (Either ParseError [BF.Instruction])
parseFile name = parseString name <$> readFile name

parseString :: String -> String -> Either ParseError [BF.Instruction]
parseString = Parsec.parse (instructionsTill eof)

type Parser a = Parsec String () a

instructionsTill :: Parser end -> Parser [BF.Instruction]
instructionsTill = fmap catMaybes . manyTill p
  where p = (Just <$> instruction) <|> (Nothing <$ fluffChar)

instruction :: Parser BF.Instruction
instruction =
  (BF.MoveLeft <$ char '<')
    <|> (BF.MoveRight <$ char '>')
    <|> (BF.AddOne <$ char '+')
    <|> (BF.SubtractOne <$ char '-')
    <|> (BF.WriteChar <$ char '.')
    <|> (BF.ReadChar <$ char ',')
    <|> (BF.Loop <$> loop)

loop :: Parser [BF.Instruction]
loop = between (char '[') (char ']') innards
  where innards = instructionsTill $ lookAhead (char ']')

-- a character to ignore
fluffChar :: Parser Char
fluffChar = noneOf "<>+-.,[]"
