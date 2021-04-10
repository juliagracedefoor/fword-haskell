module BF.Util where

import           Data.Word                      ( Word8 )
import           Data.Char
import           System.IO

writeChar :: Word8 -> IO ()
writeChar x = do
  let c = chr . fromIntegral $ x
  putChar c
  hFlush stdout

readChar :: IO (Maybe Word8)
readChar = do
  eof <- isEOF
  if eof then return Nothing else fmap (Just . fromIntegral . ord) getChar
