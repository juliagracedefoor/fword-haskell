module BF
  ( eval
  , exec
  , runCommandsOn
  , findIntents
  , findIntent
  , instructions
  )
where

import           Control.Monad.State
import           Data.Char                      ( ord
                                                , chr
                                                )
import           Data.Word                      ( Word8 )
import           System.IO
import           Zipper                         ( Zipper )
import qualified Zipper

type TapeMachine = Zipper Word8
type BF a = StateT TapeMachine IO a

instructions :: [Char]
instructions = "<>+-.,[]!#"

eval :: [Char] -> IO ()
eval = void . exec

exec :: [Char] -> IO TapeMachine
exec = flip runCommandsOn $ Zipper.new 30000

runCommandsOn :: [Char] -> TapeMachine -> IO TapeMachine
runCommandsOn = execStateT . findIntents . filter (`elem` instructions)

findIntents :: [Char] -> BF ()
findIntents cs = do
  let (before   , remaining) = break (== '[') cs
      (bracketed, after    ) = brackets remaining
  mapM_ findIntent before
  unless (null remaining) $ loop bracketed >> findIntents after

loop :: [Char] -> BF ()
loop cs = do
  x <- gets Zipper.focus
  unless (x == 0) $ findIntents cs >> loop cs

findIntent :: Char -> BF ()
findIntent '>' = modify Zipper.right
findIntent '<' = modify Zipper.left
findIntent '+' = modify $ Zipper.alter (+ 1)
findIntent '-' = modify $ Zipper.alter (subtract 1)
findIntent '.' = gets Zipper.focus >>= liftIO . writeC
findIntent ',' = liftIO readC >>= modify . Zipper.alter . maybe id const
findIntent '!' =
  get >>= liftIO . putStrLn . ('\n' :) . show >> void (liftIO getLine)
findIntent _ = return ()

writeC :: Word8 -> IO ()
writeC x = do
  let c = chr . fromIntegral $ x
  putChar c
  hFlush stdout

readC :: IO (Maybe Word8)
readC = do
  eof <- isEOF
  if eof then return Nothing else fmap (Just . fromIntegral . ord) getChar

brackets :: [Char] -> ([Char], [Char])
brackets xs = (safeTail l, safeTail r)
 where
  i      = bracketSplit 0 0 xs
  (l, r) = splitAt i xs

bracketSplit :: Int -> Int -> [Char] -> Int
bracketSplit _ i [] = i
bracketSplit n i (x : xs) | x == ']' && n == 1 = i
                          | x == '['           = bracketSplit (n + 1) (i + 1) xs
                          | x == ']'           = bracketSplit (n - 1) (i + 1) xs
                          | otherwise          = bracketSplit n (i + 1) xs

safeTail :: [a] -> [a]
safeTail []       = []
safeTail (x : xs) = xs
