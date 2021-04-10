module BF
  ( Symbol(..)
  , TapeMachine
  , characters
  , eval
  , exec
  , runInstructionsOn
  , newTape
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

type BF a = StateT TapeMachine IO a
type TapeMachine = Zipper Word8

data Symbol = Instructions String | Loop [Symbol]

instance Show Symbol where
  show (Instructions s ) = "I " ++ "|" ++ s ++ "|"
  show (Loop         xs) = "L " ++ show xs

characters :: [Char]
characters = "<>+-.,[]"

eval :: [Symbol] -> IO ()
eval = void . exec

exec :: [Symbol] -> IO TapeMachine
exec = runInstructionsOn newTape

runInstructionsOn :: TapeMachine -> [Symbol] -> IO TapeMachine
runInstructionsOn = flip $ execStateT . mapM_ processSymbol

newTape :: TapeMachine
newTape = Zipper.new 30000

processSymbol :: Symbol -> BF ()
processSymbol (Instructions cs) = mapM_ findIntent cs
processSymbol l@(Loop innards) =
  gets Zipper.focus >>= (\x -> unless (x == 0) $ loopOnce >> processSymbol l)
  where loopOnce = mapM_ processSymbol innards

findIntent :: Char -> BF ()
findIntent '>' = modify Zipper.right
findIntent '<' = modify Zipper.left
findIntent '+' = modify $ Zipper.alter (+ 1)
findIntent '-' = modify $ Zipper.alter (subtract 1)
findIntent '.' = gets Zipper.focus >>= liftIO . writeC
findIntent ',' = liftIO readC >>= modify . Zipper.alter . maybe id const
findIntent _   = return ()

writeC :: Word8 -> IO ()
writeC x = do
  let c = chr . fromIntegral $ x
  putChar c
  hFlush stdout

readC :: IO (Maybe Word8)
readC = do
  eof <- isEOF
  if eof then return Nothing else fmap (Just . fromIntegral . ord) getChar
