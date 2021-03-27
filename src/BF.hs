module BF
  ( Symbol(..)
  , characters
  , eval
  , exec
  , runCommandsOn
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

data Symbol = Instructions String | Loop [Symbol]

instance Show Symbol where
  show (Instructions s ) = "I " ++ "|" ++ s ++ "|"
  show (Loop         xs) = "L " ++ show xs

characters :: [Char]
characters = "<>+-.,[]"

eval :: [Symbol] -> IO ()
eval = void . exec

exec :: [Symbol] -> IO TapeMachine
exec = flip runCommandsOn $ Zipper.new 30000

runCommandsOn :: [Symbol] -> TapeMachine -> IO TapeMachine
runCommandsOn = execStateT . mapM_ processSymbol

processSymbol :: Symbol -> BF ()
processSymbol (Instructions cs     ) = mapM_ findIntent cs
processSymbol (Loop         innards) = do
  x <- gets Zipper.focus
  unless (x == 0) $ mapM_ processSymbol innards >> processSymbol (Loop innards)

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
