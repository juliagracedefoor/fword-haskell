module BF
  ( Instruction(..)
  , TapeMachine
  , eval
  , exec
  , runInstructionsOn
  )
where

import           BF.Util                        ( readChar
                                                , writeChar
                                                )
import           BF.Zipper                      ( Zipper )
import qualified BF.Zipper                     as Zipper
import           Control.Monad.State
import           Data.Word                      ( Word8 )

type BF a = StateT TapeMachine IO a

type TapeMachine = Zipper Word8

data Instruction = MoveLeft | MoveRight | AddOne | SubtractOne | WriteChar | ReadChar | Loop [Instruction]

eval :: [Instruction] -> IO ()
eval = void . exec

exec :: [Instruction] -> IO TapeMachine
exec = runInstructionsOn $ Zipper.new 30000

runInstructionsOn :: TapeMachine -> [Instruction] -> IO TapeMachine
runInstructionsOn tm xs = execStateT (mapM_ perform xs) tm

perform :: Instruction -> BF ()
perform MoveLeft    = modify Zipper.left
perform MoveRight   = modify Zipper.right
perform AddOne      = modify $ Zipper.alter (+ 1)
perform SubtractOne = modify $ Zipper.alter (subtract 1)
perform WriteChar   = gets Zipper.focus >>= liftIO . writeChar
perform ReadChar    = liftIO readChar >>= modify . Zipper.alter . maybe id const
perform l@(Loop xs) = do
  x <- gets Zipper.focus
  unless (x == 0) $ do
    mapM_ perform xs
    perform l
