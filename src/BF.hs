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
import           Control.Arrow                  ( (>>>) )
import           Control.Monad.State
import           Data.Word                      ( Word8 )

type TapeMachine = Zipper Word8

data Instruction = MoveLeft | MoveRight | AddOne | SubtractOne | WriteChar | ReadChar | Loop [Instruction] deriving (Show)

eval :: [Instruction] -> IO ()
eval = void . exec

exec :: [Instruction] -> IO TapeMachine
exec = runInstructionsOn $ Zipper.new 30000

runInstructionsOn :: TapeMachine -> [Instruction] -> IO TapeMachine
runInstructionsOn tm xs = execStateT (mapM_ perform xs) tm

perform :: Instruction -> StateT TapeMachine IO ()
perform MoveLeft    = modify Zipper.left
perform MoveRight   = modify Zipper.right
perform AddOne      = modify $ Zipper.alter (+ 1)
perform SubtractOne = modify $ Zipper.alter (subtract 1)
perform WriteChar   = liftIO . writeChar =<< gets Zipper.focus
perform ReadChar    = modify . Zipper.alter . maybe id const =<< liftIO readChar
perform (Loop xs)   = loop =<< gets Zipper.focus
 where
  loop x = unless (x == 0) $ do
    mapM_ perform xs
    perform (Loop xs)
