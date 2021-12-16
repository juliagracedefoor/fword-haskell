module BF.Stateful
  ( eval
  )
where

import           BF                             ( Instruction(..) )
import           BF.Util                        ( writeChar
                                                , readChar
                                                )
import           Control.Monad                  ( unless
                                                , (<=<)
                                                )
import           Control.Monad.ST               ( RealWorld )
import           Data.IORef                     ( modifyIORef'
                                                , newIORef
                                                , readIORef
                                                , IORef
                                                )
import           Data.Vector.Unboxed.Mutable    ( MVector )
import qualified Data.Vector.Unboxed.Mutable   as MV
import           Data.Word                      ( Word8 )

size :: Int
size = 30000

eval :: [Instruction] -> IO ()
eval instructions = do
  cells   <- MV.replicate size (0 :: Word8)
  pointer <- newIORef (0 :: Int)
  mapM_ (perform cells pointer) instructions

perform :: MVector RealWorld Word8 -> IORef Int -> Instruction -> IO ()
-- Move Left (<)
perform _ pointer MoveLeft  = modifyIORef' pointer ((`mod` size) . subtract 1)

-- Move Right (>)
perform _ pointer MoveRight = modifyIORef' pointer ((`mod` size) . (+ 1))

-- Add One (+)
perform cells pointer AddOne =
  MV.unsafeModify cells (+ 1) <=< readIORef $ pointer

-- Subtract One (-)
perform cells pointer SubtractOne =
  MV.unsafeModify cells (subtract 1) <=< readIORef $ pointer

-- Write Character (.)
perform cells pointer WriteChar =
  writeChar <=< MV.unsafeRead cells <=< readIORef $ pointer

-- Read Character (,)
perform cells pointer ReadChar = modifyWith . maybe id const =<< readChar
  where modifyWith f = MV.unsafeModify cells f <=< readIORef $ pointer

-- Loop instructions ([])
perform cells pointer (Loop xs) =
  loop <=< MV.unsafeRead cells <=< readIORef $ pointer
 where
  loop x = unless (x == 0) $ do
    mapM_ (perform cells pointer) xs
    perform cells pointer (Loop xs)
