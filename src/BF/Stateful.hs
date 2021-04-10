module BF.Stateful where

import           BF                             ( Instruction(..) )
import           BF.Util                        ( readChar
                                                , writeChar
                                                )
import           Control.Monad                  ( unless )
import           Control.Monad.ST
import           Data.Vector.Unboxed.Mutable    ( MVector )
import qualified Data.Vector.Unboxed.Mutable   as MV
import           Data.IORef
import           Data.Word                      ( Word8 )

size :: Int
size = 30000

eval :: [Instruction] -> IO ()
eval instructions = do
  cells   <- MV.replicate size (0 :: Word8)
  pointer <- newIORef (0 :: Int)
  mapM_ (performIO cells pointer) instructions

performIO :: MVector RealWorld Word8 -> IORef Int -> Instruction -> IO ()
performIO _ pointer MoveLeft  = modifyIORef' pointer ((`mod` size) . subtract 1)
performIO _ pointer MoveRight = modifyIORef' pointer ((`mod` size) . (+) 1)
performIO cells pointer AddOne =
  readIORef pointer >>= MV.unsafeModify cells (+ 1)
performIO cells pointer SubtractOne =
  readIORef pointer >>= MV.unsafeModify cells (subtract 1)
performIO cells pointer WriteChar =
  readIORef pointer >>= MV.unsafeRead cells >>= writeChar
performIO cells pointer ReadChar = do
  i <- readIORef pointer
  c <- readChar
  let f = maybe id const c
  MV.unsafeModify cells f i
performIO cells pointer l@(Loop xs) = do
  i <- readIORef pointer
  x <- MV.unsafeRead cells i
  unless (x == 0) $ do
    mapM_ (performIO cells pointer) xs
    performIO cells pointer l
