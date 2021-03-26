module Tape
  ( Tape(..)
  , left
  , right
  , alter
  , focus
  , new
  )
where

import           Data.Word                      ( Word8 )
import           Data.Vector.Unboxed            ( Vector
                                                , (//)
                                                , (!)
                                                )
import qualified Data.Vector.Unboxed           as V

data Tape a = Tape { index :: Int, cells :: Vector a }

new :: Int -> Tape Word8
new x = Tape 0 (V.replicate x 0)

left :: V.Unbox a => Tape a -> Tape a
left (Tape i xs) = Tape i' xs
  where i' = if i <= 0 then V.length xs - 1 else i - 1

right :: V.Unbox a => Tape a -> Tape a
right (Tape i xs) = Tape i' xs
  where i' = if i >= V.length xs - 1 then 0 else i + 1

alter :: V.Unbox a => (a -> a) -> Tape a -> Tape a
alter f (Tape i xs) = Tape i (xs // [(i, f x)]) where x = xs ! i

focus :: V.Unbox a => Tape a -> a
focus (Tape i xs) = xs ! i
