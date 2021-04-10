{-# LANGUAGE FlexibleInstances #-}

module BF.Zipper
  ( Zipper(..)
  , left
  , right
  , alter
  , focus
  , fromList
  , new
  )
where

import           Data.Foldable                  ( toList )
import           Data.Maybe                     ( fromJust )
import           Data.Sequence                  ( Seq(..)
                                                , (<|)
                                                , (|>)
                                                )
import qualified Data.Sequence                 as Seq
import           Data.Word                      ( Word8 )

data Zipper a = Zipper (Seq a) a (Seq a)

instance Show (Zipper Word8) where
  show (Zipper seql x seqr) = show ls ++ ' ' : show x ++ ' ' : show rs
   where
    ls = toList seql
    rs = toList . Seq.dropWhileR (== 0) $ seqr

new :: Int -> Zipper Word8
new x = fromJust . fromList $ replicate x 0

left :: Zipper a -> Zipper a
left z@(Zipper Empty      _ Empty     ) = z
left (  Zipper Empty      x (rs :|> r)) = Zipper (x <| rs) r Empty
left (  Zipper (ls :|> l) x rs        ) = Zipper ls l (x <| rs)

right :: Zipper a -> Zipper a
right z@(Zipper Empty      _ Empty     ) = z
right (  Zipper (l :<| ls) x Empty     ) = Zipper Empty l (ls |> x)
right (  Zipper ls         x (r :<| rs)) = Zipper (ls |> x) r rs

alter :: (a -> a) -> Zipper a -> Zipper a
alter f (Zipper l x r) = Zipper l (f x) r

focus :: Zipper a -> a
focus (Zipper _ x _) = x

fromList :: [a] -> Maybe (Zipper a)
fromList = fromSeq . Seq.fromList

fromSeq :: Seq a -> Maybe (Zipper a)
fromSeq Empty      = Nothing
fromSeq (x :<| xs) = Just $ Zipper Empty x xs
