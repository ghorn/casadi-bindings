{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Casadi.Viewable
       ( Viewable(..)
       ) where

import qualified Data.Vector as V
import qualified Data.Vector.Storable as SV
import qualified Data.Vector.Unboxed as UV

class Viewable a where
  vsize1 :: a -> Int
  vsize2 :: a -> Int
  vrecoverDimension :: a -> (Int, Int) -> a
  vvertcat :: V.Vector a -> a
  vvertsplit :: a -> V.Vector Int -> V.Vector a

instance Viewable (V.Vector a) where
  vsize1 = V.length
  vsize2 = const 1
  vrecoverDimension x _ = x
  vvertcat = V.concat . V.toList
  vvertsplit x ks = V.fromList (splitV x (V.toList ks))

instance SV.Storable a => Viewable (SV.Vector a) where
  vsize1 = SV.length
  vsize2 = const 1
  vrecoverDimension x _ = x
  vvertcat = SV.concat . V.toList
  vvertsplit x ks = V.fromList (splitSV x (V.toList ks))

instance UV.Unbox a => Viewable (UV.Vector a) where
  vsize1 = UV.length
  vsize2 = const 1
  vrecoverDimension x _ = x
  vvertcat = UV.concat . V.toList
  vvertsplit x ks = V.fromList (splitUV x (V.toList ks))

splitV :: V.Vector a -> [Int] -> [V.Vector a]
splitV = split V.length V.slice

splitSV :: SV.Storable a => SV.Vector a -> [Int] -> [SV.Vector a]
splitSV = split SV.length SV.slice

splitUV :: UV.Unbox a => UV.Vector a -> [Int] -> [UV.Vector a]
splitUV = split UV.length UV.slice

split :: forall a . (a -> Int) -> (Int -> Int -> a -> a) -> a -> [Int] -> [a]
split length' slice v xs@(0:_) = split' v xs
  where
    split' :: a -> [Int] -> [a]
    split' _ [] = error "can't split with no input"
    split' x [kf]
      | length' x == kf = []
      | otherwise = error "split: last index must be length of vector"
    split' x (k0:k1:ks) = slice k0 (k1 - k0) x : split' x (k1:ks)
split _ _ _ _ = error "split: first index must be 0"
