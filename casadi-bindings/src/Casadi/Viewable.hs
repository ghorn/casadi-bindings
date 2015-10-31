{-# OPTIONS_GHC -Wall #-}

module Casadi.Viewable
       ( Viewable(..)
       ) where

import qualified Data.Vector as V

class Viewable a where
  vvertsplit :: a -> V.Vector Int -> V.Vector a
  vveccat :: V.Vector a -> a
  vsize1 :: a -> Int
  vsize2 :: a -> Int
  vrecoverDimension :: a -> (Int, Int) -> a

instance Viewable (V.Vector a) where
  vsize1 = V.length
  vsize2 = const 1
  vveccat = V.concat . V.toList
  vvertsplit x ks = V.fromList (split x (V.toList ks))
  -- todo(greg): this doesn't look right
  vrecoverDimension x _ = x

split :: V.Vector a -> [Int] -> [V.Vector a]
split v xs@(0:_) = split' v xs
split _ _ = error "split: first index must be 0"

split' :: V.Vector a -> [Int] -> [V.Vector a]
split' _ [] = error "can't split with no input"
split' x [kf]
  | V.length x == kf = []
  | otherwise = error "split: last index must be length of vector"
split' x (k0:k1:ks) = V.slice k0 (k1 - k0) x : split' x (k1:ks)
