{-# OPTIONS_GHC -Wall #-}

module Casadi.CMatrix
       ( CMatrix(..)
       , vertslice, horzslice
       ) where

import qualified Data.Vector as V

import Casadi.Core.Classes.DMatrix ( DMatrix )

import Casadi.Overloading ( Fmod, ArcTan2, SymOrd, Erf )
import Casadi.Sparsity ( Sparsity )
import Casadi.Slice ( Slice, slice )

class (Eq a, Show a, Floating a, Fmod a, ArcTan2 a, SymOrd a, Erf a) => CMatrix a where
  vertsplit :: a -> V.Vector Int -> V.Vector a
  vertcat :: V.Vector a -> a
  horzsplit :: a -> V.Vector Int -> V.Vector a
  horzcat :: V.Vector a -> a
  veccat :: V.Vector a -> a
  size1 :: a -> Int
  size2 :: a -> Int
  numel :: a -> Int
  -- | matrix matrix product
  mm :: a -> a -> a
  -- | sumAll(x*y), x and y same dimension
  innerProd :: a -> a -> a
  -- | transpose
  trans :: a -> a
  diag :: a -> a
  eye :: Int -> a
  ones :: (Int,Int) -> a
  zeros :: (Int,Int) -> a
  zerosSp :: Sparsity -> a
  solve :: a -> a -> a
  indexed :: a -> Slice -> Slice -> a
  sparsity :: a -> Sparsity
  getNZ :: a -> Slice -> a
  setNZ :: a -> a -> Slice -> IO ()
  triu :: a -> a
  tril :: a -> a
  triu2symm :: a -> a
  tril2symm :: a -> a
  copy :: a -> IO a
  dense :: a -> a
  fromDMatrix :: DMatrix -> a
  fromDVector :: V.Vector Double -> a


vertslice :: CMatrix a => a -> V.Vector Int -> V.Vector a
vertslice x vs = V.fromList (f (V.toList vs))
  where
    cols = size2 x
    hslice = slice 0 cols 1

    f (v0:v1:others) = indexed x (slice v0 v1 1) hslice : f (v1:others)
    f _ = []

horzslice :: CMatrix a => a -> V.Vector Int -> V.Vector a
horzslice x vs = V.fromList (f (V.toList vs))
  where
    rows = size1 x
    vslice = slice 0 rows 1

    f (v0:v1:others) = indexed x vslice (slice v0 v1 1) : f (v1:others)
    f _ = []