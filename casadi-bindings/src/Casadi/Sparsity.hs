{-# OPTIONS_GHC -Wall -fno-cse -fno-warn-orphans #-}

module Casadi.Sparsity
       ( Sparsity
       , triu, tril, spy, spyMatlab
       , dense, sparse, scalar
       ) where

import qualified Data.Vector as V
import System.IO.Unsafe ( unsafePerformIO )

import Casadi.Core.Classes.Sparsity

import Casadi.SharedObject ( castSharedObject )

instance Show Sparsity where
  show x = show (castSharedObject x)
  {-# NOINLINE show #-}

instance Eq Sparsity where
  x == y = unsafePerformIO (sparsity_isEqual__1 x y)
  {-# NOINLINE (==) #-}

triu :: Int -> Sparsity
triu k = unsafePerformIO (sparsity_triu k)
{-# NOINLINE triu #-}

tril :: Int -> Sparsity
tril k = unsafePerformIO (sparsity_tril k)
{-# NOINLINE tril #-}

spy :: Sparsity -> IO ()
spy = sparsity_spy

spyMatlab :: Sparsity -> String -> IO ()
spyMatlab = sparsity_spyMatlab

scalar :: Sparsity
scalar = unsafePerformIO sparsity_scalar__0
{-# NOINLINE scalar #-}

sparse :: Int -> Int -> V.Vector Int -> V.Vector Int -> Sparsity
sparse nr nc r c = unsafePerformIO (sparsity__0 nr nc r c)
{-# NOINLINE sparse #-}

dense :: Int -> Int -> Sparsity
dense nr nc = unsafePerformIO (sparsity_dense__1 nr nc)
{-# NOINLINE dense #-}
