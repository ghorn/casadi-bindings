{-# OPTIONS_GHC -Wall -fno-cse -fno-warn-orphans #-}

module Casadi.Sparsity
       ( Sparsity
       , upper, lower, spy, spyMatlab
       , dense, sparse, scalar
       , compress, compressed
       ) where

import qualified Data.Vector as V
import System.IO.Unsafe ( unsafePerformIO )
import Data.Serialize ( Serialize(..) )

import Casadi.Core.Classes.Sparsity

import Casadi.SharedObject ( castSharedObject )

instance Serialize Sparsity where
  put = put . V.toList . compress
  get = fmap (compressed . V.fromList) get

instance Show Sparsity where
  show x = show (castSharedObject x)
  {-# NOINLINE show #-}

instance Eq Sparsity where
  x == y = unsafePerformIO (sparsity_isEqual__1 x y)
  {-# NOINLINE (==) #-}

upper :: Int -> Sparsity
upper k = unsafePerformIO (sparsity_upper k)
{-# NOINLINE upper #-}

lower :: Int -> Sparsity
lower k = unsafePerformIO (sparsity_lower k)
{-# NOINLINE lower #-}

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

compress :: Sparsity -> V.Vector Int
compress s = unsafePerformIO (sparsity_compress s)
{-# NOINLINE compress #-}

compressed :: V.Vector Int -> Sparsity
compressed v = unsafePerformIO (sparsity_compressed v)
{-# NOINLINE compressed #-}
