{-# OPTIONS_GHC -Wall -fno-cse -fno-warn-orphans #-}

module Casadi.Sparsity
       ( Sparsity
       , upper, lower, spy, spy_matlab
       , dense, sparse, scalar
       , compress, compressed
       , get_row, get_col
       ) where

import qualified Data.Vector as V
import System.IO.Unsafe ( unsafePerformIO )
import qualified Data.Serialize as S
import qualified Data.Binary as B
import Data.Vector.Binary () -- instances

import Casadi.Core.Classes.Sparsity

import Casadi.SharedObject ( castSharedObject )

putWith :: (V.Vector Int -> m ()) -> Sparsity -> m ()
putWith putVector = putVector . compress

getWith :: Functor f => f (V.Vector Int) -> f Sparsity
getWith getVector = fmap compressed getVector

-- Data.Vector.Cereal looks deprecated, it's not in master anymore
instance S.Serialize Sparsity where
  put = putWith (S.put . V.toList)
  get = getWith (fmap V.fromList S.get)
instance B.Binary Sparsity where
  put = putWith B.put
  get = getWith B.get

instance Show Sparsity where
  show x = show (castSharedObject x)
  {-# NOINLINE show #-}

instance Eq Sparsity where
  x == y = unsafePerformIO (sparsity_is_equal__1 x y)
  {-# NOINLINE (==) #-}

upper :: Int -> Sparsity
upper k = unsafePerformIO (sparsity_upper k)
{-# NOINLINE upper #-}

lower :: Int -> Sparsity
lower k = unsafePerformIO (sparsity_lower k)
{-# NOINLINE lower #-}

spy :: Sparsity -> IO ()
spy = sparsity_spy

spy_matlab :: Sparsity -> String -> IO ()
spy_matlab = sparsity_spy_matlab

scalar :: Sparsity
scalar = unsafePerformIO sparsity_scalar__0
{-# NOINLINE scalar #-}

get_row :: Sparsity -> V.Vector Int
get_row s = unsafePerformIO (sparsity_get_row s)
{-# NOINLINE get_row #-}

get_col :: Sparsity -> V.Vector Int
get_col s = unsafePerformIO (sparsity_get_col s)
{-# NOINLINE get_col #-}

sparse :: Int -> Int -> V.Vector Int -> V.Vector Int -> Sparsity
sparse nr nc r c = unsafePerformIO (sparsity__1 nr nc r c)
{-# NOINLINE sparse #-}

dense :: Int -> Int -> Sparsity
dense nr nc = unsafePerformIO (sparsity_dense__2 nr nc)
{-# NOINLINE dense #-}

compress :: Sparsity -> V.Vector Int
compress s = unsafePerformIO (sparsity_compress s)
{-# NOINLINE compress #-}

compressed :: V.Vector Int -> Sparsity
compressed v = unsafePerformIO (sparsity_compressed v)
{-# NOINLINE compressed #-}
