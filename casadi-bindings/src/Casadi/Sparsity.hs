{-# OPTIONS_GHC -Wall -fno-cse -fno-warn-orphans #-}

module Casadi.Sparsity
       ( Sparsity
       , upper, lower, spy, spyMatlab
       , dense, sparse, scalar
       , compress, compressed
       , getRow, getCol
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

getRow :: Sparsity -> V.Vector Int
getRow s = unsafePerformIO (sparsity_row__1 s) -- todo: getRow in 2.3
{-# NOINLINE getRow #-}

getCol :: Sparsity -> V.Vector Int
getCol s = unsafePerformIO (sparsity_getCol s)
{-# NOINLINE getCol #-}

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
