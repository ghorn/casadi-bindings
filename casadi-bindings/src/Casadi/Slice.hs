{-# OPTIONS_GHC -Wall -fno-cse -fno-warn-orphans #-}

module Casadi.Slice
       ( Slice, slice
       ) where

import Data.Vector ( Vector )
import System.IO.Unsafe ( unsafePerformIO )

import Casadi.Core.Classes.Slice

instance Show Slice where
  show x = unsafePerformIO (slice_getDescription x)
  {-# NOINLINE show #-}

-- | slice start stop step
slice :: Int -> Int -> Int -> Slice
slice x y z = unsafePerformIO (slice__3 x y z)
{-# NOINLINE slice #-}
