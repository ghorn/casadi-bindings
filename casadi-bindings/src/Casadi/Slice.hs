{-# OPTIONS_GHC -Wall -fno-cse -fno-warn-orphans #-}

module Casadi.Slice
       ( Slice, slice, slice'
       ) where

import System.IO.Unsafe ( unsafePerformIO )

import Casadi.Core.Classes.Slice

instance Show Slice where
  show x = unsafePerformIO (slice_get_str__0 x)
  {-# NOINLINE show #-}

-- | slice start stop step
slice :: Int -> Int -> Int -> Slice
slice x y z = unsafePerformIO (slice__1 x y z)
{-# NOINLINE slice #-}

-- | Slice()
slice' :: Slice
slice' = unsafePerformIO slice__10
{-# NOINLINE slice' #-}
