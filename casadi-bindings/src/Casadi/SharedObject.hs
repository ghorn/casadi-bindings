{-# OPTIONS_GHC -Wall -fno-warn-orphans -fno-cse #-}

module Casadi.SharedObject
       ( C.castSharedObject
       ) where

import System.IO.Unsafe ( unsafePerformIO )

import qualified Casadi.Core.Classes.SharedObject as C

instance Show C.SharedObject where
  show x = unsafePerformIO (C.sharedObject_get_str__0 x)
  {-# NOINLINE show #-}
