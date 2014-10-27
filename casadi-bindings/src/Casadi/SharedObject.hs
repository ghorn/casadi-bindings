{-# OPTIONS_GHC -Wall -fno-warn-orphans -fno-cse #-}

module Casadi.SharedObject
       ( soInit
       , C.castSharedObject
       ) where

import System.IO.Unsafe ( unsafePerformIO )

import qualified Casadi.Core.Classes.SharedObject as C

soInit :: C.SharedObjectClass a => a -> IO ()
soInit = C.sharedObject_init__0

instance Show C.SharedObject where
  show x = unsafePerformIO (C.sharedObject_getDescription x)
  {-# NOINLINE show #-}
