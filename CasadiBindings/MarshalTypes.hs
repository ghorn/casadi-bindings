{-# OPTIONS_GHC -Wall #-}
{-# Language MultiParamTypeClasses #-}

module CasadiBindings.MarshalTypes ( ForeignPtrWrapper(..)
                                   , CppVec
                                   , CppVecVec
                                   , CppVecVecVec
                                   , StdString'
                                   , StdOstream'
                                   , CppBool'
                                   , withForeignPtrs
                                   ) where

import Foreign.C.Types
import Foreign.Ptr ( Ptr )
import Foreign.ForeignPtr ( ForeignPtr, touchForeignPtr )
import Foreign.ForeignPtr.Unsafe ( unsafeForeignPtrToPtr )

data CppVec a
data CppVecVec a
data CppVecVecVec a

data StdString'
data StdOstream'
data CppBool'

class HsToC a b where
  hsToC :: a -> b
instance HsToC Int CInt where
  hsToC = fromIntegral -- really should check min/max bounds here
instance HsToC Int CLong where
  hsToC = fromIntegral
instance HsToC Bool CInt where
  hsToC True = 1
  hsToC False = 0
instance HsToC Double CDouble where
  hsToC = realToFrac
instance HsToC CUChar CUChar where
  hsToC = id

class ForeignPtrWrapper a b where
  unwrapForeignPtr :: a -> ForeignPtr b

withForeignPtrs :: [ForeignPtr a] -> ([Ptr a] -> IO b) -> IO b
withForeignPtrs xs f = do
  ret <- f (map unsafeForeignPtrToPtr xs)
  mapM_ touchForeignPtr xs
  return ret
