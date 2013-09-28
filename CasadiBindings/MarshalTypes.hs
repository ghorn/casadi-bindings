{-# OPTIONS_GHC -Wall #-}
{-# Language MultiParamTypeClasses #-}
{-# Language FunctionalDependencies #-}

module CasadiBindings.MarshalTypes ( ForeignPtrWrapper(..)
                                   , CppVec
                                   , CppVecVec
                                   , CppVecVecVec
                                   , StdString'
                                   , StdOstream'
                                   , CppBool'
                                   , withForeignPtrs
                                   , withForeignPtrsVV
                                   ) where

import qualified Data.Vector as V
import Foreign.Ptr ( Ptr )
import Foreign.ForeignPtr ( ForeignPtr, touchForeignPtr )
import Foreign.ForeignPtr.Unsafe ( unsafeForeignPtrToPtr )

data CppVec a
data CppVecVec a
data CppVecVecVec a

data StdString'
data StdOstream'
data CppBool'

class ForeignPtrWrapper a b | a -> b, b -> a where
  unwrapForeignPtr :: a -> ForeignPtr b

withForeignPtrs :: [ForeignPtr a] -> ([Ptr a] -> IO b) -> IO b
withForeignPtrs xs f = do
  ret <- f (map unsafeForeignPtrToPtr xs)
  mapM_ touchForeignPtr xs
  return ret

withForeignPtrsVV :: V.Vector (V.Vector (ForeignPtr a)) -> (V.Vector (V.Vector (Ptr a)) -> IO b) -> IO b
withForeignPtrsVV vv f = do
  ret <- f (V.map (V.map unsafeForeignPtrToPtr) vv)
  V.mapM_ (V.mapM_ touchForeignPtr) vv
  return ret
