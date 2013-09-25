{-# OPTIONS_GHC -Wall #-}
{-# Language ForeignFunctionInterface #-}
{-# Language MultiParamTypeClasses #-}
-- {-# Language FunctionalDependencies #-}
{-# Language TypeSynonymInstances #-}
{-# Language FlexibleInstances #-}

module Marshall ( ForeignPtrWrapper(..)
                , Marshall(..)
                ) where

import qualified Data.Vector as V
import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr ( Ptr )
import Foreign.ForeignPtr ( ForeignPtr, withForeignPtr, touchForeignPtr )
import Foreign.ForeignPtr.Unsafe ( unsafeForeignPtrToPtr )
import Foreign.Marshal -- ( mallocArray )

class Marshall a b where
  withMarshall :: a -> (b -> IO c) -> IO c

instance Marshall Int CInt where
  withMarshall x f = f (fromIntegral x)

instance Marshall String CString where
  withMarshall = withCString

instance Marshall (ForeignPtr a) (Ptr a) where
  withMarshall = withForeignPtr

class ForeignPtrWrapper a b where
  unwrapForeignPtr :: a -> ForeignPtr b

withForeignPtrs :: [ForeignPtr a] -> ([Ptr a] -> IO b) -> IO b
withForeignPtrs xs f = do
  ret <- f (map unsafeForeignPtrToPtr xs)
  mapM_ touchForeignPtr xs
  return ret

foreign import ccall unsafe "get_null_ptr" c_nullPtr :: IO (Ptr a)

instance ForeignPtrWrapper a b => Marshall (V.Vector a) (Ptr (Ptr b)) where
  withMarshall vec f = do
    nullPtr <- c_nullPtr
    let vec' = V.toList vec
        vec'' = map unwrapForeignPtr vec'
        runMe vec''' = do
          ptr <- newArray (vec''' ++ [nullPtr])
          ret <- f ptr
          free ptr
          return ret
    withForeignPtrs vec'' runMe
