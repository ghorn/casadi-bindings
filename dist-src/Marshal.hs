{-# OPTIONS_GHC -Wall #-}
{-# Language ForeignFunctionInterface #-}
{-# Language MultiParamTypeClasses #-}
-- {-# Language FunctionalDependencies #-}
{-# Language TypeSynonymInstances #-}
{-# Language FlexibleInstances #-}

module Marshal ( ForeignPtrWrapper(..)
               , Marshal(..)
               , WrapReturn(..)
               , CppVec
               , CppVecVec
               , CppVecVecVec
               ) where

import qualified Data.Vector as V
import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr ( Ptr )
import Foreign.ForeignPtr ( ForeignPtr, withForeignPtr, touchForeignPtr )
import Foreign.ForeignPtr.Unsafe ( unsafeForeignPtrToPtr )
import Foreign.Marshal -- ( mallocArray )

class Marshal a b where
  withMarshal :: a -> (b -> IO c) -> IO c

instance Marshal Int CInt where
  withMarshal x f = f (fromIntegral x)

instance Marshal String CString where
  withMarshal = withCString

instance Marshal (ForeignPtr a) (Ptr a) where
  withMarshal = withForeignPtr

class ForeignPtrWrapper a b where
  unwrapForeignPtr :: a -> ForeignPtr b

withForeignPtrs :: [ForeignPtr a] -> ([Ptr a] -> IO b) -> IO b
withForeignPtrs xs f = do
  ret <- f (map unsafeForeignPtrToPtr xs)
  mapM_ touchForeignPtr xs
  return ret

foreign import ccall unsafe "hs_delete_vec" c_hsDeleteVec
  :: Ptr (CppVec (Ptr a)) -> IO ()
foreign import ccall unsafe "hs_marshal_vec" c_hsMarshalVec
  :: Ptr (Ptr a) -> CInt -> IO (Ptr (CppVec (Ptr a)))

instance ForeignPtrWrapper a b => Marshal (V.Vector a) (Ptr (CppVec (Ptr b))) where
  withMarshal vec f = do
    let foreignPtrList = map unwrapForeignPtr (V.toList vec)
    withForeignPtrs foreignPtrList $ \ptrList -> do
      withArrayLen ptrList $ \num ptrArray -> do
        ptrCppVec <- c_hsMarshalVec ptrArray (fromIntegral num)
        ret <- f ptrCppVec
        c_hsDeleteVec ptrCppVec
        return ret

data CppVec a
data CppVecVec a
data CppVecVecVec a

class WrapReturn a b where
    wrapReturn :: a -> IO b

foreign import ccall unsafe "vec_size" c_vecSize
  :: Ptr (CppVec CInt) -> IO CInt
foreign import ccall unsafe "hs_unmarshal_vec" c_hsUnmarshalVec
  :: Ptr (CppVec CInt) -> Ptr CInt -> IO ()
instance WrapReturn (ForeignPtr (CppVec CInt)) (V.Vector Int) where
  wrapReturn vecPtr' = withForeignPtr vecPtr' $ \vecPtr -> do
    n <- fmap fromIntegral (c_vecSize vecPtr)
    arr <- mallocArray n
    c_hsUnmarshalVec vecPtr arr
    ret <- peekArray n arr
    free arr
    return (V.fromList (map fromIntegral ret))
