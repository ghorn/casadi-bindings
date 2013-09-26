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
               , StdString'
               , StdOstream'
               ) where

import qualified Data.Vector as V
import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr ( Ptr )
import Foreign.ForeignPtr ( ForeignPtr, withForeignPtr, touchForeignPtr )
import Foreign.ForeignPtr.Unsafe ( unsafeForeignPtrToPtr )
import Foreign.Marshal -- ( mallocArray )
import Foreign.Storable ( Storable )

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

foreign import ccall unsafe "hs_delete_vec_void_ptrs" c_hsDeleteVecVoidPtrs
  :: Ptr (CppVec (Ptr a)) -> IO ()
foreign import ccall unsafe "hs_marshal_vec_void_ptrs" c_hsMarshalVecVoidPtrs
  :: Ptr (Ptr a) -> CInt -> IO (Ptr (CppVec (Ptr a)))

instance ForeignPtrWrapper a b => Marshal (V.Vector a) (Ptr (CppVec (Ptr b))) where
  withMarshal vec f = do
    let foreignPtrList = map unwrapForeignPtr (V.toList vec)
    withForeignPtrs foreignPtrList $ \ptrList -> do
      withArrayLen ptrList $ \num ptrArray -> do
        ptrCppVec <- c_hsMarshalVecVoidPtrs ptrArray (fromIntegral num)
        ret <- f ptrCppVec
        c_hsDeleteVecVoidPtrs ptrCppVec
        return ret

foreign import ccall unsafe "hs_marshal_vec_uchar" c_hsMarshalVecUChar
  :: Ptr CUChar -> CInt -> IO (Ptr (CppVec CUChar))
foreign import ccall unsafe "hs_delete_vec_uchar" c_hsDeleteVecUChar
  :: Ptr (CppVec CUChar) -> IO ()

foreign import ccall unsafe "hs_marshal_vec_double" c_hsMarshalVecDouble
  :: Ptr CDouble -> CInt -> IO (Ptr (CppVec CDouble))
foreign import ccall unsafe "hs_delete_vec_double" c_hsDeleteVecDouble
  :: Ptr (CppVec CDouble) -> IO ()

foreign import ccall unsafe "hs_marshal_vec_int" c_hsMarshalVecInt
  :: Ptr CInt -> CInt -> IO (Ptr (CppVec CInt))
foreign import ccall unsafe "hs_delete_vec_int" c_hsDeleteVecInt
  :: Ptr (CppVec CInt) -> IO ()

foreign import ccall unsafe "hs_marshal_vec_size_t" c_hsMarshalVecSize
  :: Ptr CSize -> CInt -> IO (Ptr (CppVec CSize))
foreign import ccall unsafe "hs_delete_vec_size_t" c_hsDeleteVecSize
  :: Ptr (CppVec CSize) -> IO ()

class Storable a => CppVectorize a where
  hsMarshalVec :: Ptr a -> CInt -> IO (Ptr (CppVec a))
  hsDeleteVec :: Ptr (CppVec a) -> IO ()

instance CppVectorize CUChar where
  hsMarshalVec = c_hsMarshalVecUChar
  hsDeleteVec = c_hsDeleteVecUChar

instance CppVectorize CDouble where
  hsMarshalVec = c_hsMarshalVecDouble
  hsDeleteVec = c_hsDeleteVecDouble

instance CppVectorize CInt where
  hsMarshalVec = c_hsMarshalVecInt
  hsDeleteVec = c_hsDeleteVecInt

instance CppVectorize CSize where
  hsMarshalVec = c_hsMarshalVecSize
  hsDeleteVec = c_hsDeleteVecSize

class HsToC a b where
  hsToC :: a -> b
instance HsToC Bool CInt where
  hsToC True = 1
  hsToC False = 0
instance HsToC Double CDouble where
  hsToC = realToFrac

instance HsToC CUChar CUChar where
  hsToC = id

instance (HsToC a b, CppVectorize b) => Marshal (V.Vector a) (Ptr (CppVec b)) where
  withMarshal vec f = do
    withArrayLen (map hsToC (V.toList vec)) $ \num array -> do
      ptrCppVec <- hsMarshalVec array (fromIntegral num)
      ret <- f ptrCppVec
      hsDeleteVec ptrCppVec
      return ret

data CppVec a
data CppVecVec a
data CppVecVecVec a

data StdString'
data StdOstream'

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
