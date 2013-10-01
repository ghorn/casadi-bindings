{-# OPTIONS_GHC -Wall #-}
{-# Language ForeignFunctionInterface #-}
{-# Language MultiParamTypeClasses #-}
{-# Language TypeSynonymInstances #-}
{-# Language FlexibleInstances #-}
{-# Language FlexibleContexts #-}

module Casadi.WrapReturn ( WrapReturn(..)
                         ) where

import Control.Monad ( zipWithM )
import qualified Data.Vector as V
import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr ( Ptr )
import Foreign.Storable ( Storable )
import Foreign.ForeignPtr ( ForeignPtr, newForeignPtr_, withForeignPtr )
import Foreign.Marshal ( mallocArray, free, peekArray, withArray )

import Casadi.MarshalTypes

class WrapReturn a b where
  wrapReturn :: a -> IO b
instance WrapReturn a a where
  wrapReturn = return
instance WrapReturn CInt Int where
  wrapReturn = return . fromIntegral
instance WrapReturn CDouble Double where
  wrapReturn = return . realToFrac
instance WrapReturn CLong Int where
  wrapReturn = return . fromIntegral

foreign import ccall unsafe "hs_read_bool" c_readBool
  :: Ptr CppBool' -> IO CInt
instance WrapReturn (ForeignPtr CppBool') Bool where
  wrapReturn boolPtr' = withForeignPtr boolPtr' $ \boolPtr -> do
    ret <- c_readBool boolPtr
    return $ case ret of 0 -> False
                         _ -> True
    

foreign import ccall unsafe "hs_string_length" c_stringLength
  :: Ptr StdString' -> IO CInt
foreign import ccall unsafe "hs_string_copy" c_stringCopy
  :: Ptr StdString' -> Ptr CChar -> IO ()
instance WrapReturn (ForeignPtr StdString') String where
  wrapReturn stdStr' = withForeignPtr stdStr' $ \stdStr -> do
    len <- fmap fromIntegral $ c_stringLength stdStr
    cstring <- mallocArray (len + 1)
    c_stringCopy stdStr cstring
    ret <- peekCString cstring
    free cstring
    return ret

wrapReturnVec ::
  Storable a =>
  (Ptr (CppVec a) -> IO CInt) ->
  (Ptr (CppVec a) -> Ptr a -> IO ()) ->
  (a -> IO b) ->
  ForeignPtr (CppVec a) -> IO (V.Vector b)
wrapReturnVec vecSize vecCopy cToHs vecPtr' = withForeignPtr vecPtr' $ \vecPtr -> do
  n <- fmap fromIntegral (vecSize vecPtr)
  arr <- mallocArray n
  vecCopy vecPtr arr
  ret <- peekArray n arr
  free arr
  fmap V.fromList (mapM cToHs ret)

foreign import ccall unsafe "hs_vec_size_int" c_vecSizeCInt
  :: Ptr (CppVec CInt) -> IO CInt
foreign import ccall unsafe "hs_vec_copy_int" c_vecCopyCInt
  :: Ptr (CppVec CInt) -> Ptr CInt -> IO ()
foreign import ccall unsafe "hs_vec_size_double" c_vecSizeCDouble
  :: Ptr (CppVec CDouble) -> IO CInt
foreign import ccall unsafe "hs_vec_copy_double" c_vecCopyCDouble
  :: Ptr (CppVec CDouble) -> Ptr CDouble -> IO ()

instance WrapReturn (ForeignPtr (CppVec CInt)) (V.Vector Int) where
  wrapReturn = wrapReturnVec c_vecSizeCInt c_vecCopyCInt (return . fromIntegral)

instance WrapReturn (ForeignPtr (CppVec CDouble)) (V.Vector Double) where
  wrapReturn = wrapReturnVec c_vecSizeCDouble c_vecCopyCDouble (return . realToFrac)

foreign import ccall unsafe "hs_vec_size_voidp" c_vecSizeVoidP
  :: Ptr (CppVec (Ptr a)) -> IO CInt
foreign import ccall unsafe "hs_vec_copy_voidp" c_vecCopyVoidP
  :: Ptr (CppVec (Ptr a)) -> Ptr (Ptr a) -> IO ()
instance WrapReturn (ForeignPtr a) b =>
         WrapReturn (ForeignPtr (CppVec (Ptr a))) (V.Vector b) where
  wrapReturn = wrapReturnVec c_vecSizeVoidP c_vecCopyVoidP ((>>= wrapReturn) . newForeignPtr_)

wrapReturnVecVec
  :: Storable a =>
     (Ptr (CppVecVec a) -> IO CInt) ->
     (Ptr (CppVecVec a) -> Ptr CInt -> IO ()) ->
     (Ptr (CppVecVec a) -> Ptr (Ptr a) -> IO ()) ->
     (a -> IO b) ->
     (ForeignPtr (CppVecVec a)) -> IO (V.Vector (V.Vector b))
wrapReturnVecVec vecVecSize vecVecSizes vecVecCopy cToHs foreignVecPtr = withForeignPtr foreignVecPtr $ \vecPtr -> do
  outerLength <- fmap fromIntegral $ vecVecSize vecPtr
  innerLengthArray <- mallocArray outerLength
  vecVecSizes vecPtr innerLengthArray
  innerLengths <- fmap (map fromIntegral) $ peekArray outerLength innerLengthArray
  free innerLengthArray

  outputs <- mapM mallocArray innerLengths
  withArray outputs $ \outputArray ->
    vecVecCopy vecPtr outputArray
  outputLists <- zipWithM peekArray innerLengths outputs
  mapM_ free outputs
  let f = fmap V.fromList . mapM cToHs
  fmap V.fromList $ mapM f outputLists

foreign import ccall unsafe "hs_vec_vec_size_voidp" c_vecVecSizeVoidP
  :: Ptr (CppVecVec (Ptr a)) -> IO CInt
foreign import ccall unsafe "hs_vec_vec_sizes_voidp" c_vecVecSizesVoidP
  :: Ptr (CppVecVec (Ptr a)) -> Ptr CInt -> IO ()
foreign import ccall unsafe "hs_vec_vec_copy_voidp" c_vecVecCopyVoidP
  :: Ptr (CppVecVec (Ptr a)) -> Ptr (Ptr (Ptr a)) -> IO ()

instance WrapReturn (ForeignPtr a) b => WrapReturn (ForeignPtr (CppVecVec (Ptr a))) (V.Vector (V.Vector b)) where
  wrapReturn =
    wrapReturnVecVec c_vecVecSizeVoidP c_vecVecSizesVoidP c_vecVecCopyVoidP
    ((>>= wrapReturn) . newForeignPtr_)

foreign import ccall unsafe "hs_vec_vec_size_int" c_vecVecSizeInt
  :: Ptr (CppVecVec CInt) -> IO CInt
foreign import ccall unsafe "hs_vec_vec_sizes_int" c_vecVecSizesInt
  :: Ptr (CppVecVec CInt) -> Ptr CInt -> IO ()
foreign import ccall unsafe "hs_vec_vec_copy_int" c_vecVecCopyInt
  :: Ptr (CppVecVec CInt) -> Ptr (Ptr CInt) -> IO ()

instance WrapReturn (ForeignPtr (CppVecVec CInt)) (V.Vector (V.Vector Int)) where
  wrapReturn = wrapReturnVecVec c_vecVecSizeInt c_vecVecSizesInt c_vecVecCopyInt (return . fromIntegral)
