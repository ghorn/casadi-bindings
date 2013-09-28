{-# OPTIONS_GHC -Wall #-}
{-# Language ForeignFunctionInterface #-}
{-# Language MultiParamTypeClasses #-}
-- {-# Language FunctionalDependencies #-}
{-# Language TypeSynonymInstances #-}
{-# Language FlexibleInstances #-}
{-# Language FlexibleContexts #-}

module Casadi.Wrappers.WrapReturn ( WrapReturn(..)
                                  ) where

import Control.Monad ( zipWithM )
import qualified Data.Vector as V
import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr ( Ptr )
import Foreign.ForeignPtr ( ForeignPtr, newForeignPtr_, withForeignPtr )
import Foreign.Marshal ( mallocArray, free, peekArray, withArray )

import Casadi.Wrappers.MarshalTypes

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


foreign import ccall unsafe "hs_vec_size_int" c_vecSizeCInt
  :: Ptr (CppVec CInt) -> IO CInt
foreign import ccall unsafe "hs_vec_copy_int" c_vecCopyCInt
  :: Ptr (CppVec CInt) -> Ptr CInt -> IO ()
foreign import ccall unsafe "hs_vec_size_double" c_vecSizeCDouble
  :: Ptr (CppVec CDouble) -> IO CInt
foreign import ccall unsafe "hs_vec_copy_double" c_vecCopyCDouble
  :: Ptr (CppVec CDouble) -> Ptr CDouble -> IO ()
instance WrapReturn (ForeignPtr (CppVec CInt)) (V.Vector Int) where
  wrapReturn vecPtr' = withForeignPtr vecPtr' $ \vecPtr -> do
    n <- fmap fromIntegral (c_vecSizeCInt vecPtr)
    arr <- mallocArray n
    c_vecCopyCInt vecPtr arr
    ret <- peekArray n arr
    free arr
    return (V.fromList (map fromIntegral ret))

instance WrapReturn (ForeignPtr (CppVec CDouble)) (V.Vector Double) where
  wrapReturn vecPtr' = withForeignPtr vecPtr' $ \vecPtr -> do
    n <- fmap fromIntegral (c_vecSizeCDouble vecPtr)
    arr <- mallocArray n
    c_vecCopyCDouble vecPtr arr
    ret <- peekArray n arr
    free arr
    return (V.fromList (map realToFrac ret))


foreign import ccall unsafe "hs_vec_size_voidp" c_vecSizeVoidP
  :: Ptr (CppVec (Ptr a)) -> IO CInt
foreign import ccall unsafe "hs_vec_copy_voidp" c_vecCopyVoidP
  :: Ptr (CppVec (Ptr a)) -> Ptr (Ptr a) -> IO ()
instance WrapReturn (ForeignPtr a) b =>
         WrapReturn (ForeignPtr (CppVec (Ptr a))) (V.Vector b) where
  wrapReturn foreignVecPtr = withForeignPtr foreignVecPtr $ \vecPtr -> do
    len <- fmap fromIntegral $ c_vecSizeVoidP vecPtr
    ptrArray <- mallocArray len
    c_vecCopyVoidP vecPtr ptrArray
    ptrList <- peekArray len ptrArray -- :: IO [Ptr StdString']
    free ptrArray
    foreignPtrList <- mapM newForeignPtr_ ptrList -- :: IO [ForeignPtr StdString']
    fmap V.fromList $ mapM wrapReturn foreignPtrList

foreign import ccall unsafe "hs_vec_vec_voidp_size" c_vecVecVoidPSize
  :: Ptr (CppVecVec (Ptr a)) -> IO CInt
foreign import ccall unsafe "hs_vec_vec_voidp_sizes" c_vecVecVoidPSizes
  :: Ptr (CppVecVec (Ptr a)) -> Ptr CInt -> IO ()
foreign import ccall unsafe "hs_vec_vec_voidp_copy" c_vecVecVoidPCopy
  :: Ptr (CppVecVec (Ptr a)) -> Ptr (Ptr (Ptr a)) -> IO ()
instance WrapReturn (ForeignPtr a) b =>
         WrapReturn (ForeignPtr (CppVecVec (Ptr a))) (V.Vector (V.Vector b)) where
  wrapReturn foreignVecPtr = withForeignPtr foreignVecPtr $ \vecPtr -> do
    outerLength <- fmap fromIntegral $ c_vecVecVoidPSize vecPtr
    innerLengthArray <- mallocArray outerLength
    c_vecVecVoidPSizes vecPtr innerLengthArray
    innerLengths <- fmap (map fromIntegral) $ peekArray outerLength innerLengthArray
    free innerLengthArray

    outputs <- mapM mallocArray innerLengths -- :: IO [Ptr (Ptr a)]
    withArray outputs $ \outputArray ->
      c_vecVecVoidPCopy vecPtr outputArray
    outputLists <- zipWithM peekArray innerLengths outputs -- :: IO [[Ptr a]]
    mapM_ free outputs
    let f ptrList = do
          foreignPtrList <- mapM newForeignPtr_ ptrList -- :: IO [ForeignPtr StdString']
          fmap V.fromList $ mapM wrapReturn foreignPtrList
    fmap V.fromList $ mapM f outputLists
