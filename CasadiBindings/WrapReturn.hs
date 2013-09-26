{-# OPTIONS_GHC -Wall #-}
{-# Language ForeignFunctionInterface #-}
{-# Language MultiParamTypeClasses #-}
-- {-# Language FunctionalDependencies #-}
{-# Language TypeSynonymInstances #-}
{-# Language FlexibleInstances #-}
{-# Language FlexibleContexts #-}

module CasadiBindings.WrapReturn ( ForeignPtrWrapper(..)
                                 , Marshal(..)
                                 , WrapReturn(..)
                                 , CppVec
                                 , CppVecVec
                                 , CppVecVecVec
                                 , StdString'
                                 , StdOstream'
                                 , CppBool'
                                 ) where

import Control.Monad ( zipWithM )
import qualified Data.Vector as V
import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr ( Ptr )
import Foreign.ForeignPtr ( ForeignPtr, newForeignPtr_, withForeignPtr, touchForeignPtr )
import Foreign.ForeignPtr.Unsafe ( unsafeForeignPtrToPtr )
import Foreign.Marshal -- ( mallocArray )
import Foreign.Storable ( Storable )

data CppVec a
data CppVecVec a
data CppVecVecVec a

data StdString'
data StdOstream'
data CppBool'


class Marshal a b where
  withMarshal :: a -> (b -> IO c) -> IO c

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

instance Marshal Int CInt where
  withMarshal x f = f (hsToC x)
instance Marshal Int CLong where
  withMarshal x f = f (hsToC x)
instance Marshal Bool CInt where
  withMarshal x f = f (hsToC x)
instance Marshal Double CDouble where
  withMarshal x f = f (hsToC x)
instance Marshal CUChar CUChar where
  withMarshal x f = f (hsToC x)

foreign import ccall unsafe "hs_marshal_bool" c_hsMarshalBool
  :: CInt -> IO (Ptr CppBool')
foreign import ccall unsafe "hs_delete_bool" c_hsDeleteBool
  :: Ptr CppBool' -> IO ()
instance Marshal Bool (Ptr CppBool') where
  withMarshal x f = do
    let x' = case x of False -> 0
                       True -> 1
    boolPtr <- c_hsMarshalBool x'
    ret <- f boolPtr
    c_hsDeleteBool boolPtr
    return ret

instance Marshal String (Ptr CChar) where
  withMarshal = withCString

foreign import ccall unsafe "new_string" c_newStdString
  :: Ptr CChar -> IO (Ptr StdString')
foreign import ccall unsafe "delete_string" c_deleteStdString
  :: Ptr StdString' -> IO ()
instance Marshal String (Ptr StdString') where
  withMarshal str f =
    withCString str $ \cstring -> do
      stdStr <- c_newStdString cstring
      ret <- f stdStr
      c_deleteStdString stdStr
      return ret
      
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
      ptrCppVec <- withArrayLen ptrList $ \num ptrArray ->
        c_hsMarshalVecVoidPtrs ptrArray (fromIntegral num)
      ret <- f ptrCppVec
      c_hsDeleteVecVoidPtrs ptrCppVec
      return ret

foreign import ccall unsafe "hs_delete_vec_vec_void_ptrs" c_hsDeleteVecVecVoidPtrs
  :: Ptr (CppVecVec (Ptr a)) -> IO ()
foreign import ccall unsafe "hs_marshal_vec_vec_void_ptrs" c_hsMarshalVecVecVoidPtrs
  :: Ptr (Ptr a) -> CInt -> Ptr CInt -> IO (Ptr (CppVecVec (Ptr a)))

instance ForeignPtrWrapper a b => Marshal (V.Vector (V.Vector a)) (Ptr (CppVecVec (Ptr b))) where
  withMarshal vec f = do
    let innerLengths = map (fromIntegral . V.length) (V.toList vec)
        outerLength = length innerLengths
        foreignPtrList = concatMap (map unwrapForeignPtr . V.toList) (V.toList vec)
    withForeignPtrs foreignPtrList $ \ptrList -> do
      withArray ptrList $ \ptrArray -> do
        ptrCppVecVec <- withArray innerLengths $ \innerLengthsArray ->
          c_hsMarshalVecVecVoidPtrs ptrArray (fromIntegral outerLength) innerLengthsArray
        ret <- f ptrCppVecVec
        c_hsDeleteVecVecVoidPtrs ptrCppVecVec
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

instance Marshal (V.Vector CUChar) (Ptr (CppVec CUChar)) where
  withMarshal = withMarshalStorableVec c_hsMarshalVecUChar c_hsDeleteVecUChar
instance Marshal (V.Vector CDouble) (Ptr (CppVec CDouble)) where
  withMarshal = withMarshalStorableVec c_hsMarshalVecDouble c_hsDeleteVecDouble
instance Marshal (V.Vector CInt) (Ptr (CppVec CInt)) where
  withMarshal = withMarshalStorableVec c_hsMarshalVecInt c_hsDeleteVecInt
instance Marshal (V.Vector CSize) (Ptr (CppVec CSize)) where
  withMarshal = withMarshalStorableVec c_hsMarshalVecSize c_hsDeleteVecSize

withMarshalStorableVec ::
  Storable a =>
  (Ptr a -> CInt -> IO (Ptr (CppVec a))) ->
  (Ptr (CppVec a) -> IO ()) ->
  (V.Vector a) -> (Ptr (CppVec a) -> IO b) -> IO b
withMarshalStorableVec hsMarshalVec hsDeleteVec vec f = do
  withArrayLen ((V.toList vec)) $ \num array -> do
    ptrCppVec <- hsMarshalVec array (fromIntegral num)
    ret <- f ptrCppVec
    hsDeleteVec ptrCppVec
    return ret



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
    

foreign import ccall unsafe "string_length" c_stringLength
  :: Ptr StdString' -> IO CInt
foreign import ccall unsafe "string_copy" c_stringCopy
  :: Ptr StdString' -> Ptr CChar -> IO ()
instance WrapReturn (ForeignPtr StdString') String where
  wrapReturn stdStr' = withForeignPtr stdStr' $ \stdStr -> do
    len <- fmap fromIntegral $ c_stringLength stdStr
    cstring <- mallocArray (len + 1)
    c_stringCopy stdStr cstring
    ret <- peekCString cstring
    free cstring
    return ret


foreign import ccall unsafe "vec_int_size" c_vecIntSize
  :: Ptr (CppVec CInt) -> IO CInt
foreign import ccall unsafe "vec_int_copy" c_vecIntCopy
  :: Ptr (CppVec CInt) -> Ptr CInt -> IO ()
instance WrapReturn (ForeignPtr (CppVec CInt)) (V.Vector Int) where
  wrapReturn vecPtr' = withForeignPtr vecPtr' $ \vecPtr -> do
    n <- fmap fromIntegral (c_vecIntSize vecPtr)
    arr <- mallocArray n
    c_vecIntCopy vecPtr arr
    ret <- peekArray n arr
    free arr
    return (V.fromList (map fromIntegral ret))

foreign import ccall unsafe "vec_voidp_size" c_vecVoidPSize
  :: Ptr (CppVec (Ptr a)) -> IO CInt
foreign import ccall unsafe "vec_voidp_copy" c_vecVoidPCopy
  :: Ptr (CppVec (Ptr a)) -> Ptr (Ptr a) -> IO ()
instance WrapReturn (ForeignPtr a) b =>
         WrapReturn (ForeignPtr (CppVec (Ptr a))) (V.Vector b) where
  wrapReturn foreignVecPtr = withForeignPtr foreignVecPtr $ \vecPtr -> do
    len <- fmap fromIntegral $ c_vecVoidPSize vecPtr
    ptrArray <- mallocArray len
    c_vecVoidPCopy vecPtr ptrArray
    ptrList <- peekArray len ptrArray -- :: IO [Ptr StdString']
    free ptrArray
    foreignPtrList <- mapM newForeignPtr_ ptrList -- :: IO [ForeignPtr StdString']
    fmap V.fromList $ mapM wrapReturn foreignPtrList

foreign import ccall unsafe "vec_vec_voidp_size" c_vecVecVoidPSize
  :: Ptr (CppVecVec (Ptr a)) -> IO CInt
foreign import ccall unsafe "vec_vec_voidp_sizes" c_vecVecVoidPSizes
  :: Ptr (CppVecVec (Ptr a)) -> Ptr CInt -> IO ()
foreign import ccall unsafe "vec_vec_voidp_copy" c_vecVecVoidPCopy
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
