{-# OPTIONS_GHC -Wall #-}
{-# Language ForeignFunctionInterface #-}

module CasadiBindings.ForeignTools
       ( -- * scalars
         c_newBool
       , c_deleteBool
       , c_newStdString
       , c_deleteStdString
         -- * 1-d vectors
       , c_newVec
--       , c_newVecVoidP
--       , c_newVec8
--       , c_newVec16
--       , c_newVec32
--       , c_newVec64
--
       , c_deleteVec
--       , c_deleteVecVoidP
--       , c_deleteVec8
--       , c_deleteVec16
--       , c_deleteVec32
--       , c_deleteVec64

         -- * 2-d vectors
       , c_newVVec
--       , c_newVVecVoidP
--       , c_newVVec8
--       , c_newVVec16
--       , c_newVVec32
--       , c_newVVec64

       , c_deleteVVec
--       , c_deleteVVecVoidP
--       , c_deleteVVec8
--       , c_deleteVVec16
--       , c_deleteVVec32
--       , c_deleteVVec64
       ) where

import Data.Word
import Foreign.C.Types
import Foreign.Ptr ( Ptr, castPtr )
import Foreign.Storable

import CasadiBindings.MarshalTypes

foreign import ccall unsafe "hs_new_bool" c_newBool
  :: CInt -> IO (Ptr CppBool')
foreign import ccall unsafe "hs_delete_bool" c_deleteBool
  :: Ptr CppBool' -> IO ()
foreign import ccall unsafe "hs_new_string" c_newStdString
  :: Ptr CChar -> IO (Ptr StdString')
foreign import ccall unsafe "hs_delete_string" c_deleteStdString
  :: Ptr StdString' -> IO ()

-- 1-d
--foreign import ccall unsafe "hs_new_vec_voidp" c_newVecVoidP
--  :: Ptr (Ptr a) -> CInt -> IO (Ptr (CppVec (Ptr a)))
--foreign import ccall unsafe "hs_delete_vec_voidp" c_deleteVecVoidP
--  :: Ptr (CppVec (Ptr a)) -> IO ()
foreign import ccall unsafe "hs_new_vec_8" c_newVec8
  :: Ptr Word8 -> CInt -> IO (Ptr (CppVec Word8))
foreign import ccall unsafe "hs_delete_vec_8" c_deleteVec8
  :: Ptr (CppVec Word8) -> IO ()
foreign import ccall unsafe "hs_new_vec_16" c_newVec16
  :: Ptr Word16 -> CInt -> IO (Ptr (CppVec Word16))
foreign import ccall unsafe "hs_delete_vec_16" c_deleteVec16
  :: Ptr (CppVec Word16) -> IO ()
foreign import ccall unsafe "hs_new_vec_32" c_newVec32
  :: Ptr Word32 -> CInt -> IO (Ptr (CppVec Word32))
foreign import ccall unsafe "hs_delete_vec_32" c_deleteVec32
  :: Ptr (CppVec Word32) -> IO ()
foreign import ccall unsafe "hs_new_vec_64" c_newVec64
  :: Ptr Word64 -> CInt -> IO (Ptr (CppVec Word64))
foreign import ccall unsafe "hs_delete_vec_64" c_deleteVec64
  :: Ptr (CppVec Word64) -> IO ()

-- 2-d
--foreign import ccall unsafe "hs_new_vvec_voidp" c_newVVecVoidP
--  :: Ptr (Ptr a) -> CInt -> Ptr CInt -> IO (Ptr (CppVecVec (Ptr a)))
--foreign import ccall unsafe "hs_delete_vvec_voidp" c_deleteVVecVoidP
--  :: Ptr (CppVecVec (Ptr a)) -> IO ()
foreign import ccall unsafe "hs_marshal_vvec_8" c_newVVec8
  :: Ptr Word8 -> CInt -> Ptr CInt -> IO (Ptr (CppVecVec Word8))
foreign import ccall unsafe "hs_delete_vvec_8" c_deleteVVec8
  :: Ptr (CppVecVec Word8) -> IO ()
foreign import ccall unsafe "hs_marshal_vvec_16" c_newVVec16
  :: Ptr Word16 -> CInt -> Ptr CInt -> IO (Ptr (CppVecVec Word16))
foreign import ccall unsafe "hs_delete_vvec_16" c_deleteVVec16
  :: Ptr (CppVecVec Word16) -> IO ()
foreign import ccall unsafe "hs_marshal_vvec_32" c_newVVec32
  :: Ptr Word32 -> CInt -> Ptr CInt -> IO (Ptr (CppVecVec Word32))
foreign import ccall unsafe "hs_delete_vvec_32" c_deleteVVec32
  :: Ptr (CppVecVec Word32) -> IO ()
foreign import ccall unsafe "hs_marshal_vvec_64" c_newVVec64
  :: Ptr Word64 -> CInt -> Ptr CInt -> IO (Ptr (CppVecVec Word64))
foreign import ccall unsafe "hs_delete_vvec_64" c_deleteVVec64
  :: Ptr (CppVecVec Word64) -> IO ()

c_newVec :: Storable a => Ptr a -> CInt -> IO (Ptr (CppVec a))
c_newVec x n = case sizeOf (getPtr x) of
  1 -> fmap castPtr $ c_newVec8  (castPtr x) n
  2 -> fmap castPtr $ c_newVec16 (castPtr x) n
  4 -> fmap castPtr $ c_newVec32 (castPtr x) n
  8 -> fmap castPtr $ c_newVec64 (castPtr x) n
  k -> error $ "c_newVec: sizeOf not 1,2,4,8: " ++ show k
  where
    getPtr :: Ptr a -> a
    getPtr _ = undefined

c_deleteVec :: Storable a => Ptr (CppVec a) -> IO ()
c_deleteVec x = case sizeOf (getPtr x) of
  1 -> c_deleteVec8  (castPtr x)
  2 -> c_deleteVec16 (castPtr x)
  4 -> c_deleteVec32 (castPtr x)
  8 -> c_deleteVec64 (castPtr x)
  k -> error $ "c_deleteVec: sizeOf not 1,2,4,8: " ++ show k
  where
    getPtr :: Ptr (CppVec a) -> a
    getPtr _ = undefined

c_newVVec :: Storable a => Ptr a -> CInt -> Ptr CInt -> IO (Ptr (CppVecVec a))
c_newVVec x n0 n1 = case sizeOf (getPtr x) of
  1 -> fmap castPtr $ c_newVVec8  (castPtr x) n0 n1
  2 -> fmap castPtr $ c_newVVec16 (castPtr x) n0 n1
  4 -> fmap castPtr $ c_newVVec32 (castPtr x) n0 n1
  8 -> fmap castPtr $ c_newVVec64 (castPtr x) n0 n1
  k -> error $ "c_newVVec: sizeOf not 1,2,4,8: " ++ show k
  where
    getPtr :: Ptr a -> a
    getPtr _ = undefined

c_deleteVVec :: Storable a => Ptr (CppVecVec a) -> IO ()
c_deleteVVec x = case sizeOf (getPtr x) of
  1 -> c_deleteVVec8  (castPtr x)
  2 -> c_deleteVVec16 (castPtr x)
  4 -> c_deleteVVec32 (castPtr x)
  8 -> c_deleteVVec64 (castPtr x)
  k -> error $ "c_deleteVec: sizeOf not 1,2,4,8: " ++ show k
  where
    getPtr :: Ptr (CppVecVec a) -> a
    getPtr _ = undefined
