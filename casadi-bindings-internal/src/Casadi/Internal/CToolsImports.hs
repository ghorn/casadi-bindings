{-# OPTIONS_GHC -Wall #-}
{-# Language ForeignFunctionInterface #-}

module Casadi.Internal.CToolsImports where

import Foreign.C.Types
import Foreign.Ptr ( Ptr )

import Casadi.Internal.MarshalTypes

--------------------------- ("int","CInt",Nothing,["Int","Bool"]) -----------------------------------
foreign import ccall unsafe "hs_new_vec_int" c_newVecCInt
  :: Ptr CInt -> CInt -> IO (Ptr (StdVec CInt))
foreign import ccall unsafe "hs_delete_vec_int" c_deleteVecCInt
  :: Ptr (StdVec CInt) -> IO ()
foreign import ccall unsafe "hs_copy_vec_int" c_copyVecCInt
  :: Ptr (StdVec CInt) -> Ptr CInt -> IO ()
foreign import ccall unsafe "hs_size_vec_int" c_sizeVecCInt
  :: Ptr (StdVec CInt) -> IO CInt

--------------------------- ("voidp","VoidP",Just "(Ptr a)",[]) -----------------------------------
foreign import ccall unsafe "hs_new_vec_voidp" c_newVecVoidP
  :: Ptr (Ptr a) -> CInt -> IO (Ptr (StdVec (Ptr a)))
foreign import ccall unsafe "hs_delete_vec_voidp" c_deleteVecVoidP
  :: Ptr (StdVec (Ptr a)) -> IO ()
foreign import ccall unsafe "hs_copy_vec_voidp" c_copyVecVoidP
  :: Ptr (StdVec (Ptr a)) -> Ptr (Ptr a) -> IO ()
foreign import ccall unsafe "hs_size_vec_voidp" c_sizeVecVoidP
  :: Ptr (StdVec (Ptr a)) -> IO CInt

--------------------------- ("uchar","CUChar",Nothing,[]) -----------------------------------
foreign import ccall unsafe "hs_new_vec_uchar" c_newVecCUChar
  :: Ptr CUChar -> CInt -> IO (Ptr (StdVec CUChar))
foreign import ccall unsafe "hs_delete_vec_uchar" c_deleteVecCUChar
  :: Ptr (StdVec CUChar) -> IO ()
foreign import ccall unsafe "hs_copy_vec_uchar" c_copyVecCUChar
  :: Ptr (StdVec CUChar) -> Ptr CUChar -> IO ()
foreign import ccall unsafe "hs_size_vec_uchar" c_sizeVecCUChar
  :: Ptr (StdVec CUChar) -> IO CInt

--------------------------- ("double","CDouble",Nothing,["Double"]) -----------------------------------
foreign import ccall unsafe "hs_new_vec_double" c_newVecCDouble
  :: Ptr CDouble -> CInt -> IO (Ptr (StdVec CDouble))
foreign import ccall unsafe "hs_delete_vec_double" c_deleteVecCDouble
  :: Ptr (StdVec CDouble) -> IO ()
foreign import ccall unsafe "hs_copy_vec_double" c_copyVecCDouble
  :: Ptr (StdVec CDouble) -> Ptr CDouble -> IO ()
foreign import ccall unsafe "hs_size_vec_double" c_sizeVecCDouble
  :: Ptr (StdVec CDouble) -> IO CInt
