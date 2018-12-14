{-# OPTIONS_GHC -Wall #-}
{-# Language ForeignFunctionInterface #-}

module Casadi.Internal.CToolsImports where

import Foreign.C.Types
import Foreign.Ptr ( Ptr )

import Casadi.Internal.MarshalTypes

--------------------------- stdpair -----------------------------------
foreign import ccall unsafe "hs_new_stdpair" c_newStdPair
  :: Ptr a -> Ptr b -> IO (Ptr (StdPair (Ptr a) (Ptr b)))
foreign import ccall unsafe "hs_stdpair_fst" c_stdPairFst
  :: Ptr (StdPair (Ptr a) (Ptr b)) -> IO (Ptr a)
foreign import ccall unsafe "hs_stdpair_snd" c_stdPairSnd
  :: Ptr (StdPair (Ptr a) (Ptr b)) -> IO (Ptr b)
foreign import ccall unsafe "hs_delete_stdpair" c_deleteStdPair
  :: Ptr (StdPair (Ptr a) (Ptr b)) -> IO ()

foreign import ccall unsafe "hs_new_stdpair_long_long" c_newStdPairLLong
  :: CLLong -> CLLong -> IO (Ptr (StdPair CLLong CLLong))
foreign import ccall unsafe "hs_stdpair_fst_long_long" c_stdPairFstLLong
  :: Ptr (StdPair CLLong CLLong) -> IO CLLong
foreign import ccall unsafe "hs_stdpair_snd_long_long" c_stdPairSndLLong
  :: Ptr (StdPair CLLong CLLong) -> IO CLLong
foreign import ccall unsafe "hs_delete_stdpair_long_long" c_deleteStdPairLLong
  :: Ptr (StdPair CLLong CLLong) -> IO ()

--------------------------- stdmap -----------------------------------
foreign import ccall unsafe "hs_new_dict" c_newDict
  :: Ptr (StdVec (Ptr StdString)) -> Ptr (StdVec (Ptr a)) -> IO (Ptr (StdMap StdString (Ptr a)))

foreign import ccall unsafe "hs_dict_size" c_dictSize
  :: Ptr (StdMap StdString (Ptr a)) -> IO CInt

foreign import ccall unsafe "hs_lookup_dict" c_lookupDict
  :: Ptr (StdMap StdString (Ptr a)) -> Ptr StdString -> IO (Ptr a)

foreign import ccall unsafe "hs_dict_copy" c_dictCopy
  :: Ptr (StdMap StdString (Ptr a)) -> Ptr (Ptr StdString) -> Ptr (Ptr a) -> IO ()

foreign import ccall unsafe "hs_delete_dict" c_deleteDict
  :: Ptr (StdMap StdString (Ptr a)) -> IO ()

--------------------------- ("int","CInt",Nothing,["Int","Bool"]) -----------------------------------
foreign import ccall unsafe "hs_new_vec_int" c_newVecCInt
  :: Ptr CInt -> CInt -> IO (Ptr (StdVec CInt))
foreign import ccall unsafe "hs_delete_vec_int" c_deleteVecCInt
  :: Ptr (StdVec CInt) -> IO ()
foreign import ccall unsafe "hs_copy_vec_int" c_copyVecCInt
  :: Ptr (StdVec CInt) -> Ptr CInt -> IO ()
foreign import ccall unsafe "hs_size_vec_int" c_sizeVecCInt
  :: Ptr (StdVec CInt) -> IO CInt


foreign import ccall unsafe "hs_new_vec_llong" c_newVecLLong
  :: Ptr CLLong -> CInt -> IO (Ptr (StdVec CLLong))
foreign import ccall unsafe "hs_delete_vec_llong" c_deleteVecLLong
  :: Ptr (StdVec CLLong) -> IO ()
foreign import ccall unsafe "hs_copy_vec_llong" c_copyVecCLLong
  :: Ptr (StdVec CLLong) -> Ptr CLLong -> IO ()
foreign import ccall unsafe "hs_size_vec_llong" c_sizeVecCLLong
  :: Ptr (StdVec CLLong) -> IO CInt

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
