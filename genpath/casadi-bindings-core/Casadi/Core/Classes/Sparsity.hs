{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# Language ForeignFunctionInterface #-}
{-# Language FlexibleInstances #-}
{-# Language MultiParamTypeClasses #-}

module Casadi.Core.Classes.Sparsity
       (
         Sparsity,
         SparsityClass(..),
         sparsity_T,
         sparsity__0,
         sparsity__1,
         sparsity__2,
         sparsity__3,
         sparsity__4,
         sparsity_add_nz,
         sparsity_append,
         sparsity_appendColumns,
         sparsity_band,
         sparsity_banded,
         sparsity_btf__0,
         sparsity_btf__1,
         sparsity_bw_lower,
         sparsity_bw_upper,
         sparsity_colind,
         sparsity_combine,
         sparsity_compress,
         sparsity_compressed,
         sparsity_dense__0,
         sparsity_dense__1,
         sparsity_dense__2,
         sparsity_dfs,
         sparsity_diag__0,
         sparsity_diag__1,
         sparsity_diag__2,
         sparsity_dim,
         sparsity_enlargeColumns__0,
         sparsity_enlargeColumns__1,
         sparsity_enlargeRows__0,
         sparsity_enlargeRows__1,
         sparsity_enlarge__0,
         sparsity_enlarge__1,
         sparsity_erase__0,
         sparsity_erase__1,
         sparsity_erase__2,
         sparsity_erase__3,
         sparsity_etree__0,
         sparsity_etree__1,
         sparsity_find__0,
         sparsity_find__1,
         sparsity_get_ccs,
         sparsity_get_col,
         sparsity_get_colind,
         sparsity_get_crs,
         sparsity_get_diag,
         sparsity_get_lower,
         sparsity_get_nz__0,
         sparsity_get_nz__1,
         sparsity_get_nz__2,
         sparsity_get_row,
         sparsity_get_triplet,
         sparsity_get_upper,
         sparsity_has_nz,
         sparsity_hash,
         sparsity_intersect,
         sparsity_isReshape,
         sparsity_is_column,
         sparsity_is_dense,
         sparsity_is_diag,
         sparsity_is_empty__0,
         sparsity_is_empty__1,
         sparsity_is_equal__0,
         sparsity_is_equal__1,
         sparsity_is_row,
         sparsity_is_scalar__0,
         sparsity_is_scalar__1,
         sparsity_is_singular,
         sparsity_is_square,
         sparsity_is_symmetric,
         sparsity_is_transpose,
         sparsity_is_tril,
         sparsity_is_triu,
         sparsity_is_vector,
         sparsity_largest_first,
         sparsity_lower,
         sparsity_makeDense,
         sparsity_nnz,
         sparsity_nnz_diag,
         sparsity_nnz_lower__0,
         sparsity_nnz_lower__1,
         sparsity_nnz_upper__0,
         sparsity_nnz_upper__1,
         sparsity_numel,
         sparsity_operator__equals,
         sparsity_operator__mul,
         sparsity_operator__nequals,
         sparsity_operator__plus,
         sparsity_pattern_inverse,
         sparsity_pmult__0,
         sparsity_pmult__1,
         sparsity_pmult__2,
         sparsity_pmult__3,
         sparsity_print_compact,
         sparsity_removeDuplicates,
         sparsity_repr_el,
         sparsity_resize,
         sparsity_row,
         sparsity_rowcol,
         sparsity_rowsSequential__0,
         sparsity_rowsSequential__1,
         sparsity_sanity_check__0,
         sparsity_sanity_check__1,
         sparsity_scalar__0,
         sparsity_scalar__1,
         sparsity_scc,
         sparsity_size1,
         sparsity_size2,
         sparsity_size__0,
         sparsity_size__1,
         sparsity_spy,
         sparsity_spy_matlab,
         sparsity_star_coloring2__0,
         sparsity_star_coloring2__1,
         sparsity_star_coloring2__2,
         sparsity_star_coloring__0,
         sparsity_star_coloring__1,
         sparsity_star_coloring__2,
         sparsity_sub__0,
         sparsity_sub__1,
         sparsity_sub__2,
         sparsity_sub__3,
         sparsity_transpose__0,
         sparsity_transpose__1,
         sparsity_triplet__0,
         sparsity_triplet__1,
         sparsity_uni_coloring__0,
         sparsity_uni_coloring__1,
         sparsity_uni_coloring__2,
         sparsity_unit,
         sparsity_unite,
         sparsity_upper,
       ) where


import Prelude hiding ( Functor )

import Data.Vector ( Vector )
import qualified Data.Map as M
import Foreign.C.Types
import Foreign.Marshal ( new, free )
import Foreign.Storable ( peek )
import Foreign.Ptr ( Ptr, nullPtr )
import Foreign.ForeignPtr ( newForeignPtr )
import System.IO.Unsafe ( unsafePerformIO ) -- for show instances

import Casadi.Internal.FormatException ( formatException )
import Casadi.Internal.MarshalTypes ( StdVec, StdString, StdMap, StdPair ) -- StdPair StdOstream'
import Casadi.Internal.Marshal ( Marshal(..), marshal, marshalFree )
import Casadi.Internal.WrapReturn ( WrapReturn(..) )
import Casadi.Core.Data
import Casadi.Core.Enums
-- direct wrapper
foreign import ccall unsafe "casadi__Sparsity__CONSTRUCTOR__0" c_casadi__Sparsity__CONSTRUCTOR__0
  :: Ptr (Ptr StdString) -> Ptr (StdPair CInt CInt) -> IO (Ptr Sparsity')

casadi__Sparsity__CONSTRUCTOR__0
  :: (Int, Int) -> IO Sparsity
casadi__Sparsity__CONSTRUCTOR__0 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Sparsity__CONSTRUCTOR__0 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
sparsity__0 :: (Int, Int) -> IO Sparsity
sparsity__0 = casadi__Sparsity__CONSTRUCTOR__0


-- direct wrapper
foreign import ccall unsafe "casadi__Sparsity__CONSTRUCTOR__1" c_casadi__Sparsity__CONSTRUCTOR__1
  :: Ptr (Ptr StdString) -> CInt -> CInt -> Ptr (StdVec CInt) -> Ptr (StdVec CInt) -> IO (Ptr Sparsity')

casadi__Sparsity__CONSTRUCTOR__1
  :: Int -> Int -> Vector Int -> Vector Int -> IO Sparsity
casadi__Sparsity__CONSTRUCTOR__1 x0 x1 x2 x3 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Sparsity__CONSTRUCTOR__1 errStrPtrP x0' x1' x2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'

  return ret



-- classy wrapper
sparsity__1 :: Int -> Int -> Vector Int -> Vector Int -> IO Sparsity
sparsity__1 = casadi__Sparsity__CONSTRUCTOR__1


-- direct wrapper
foreign import ccall unsafe "casadi__Sparsity__CONSTRUCTOR__2" c_casadi__Sparsity__CONSTRUCTOR__2
  :: Ptr (Ptr StdString) -> CInt -> CInt -> IO (Ptr Sparsity')

casadi__Sparsity__CONSTRUCTOR__2
  :: Int -> Int -> IO Sparsity
casadi__Sparsity__CONSTRUCTOR__2 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Sparsity__CONSTRUCTOR__2 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
sparsity__2 :: Int -> Int -> IO Sparsity
sparsity__2 = casadi__Sparsity__CONSTRUCTOR__2


-- direct wrapper
foreign import ccall unsafe "casadi__Sparsity__CONSTRUCTOR__3" c_casadi__Sparsity__CONSTRUCTOR__3
  :: Ptr (Ptr StdString) -> IO (Ptr Sparsity')

casadi__Sparsity__CONSTRUCTOR__3
  :: IO Sparsity
casadi__Sparsity__CONSTRUCTOR__3  = do


  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Sparsity__CONSTRUCTOR__3 errStrPtrP 
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)



  return ret



-- classy wrapper
sparsity__3 :: IO Sparsity
sparsity__3 = casadi__Sparsity__CONSTRUCTOR__3


-- direct wrapper
foreign import ccall unsafe "casadi__Sparsity__CONSTRUCTOR__4" c_casadi__Sparsity__CONSTRUCTOR__4
  :: Ptr (Ptr StdString) -> CInt -> IO (Ptr Sparsity')

casadi__Sparsity__CONSTRUCTOR__4
  :: Int -> IO Sparsity
casadi__Sparsity__CONSTRUCTOR__4 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Sparsity__CONSTRUCTOR__4 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
sparsity__4 :: Int -> IO Sparsity
sparsity__4 = casadi__Sparsity__CONSTRUCTOR__4


-- direct wrapper
foreign import ccall unsafe "casadi__Sparsity__T" c_casadi__Sparsity__T
  :: Ptr (Ptr StdString) -> Ptr Sparsity' -> IO (Ptr Sparsity')

casadi__Sparsity__T
  :: Sparsity -> IO Sparsity
casadi__Sparsity__T x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Sparsity__T errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
sparsity_T :: SparsityClass a => a -> IO Sparsity
sparsity_T x = casadi__Sparsity__T (castSparsity x)


-- direct wrapper
foreign import ccall unsafe "casadi__Sparsity__add_nz" c_casadi__Sparsity__add_nz
  :: Ptr (Ptr StdString) -> Ptr Sparsity' -> CInt -> CInt -> IO CInt

casadi__Sparsity__add_nz
  :: Sparsity -> Int -> Int -> IO Int
casadi__Sparsity__add_nz x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Sparsity__add_nz errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



-- classy wrapper
sparsity_add_nz :: SparsityClass a => a -> Int -> Int -> IO Int
sparsity_add_nz x = casadi__Sparsity__add_nz (castSparsity x)


-- direct wrapper
foreign import ccall unsafe "casadi__Sparsity__append" c_casadi__Sparsity__append
  :: Ptr (Ptr StdString) -> Ptr Sparsity' -> Ptr Sparsity' -> IO ()

casadi__Sparsity__append
  :: Sparsity -> Sparsity -> IO ()
casadi__Sparsity__append x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Sparsity__append errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ()



-- classy wrapper
sparsity_append :: SparsityClass a => a -> Sparsity -> IO ()
sparsity_append x = casadi__Sparsity__append (castSparsity x)


-- direct wrapper
foreign import ccall unsafe "casadi__Sparsity__appendColumns" c_casadi__Sparsity__appendColumns
  :: Ptr (Ptr StdString) -> Ptr Sparsity' -> Ptr Sparsity' -> IO ()

casadi__Sparsity__appendColumns
  :: Sparsity -> Sparsity -> IO ()
casadi__Sparsity__appendColumns x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Sparsity__appendColumns errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ()



-- classy wrapper
sparsity_appendColumns :: SparsityClass a => a -> Sparsity -> IO ()
sparsity_appendColumns x = casadi__Sparsity__appendColumns (castSparsity x)


-- direct wrapper
foreign import ccall unsafe "casadi__Sparsity__band" c_casadi__Sparsity__band
  :: Ptr (Ptr StdString) -> CInt -> CInt -> IO (Ptr Sparsity')

casadi__Sparsity__band
  :: Int -> Int -> IO Sparsity
casadi__Sparsity__band x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Sparsity__band errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
sparsity_band :: Int -> Int -> IO Sparsity
sparsity_band = casadi__Sparsity__band


-- direct wrapper
foreign import ccall unsafe "casadi__Sparsity__banded" c_casadi__Sparsity__banded
  :: Ptr (Ptr StdString) -> CInt -> CInt -> IO (Ptr Sparsity')

casadi__Sparsity__banded
  :: Int -> Int -> IO Sparsity
casadi__Sparsity__banded x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Sparsity__banded errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
sparsity_banded :: Int -> Int -> IO Sparsity
sparsity_banded = casadi__Sparsity__banded


-- direct wrapper
foreign import ccall unsafe "casadi__Sparsity__btf__0" c_casadi__Sparsity__btf__0
  :: Ptr (Ptr StdString) -> Ptr Sparsity' -> Ptr (Ptr (StdVec CInt)) -> Ptr (Ptr (StdVec CInt)) -> Ptr (Ptr (StdVec CInt)) -> Ptr (Ptr (StdVec CInt)) -> Ptr (Ptr (StdVec CInt)) -> Ptr (Ptr (StdVec CInt)) -> IO CInt

casadi__Sparsity__btf__0
  :: Sparsity -> IO (Int, Vector Int, Vector Int, Vector Int, Vector Int, Vector Int, Vector Int)
casadi__Sparsity__btf__0 x0 = do
  x0' <- marshal x0
  o1' <- new nullPtr
  o2' <- new nullPtr
  o3' <- new nullPtr
  o4' <- new nullPtr
  o5' <- new nullPtr
  o6' <- new nullPtr

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Sparsity__btf__0 errStrPtrP x0' o1' o2' o3' o4' o5' o6'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  o1'' <- peek o1'
  free o1'
  o1''' <- if o1'' == nullPtr then error "swig output o1' was not set in casadi__Sparsity__btf__0/c_casadi__Sparsity__btf__0" else wrapReturn o1''
  o2'' <- peek o2'
  free o2'
  o2''' <- if o2'' == nullPtr then error "swig output o2' was not set in casadi__Sparsity__btf__0/c_casadi__Sparsity__btf__0" else wrapReturn o2''
  o3'' <- peek o3'
  free o3'
  o3''' <- if o3'' == nullPtr then error "swig output o3' was not set in casadi__Sparsity__btf__0/c_casadi__Sparsity__btf__0" else wrapReturn o3''
  o4'' <- peek o4'
  free o4'
  o4''' <- if o4'' == nullPtr then error "swig output o4' was not set in casadi__Sparsity__btf__0/c_casadi__Sparsity__btf__0" else wrapReturn o4''
  o5'' <- peek o5'
  free o5'
  o5''' <- if o5'' == nullPtr then error "swig output o5' was not set in casadi__Sparsity__btf__0/c_casadi__Sparsity__btf__0" else wrapReturn o5''
  o6'' <- peek o6'
  free o6'
  o6''' <- if o6'' == nullPtr then error "swig output o6' was not set in casadi__Sparsity__btf__0/c_casadi__Sparsity__btf__0" else wrapReturn o6''

  return (ret, o1''', o2''', o3''', o4''', o5''', o6''')



-- classy wrapper
sparsity_btf__0 :: SparsityClass a => a -> IO (Int, Vector Int, Vector Int, Vector Int, Vector Int, Vector Int, Vector Int)
sparsity_btf__0 x = casadi__Sparsity__btf__0 (castSparsity x)


-- direct wrapper
foreign import ccall unsafe "casadi__Sparsity__btf__1" c_casadi__Sparsity__btf__1
  :: Ptr (Ptr StdString) -> Ptr Sparsity' -> Ptr (Ptr (StdVec CInt)) -> Ptr (Ptr (StdVec CInt)) -> Ptr (Ptr (StdVec CInt)) -> Ptr (Ptr (StdVec CInt)) -> Ptr (Ptr (StdVec CInt)) -> Ptr (Ptr (StdVec CInt)) -> CInt -> IO CInt

casadi__Sparsity__btf__1
  :: Sparsity -> Int -> IO (Int, Vector Int, Vector Int, Vector Int, Vector Int, Vector Int, Vector Int)
casadi__Sparsity__btf__1 x0 x7 = do
  x0' <- marshal x0
  o1' <- new nullPtr
  o2' <- new nullPtr
  o3' <- new nullPtr
  o4' <- new nullPtr
  o5' <- new nullPtr
  o6' <- new nullPtr
  x7' <- marshal x7

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Sparsity__btf__1 errStrPtrP x0' o1' o2' o3' o4' o5' o6' x7'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  o1'' <- peek o1'
  free o1'
  o1''' <- if o1'' == nullPtr then error "swig output o1' was not set in casadi__Sparsity__btf__1/c_casadi__Sparsity__btf__1" else wrapReturn o1''
  o2'' <- peek o2'
  free o2'
  o2''' <- if o2'' == nullPtr then error "swig output o2' was not set in casadi__Sparsity__btf__1/c_casadi__Sparsity__btf__1" else wrapReturn o2''
  o3'' <- peek o3'
  free o3'
  o3''' <- if o3'' == nullPtr then error "swig output o3' was not set in casadi__Sparsity__btf__1/c_casadi__Sparsity__btf__1" else wrapReturn o3''
  o4'' <- peek o4'
  free o4'
  o4''' <- if o4'' == nullPtr then error "swig output o4' was not set in casadi__Sparsity__btf__1/c_casadi__Sparsity__btf__1" else wrapReturn o4''
  o5'' <- peek o5'
  free o5'
  o5''' <- if o5'' == nullPtr then error "swig output o5' was not set in casadi__Sparsity__btf__1/c_casadi__Sparsity__btf__1" else wrapReturn o5''
  o6'' <- peek o6'
  free o6'
  o6''' <- if o6'' == nullPtr then error "swig output o6' was not set in casadi__Sparsity__btf__1/c_casadi__Sparsity__btf__1" else wrapReturn o6''
  marshalFree x7 x7'

  return (ret, o1''', o2''', o3''', o4''', o5''', o6''')



-- classy wrapper
sparsity_btf__1 :: SparsityClass a => a -> Int -> IO (Int, Vector Int, Vector Int, Vector Int, Vector Int, Vector Int, Vector Int)
sparsity_btf__1 x = casadi__Sparsity__btf__1 (castSparsity x)


-- direct wrapper
foreign import ccall unsafe "casadi__Sparsity__bw_lower" c_casadi__Sparsity__bw_lower
  :: Ptr (Ptr StdString) -> Ptr Sparsity' -> IO CInt

casadi__Sparsity__bw_lower
  :: Sparsity -> IO Int
casadi__Sparsity__bw_lower x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Sparsity__bw_lower errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
sparsity_bw_lower :: SparsityClass a => a -> IO Int
sparsity_bw_lower x = casadi__Sparsity__bw_lower (castSparsity x)


-- direct wrapper
foreign import ccall unsafe "casadi__Sparsity__bw_upper" c_casadi__Sparsity__bw_upper
  :: Ptr (Ptr StdString) -> Ptr Sparsity' -> IO CInt

casadi__Sparsity__bw_upper
  :: Sparsity -> IO Int
casadi__Sparsity__bw_upper x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Sparsity__bw_upper errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
sparsity_bw_upper :: SparsityClass a => a -> IO Int
sparsity_bw_upper x = casadi__Sparsity__bw_upper (castSparsity x)


-- direct wrapper
foreign import ccall unsafe "casadi__Sparsity__colind" c_casadi__Sparsity__colind
  :: Ptr (Ptr StdString) -> Ptr Sparsity' -> CInt -> IO CInt

casadi__Sparsity__colind
  :: Sparsity -> Int -> IO Int
casadi__Sparsity__colind x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Sparsity__colind errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
sparsity_colind :: SparsityClass a => a -> Int -> IO Int
sparsity_colind x = casadi__Sparsity__colind (castSparsity x)


-- direct wrapper
foreign import ccall unsafe "casadi__Sparsity__combine" c_casadi__Sparsity__combine
  :: Ptr (Ptr StdString) -> Ptr Sparsity' -> Ptr Sparsity' -> CInt -> CInt -> IO (Ptr Sparsity')

casadi__Sparsity__combine
  :: Sparsity -> Sparsity -> Bool -> Bool -> IO Sparsity
casadi__Sparsity__combine x0 x1 x2 x3 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Sparsity__combine errStrPtrP x0' x1' x2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'

  return ret



-- classy wrapper
sparsity_combine :: SparsityClass a => a -> Sparsity -> Bool -> Bool -> IO Sparsity
sparsity_combine x = casadi__Sparsity__combine (castSparsity x)


-- direct wrapper
foreign import ccall unsafe "casadi__Sparsity__compress" c_casadi__Sparsity__compress
  :: Ptr (Ptr StdString) -> Ptr Sparsity' -> IO (Ptr (StdVec CInt))

casadi__Sparsity__compress
  :: Sparsity -> IO (Vector Int)
casadi__Sparsity__compress x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Sparsity__compress errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
sparsity_compress :: SparsityClass a => a -> IO (Vector Int)
sparsity_compress x = casadi__Sparsity__compress (castSparsity x)


-- direct wrapper
foreign import ccall unsafe "casadi__Sparsity__compressed" c_casadi__Sparsity__compressed
  :: Ptr (Ptr StdString) -> Ptr (StdVec CInt) -> IO (Ptr Sparsity')

casadi__Sparsity__compressed
  :: Vector Int -> IO Sparsity
casadi__Sparsity__compressed x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Sparsity__compressed errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
sparsity_compressed :: Vector Int -> IO Sparsity
sparsity_compressed = casadi__Sparsity__compressed


-- direct wrapper
foreign import ccall unsafe "casadi__Sparsity__dense__0" c_casadi__Sparsity__dense__0
  :: Ptr (Ptr StdString) -> Ptr (StdPair CInt CInt) -> IO (Ptr Sparsity')

casadi__Sparsity__dense__0
  :: (Int, Int) -> IO Sparsity
casadi__Sparsity__dense__0 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Sparsity__dense__0 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
sparsity_dense__0 :: (Int, Int) -> IO Sparsity
sparsity_dense__0 = casadi__Sparsity__dense__0


-- direct wrapper
foreign import ccall unsafe "casadi__Sparsity__dense__1" c_casadi__Sparsity__dense__1
  :: Ptr (Ptr StdString) -> CInt -> IO (Ptr Sparsity')

casadi__Sparsity__dense__1
  :: Int -> IO Sparsity
casadi__Sparsity__dense__1 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Sparsity__dense__1 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
sparsity_dense__1 :: Int -> IO Sparsity
sparsity_dense__1 = casadi__Sparsity__dense__1


-- direct wrapper
foreign import ccall unsafe "casadi__Sparsity__dense__2" c_casadi__Sparsity__dense__2
  :: Ptr (Ptr StdString) -> CInt -> CInt -> IO (Ptr Sparsity')

casadi__Sparsity__dense__2
  :: Int -> Int -> IO Sparsity
casadi__Sparsity__dense__2 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Sparsity__dense__2 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
sparsity_dense__2 :: Int -> Int -> IO Sparsity
sparsity_dense__2 = casadi__Sparsity__dense__2


-- direct wrapper
foreign import ccall unsafe "casadi__Sparsity__dfs" c_casadi__Sparsity__dfs
  :: Ptr (Ptr StdString) -> Ptr Sparsity' -> CInt -> CInt -> Ptr (StdVec CInt) -> Ptr (StdVec CInt) -> Ptr (StdVec CInt) -> Ptr (StdVec CInt) -> IO CInt

casadi__Sparsity__dfs
  :: Sparsity -> Int -> Int -> Vector Int -> Vector Int -> Vector Int -> Vector Bool -> IO Int
casadi__Sparsity__dfs x0 x1 x2 x3 x4 x5 x6 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3
  x4' <- marshal x4
  x5' <- marshal x5
  x6' <- marshal x6

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Sparsity__dfs errStrPtrP x0' x1' x2' x3' x4' x5' x6'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'
  marshalFree x4 x4'
  marshalFree x5 x5'
  marshalFree x6 x6'

  return ret



-- classy wrapper
sparsity_dfs :: SparsityClass a => a -> Int -> Int -> Vector Int -> Vector Int -> Vector Int -> Vector Bool -> IO Int
sparsity_dfs x = casadi__Sparsity__dfs (castSparsity x)


-- direct wrapper
foreign import ccall unsafe "casadi__Sparsity__diag__0" c_casadi__Sparsity__diag__0
  :: Ptr (Ptr StdString) -> Ptr (StdPair CInt CInt) -> IO (Ptr Sparsity')

casadi__Sparsity__diag__0
  :: (Int, Int) -> IO Sparsity
casadi__Sparsity__diag__0 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Sparsity__diag__0 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
sparsity_diag__0 :: (Int, Int) -> IO Sparsity
sparsity_diag__0 = casadi__Sparsity__diag__0


-- direct wrapper
foreign import ccall unsafe "casadi__Sparsity__diag__1" c_casadi__Sparsity__diag__1
  :: Ptr (Ptr StdString) -> CInt -> CInt -> IO (Ptr Sparsity')

casadi__Sparsity__diag__1
  :: Int -> Int -> IO Sparsity
casadi__Sparsity__diag__1 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Sparsity__diag__1 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
sparsity_diag__1 :: Int -> Int -> IO Sparsity
sparsity_diag__1 = casadi__Sparsity__diag__1


-- direct wrapper
foreign import ccall unsafe "casadi__Sparsity__diag__2" c_casadi__Sparsity__diag__2
  :: Ptr (Ptr StdString) -> CInt -> IO (Ptr Sparsity')

casadi__Sparsity__diag__2
  :: Int -> IO Sparsity
casadi__Sparsity__diag__2 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Sparsity__diag__2 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
sparsity_diag__2 :: Int -> IO Sparsity
sparsity_diag__2 = casadi__Sparsity__diag__2


-- direct wrapper
foreign import ccall unsafe "casadi__Sparsity__dim" c_casadi__Sparsity__dim
  :: Ptr (Ptr StdString) -> Ptr Sparsity' -> IO (Ptr StdString)

casadi__Sparsity__dim
  :: Sparsity -> IO String
casadi__Sparsity__dim x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Sparsity__dim errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
sparsity_dim :: SparsityClass a => a -> IO String
sparsity_dim x = casadi__Sparsity__dim (castSparsity x)


-- direct wrapper
foreign import ccall unsafe "casadi__Sparsity__enlarge__0" c_casadi__Sparsity__enlarge__0
  :: Ptr (Ptr StdString) -> Ptr Sparsity' -> CInt -> CInt -> Ptr (StdVec CInt) -> Ptr (StdVec CInt) -> IO ()

casadi__Sparsity__enlarge__0
  :: Sparsity -> Int -> Int -> Vector Int -> Vector Int -> IO ()
casadi__Sparsity__enlarge__0 x0 x1 x2 x3 x4 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3
  x4' <- marshal x4

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Sparsity__enlarge__0 errStrPtrP x0' x1' x2' x3' x4'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'
  marshalFree x4 x4'

  return ()



-- classy wrapper
sparsity_enlarge__0 :: SparsityClass a => a -> Int -> Int -> Vector Int -> Vector Int -> IO ()
sparsity_enlarge__0 x = casadi__Sparsity__enlarge__0 (castSparsity x)


-- direct wrapper
foreign import ccall unsafe "casadi__Sparsity__enlarge__1" c_casadi__Sparsity__enlarge__1
  :: Ptr (Ptr StdString) -> Ptr Sparsity' -> CInt -> CInt -> Ptr (StdVec CInt) -> Ptr (StdVec CInt) -> CInt -> IO ()

casadi__Sparsity__enlarge__1
  :: Sparsity -> Int -> Int -> Vector Int -> Vector Int -> Bool -> IO ()
casadi__Sparsity__enlarge__1 x0 x1 x2 x3 x4 x5 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3
  x4' <- marshal x4
  x5' <- marshal x5

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Sparsity__enlarge__1 errStrPtrP x0' x1' x2' x3' x4' x5'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'
  marshalFree x4 x4'
  marshalFree x5 x5'

  return ()



-- classy wrapper
sparsity_enlarge__1 :: SparsityClass a => a -> Int -> Int -> Vector Int -> Vector Int -> Bool -> IO ()
sparsity_enlarge__1 x = casadi__Sparsity__enlarge__1 (castSparsity x)


-- direct wrapper
foreign import ccall unsafe "casadi__Sparsity__enlargeColumns__0" c_casadi__Sparsity__enlargeColumns__0
  :: Ptr (Ptr StdString) -> Ptr Sparsity' -> CInt -> Ptr (StdVec CInt) -> IO ()

casadi__Sparsity__enlargeColumns__0
  :: Sparsity -> Int -> Vector Int -> IO ()
casadi__Sparsity__enlargeColumns__0 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Sparsity__enlargeColumns__0 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ()



-- classy wrapper
sparsity_enlargeColumns__0 :: SparsityClass a => a -> Int -> Vector Int -> IO ()
sparsity_enlargeColumns__0 x = casadi__Sparsity__enlargeColumns__0 (castSparsity x)


-- direct wrapper
foreign import ccall unsafe "casadi__Sparsity__enlargeColumns__1" c_casadi__Sparsity__enlargeColumns__1
  :: Ptr (Ptr StdString) -> Ptr Sparsity' -> CInt -> Ptr (StdVec CInt) -> CInt -> IO ()

casadi__Sparsity__enlargeColumns__1
  :: Sparsity -> Int -> Vector Int -> Bool -> IO ()
casadi__Sparsity__enlargeColumns__1 x0 x1 x2 x3 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Sparsity__enlargeColumns__1 errStrPtrP x0' x1' x2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'

  return ()



-- classy wrapper
sparsity_enlargeColumns__1 :: SparsityClass a => a -> Int -> Vector Int -> Bool -> IO ()
sparsity_enlargeColumns__1 x = casadi__Sparsity__enlargeColumns__1 (castSparsity x)


-- direct wrapper
foreign import ccall unsafe "casadi__Sparsity__enlargeRows__0" c_casadi__Sparsity__enlargeRows__0
  :: Ptr (Ptr StdString) -> Ptr Sparsity' -> CInt -> Ptr (StdVec CInt) -> IO ()

casadi__Sparsity__enlargeRows__0
  :: Sparsity -> Int -> Vector Int -> IO ()
casadi__Sparsity__enlargeRows__0 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Sparsity__enlargeRows__0 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ()



-- classy wrapper
sparsity_enlargeRows__0 :: SparsityClass a => a -> Int -> Vector Int -> IO ()
sparsity_enlargeRows__0 x = casadi__Sparsity__enlargeRows__0 (castSparsity x)


-- direct wrapper
foreign import ccall unsafe "casadi__Sparsity__enlargeRows__1" c_casadi__Sparsity__enlargeRows__1
  :: Ptr (Ptr StdString) -> Ptr Sparsity' -> CInt -> Ptr (StdVec CInt) -> CInt -> IO ()

casadi__Sparsity__enlargeRows__1
  :: Sparsity -> Int -> Vector Int -> Bool -> IO ()
casadi__Sparsity__enlargeRows__1 x0 x1 x2 x3 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Sparsity__enlargeRows__1 errStrPtrP x0' x1' x2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'

  return ()



-- classy wrapper
sparsity_enlargeRows__1 :: SparsityClass a => a -> Int -> Vector Int -> Bool -> IO ()
sparsity_enlargeRows__1 x = casadi__Sparsity__enlargeRows__1 (castSparsity x)


-- direct wrapper
foreign import ccall unsafe "casadi__Sparsity__erase__0" c_casadi__Sparsity__erase__0
  :: Ptr (Ptr StdString) -> Ptr Sparsity' -> Ptr (StdVec CInt) -> IO (Ptr (StdVec CInt))

casadi__Sparsity__erase__0
  :: Sparsity -> Vector Int -> IO (Vector Int)
casadi__Sparsity__erase__0 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Sparsity__erase__0 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
sparsity_erase__0 :: SparsityClass a => a -> Vector Int -> IO (Vector Int)
sparsity_erase__0 x = casadi__Sparsity__erase__0 (castSparsity x)


-- direct wrapper
foreign import ccall unsafe "casadi__Sparsity__erase__1" c_casadi__Sparsity__erase__1
  :: Ptr (Ptr StdString) -> Ptr Sparsity' -> Ptr (StdVec CInt) -> CInt -> IO (Ptr (StdVec CInt))

casadi__Sparsity__erase__1
  :: Sparsity -> Vector Int -> Bool -> IO (Vector Int)
casadi__Sparsity__erase__1 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Sparsity__erase__1 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



-- classy wrapper
sparsity_erase__1 :: SparsityClass a => a -> Vector Int -> Bool -> IO (Vector Int)
sparsity_erase__1 x = casadi__Sparsity__erase__1 (castSparsity x)


-- direct wrapper
foreign import ccall unsafe "casadi__Sparsity__erase__2" c_casadi__Sparsity__erase__2
  :: Ptr (Ptr StdString) -> Ptr Sparsity' -> Ptr (StdVec CInt) -> Ptr (StdVec CInt) -> IO (Ptr (StdVec CInt))

casadi__Sparsity__erase__2
  :: Sparsity -> Vector Int -> Vector Int -> IO (Vector Int)
casadi__Sparsity__erase__2 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Sparsity__erase__2 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



-- classy wrapper
sparsity_erase__2 :: SparsityClass a => a -> Vector Int -> Vector Int -> IO (Vector Int)
sparsity_erase__2 x = casadi__Sparsity__erase__2 (castSparsity x)


-- direct wrapper
foreign import ccall unsafe "casadi__Sparsity__erase__3" c_casadi__Sparsity__erase__3
  :: Ptr (Ptr StdString) -> Ptr Sparsity' -> Ptr (StdVec CInt) -> Ptr (StdVec CInt) -> CInt -> IO (Ptr (StdVec CInt))

casadi__Sparsity__erase__3
  :: Sparsity -> Vector Int -> Vector Int -> Bool -> IO (Vector Int)
casadi__Sparsity__erase__3 x0 x1 x2 x3 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Sparsity__erase__3 errStrPtrP x0' x1' x2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'

  return ret



-- classy wrapper
sparsity_erase__3 :: SparsityClass a => a -> Vector Int -> Vector Int -> Bool -> IO (Vector Int)
sparsity_erase__3 x = casadi__Sparsity__erase__3 (castSparsity x)


-- direct wrapper
foreign import ccall unsafe "casadi__Sparsity__etree__0" c_casadi__Sparsity__etree__0
  :: Ptr (Ptr StdString) -> Ptr Sparsity' -> IO (Ptr (StdVec CInt))

casadi__Sparsity__etree__0
  :: Sparsity -> IO (Vector Int)
casadi__Sparsity__etree__0 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Sparsity__etree__0 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
sparsity_etree__0 :: SparsityClass a => a -> IO (Vector Int)
sparsity_etree__0 x = casadi__Sparsity__etree__0 (castSparsity x)


-- direct wrapper
foreign import ccall unsafe "casadi__Sparsity__etree__1" c_casadi__Sparsity__etree__1
  :: Ptr (Ptr StdString) -> Ptr Sparsity' -> CInt -> IO (Ptr (StdVec CInt))

casadi__Sparsity__etree__1
  :: Sparsity -> Bool -> IO (Vector Int)
casadi__Sparsity__etree__1 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Sparsity__etree__1 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
sparsity_etree__1 :: SparsityClass a => a -> Bool -> IO (Vector Int)
sparsity_etree__1 x = casadi__Sparsity__etree__1 (castSparsity x)


-- direct wrapper
foreign import ccall unsafe "casadi__Sparsity__find__0" c_casadi__Sparsity__find__0
  :: Ptr (Ptr StdString) -> Ptr Sparsity' -> IO (Ptr (StdVec CInt))

casadi__Sparsity__find__0
  :: Sparsity -> IO (Vector Int)
casadi__Sparsity__find__0 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Sparsity__find__0 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
sparsity_find__0 :: SparsityClass a => a -> IO (Vector Int)
sparsity_find__0 x = casadi__Sparsity__find__0 (castSparsity x)


-- direct wrapper
foreign import ccall unsafe "casadi__Sparsity__find__1" c_casadi__Sparsity__find__1
  :: Ptr (Ptr StdString) -> Ptr Sparsity' -> CInt -> IO (Ptr (StdVec CInt))

casadi__Sparsity__find__1
  :: Sparsity -> Bool -> IO (Vector Int)
casadi__Sparsity__find__1 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Sparsity__find__1 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
sparsity_find__1 :: SparsityClass a => a -> Bool -> IO (Vector Int)
sparsity_find__1 x = casadi__Sparsity__find__1 (castSparsity x)


-- direct wrapper
foreign import ccall unsafe "casadi__Sparsity__get_ccs" c_casadi__Sparsity__get_ccs
  :: Ptr (Ptr StdString) -> Ptr Sparsity' -> Ptr (Ptr (StdVec CInt)) -> Ptr (Ptr (StdVec CInt)) -> IO ()

casadi__Sparsity__get_ccs
  :: Sparsity -> IO (Vector Int, Vector Int)
casadi__Sparsity__get_ccs x0 = do
  x0' <- marshal x0
  o1' <- new nullPtr
  o2' <- new nullPtr

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Sparsity__get_ccs errStrPtrP x0' o1' o2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  o1'' <- peek o1'
  free o1'
  o1''' <- if o1'' == nullPtr then error "swig output o1' was not set in casadi__Sparsity__get_ccs/c_casadi__Sparsity__get_ccs" else wrapReturn o1''
  o2'' <- peek o2'
  free o2'
  o2''' <- if o2'' == nullPtr then error "swig output o2' was not set in casadi__Sparsity__get_ccs/c_casadi__Sparsity__get_ccs" else wrapReturn o2''

  return (o1''', o2''')



-- classy wrapper
sparsity_get_ccs :: SparsityClass a => a -> IO (Vector Int, Vector Int)
sparsity_get_ccs x = casadi__Sparsity__get_ccs (castSparsity x)


-- direct wrapper
foreign import ccall unsafe "casadi__Sparsity__get_col" c_casadi__Sparsity__get_col
  :: Ptr (Ptr StdString) -> Ptr Sparsity' -> IO (Ptr (StdVec CInt))

casadi__Sparsity__get_col
  :: Sparsity -> IO (Vector Int)
casadi__Sparsity__get_col x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Sparsity__get_col errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
sparsity_get_col :: SparsityClass a => a -> IO (Vector Int)
sparsity_get_col x = casadi__Sparsity__get_col (castSparsity x)


-- direct wrapper
foreign import ccall unsafe "casadi__Sparsity__get_colind" c_casadi__Sparsity__get_colind
  :: Ptr (Ptr StdString) -> Ptr Sparsity' -> IO (Ptr (StdVec CInt))

casadi__Sparsity__get_colind
  :: Sparsity -> IO (Vector Int)
casadi__Sparsity__get_colind x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Sparsity__get_colind errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
sparsity_get_colind :: SparsityClass a => a -> IO (Vector Int)
sparsity_get_colind x = casadi__Sparsity__get_colind (castSparsity x)


-- direct wrapper
foreign import ccall unsafe "casadi__Sparsity__get_crs" c_casadi__Sparsity__get_crs
  :: Ptr (Ptr StdString) -> Ptr Sparsity' -> Ptr (Ptr (StdVec CInt)) -> Ptr (Ptr (StdVec CInt)) -> IO ()

casadi__Sparsity__get_crs
  :: Sparsity -> IO (Vector Int, Vector Int)
casadi__Sparsity__get_crs x0 = do
  x0' <- marshal x0
  o1' <- new nullPtr
  o2' <- new nullPtr

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Sparsity__get_crs errStrPtrP x0' o1' o2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  o1'' <- peek o1'
  free o1'
  o1''' <- if o1'' == nullPtr then error "swig output o1' was not set in casadi__Sparsity__get_crs/c_casadi__Sparsity__get_crs" else wrapReturn o1''
  o2'' <- peek o2'
  free o2'
  o2''' <- if o2'' == nullPtr then error "swig output o2' was not set in casadi__Sparsity__get_crs/c_casadi__Sparsity__get_crs" else wrapReturn o2''

  return (o1''', o2''')



-- classy wrapper
sparsity_get_crs :: SparsityClass a => a -> IO (Vector Int, Vector Int)
sparsity_get_crs x = casadi__Sparsity__get_crs (castSparsity x)


-- direct wrapper
foreign import ccall unsafe "casadi__Sparsity__get_diag" c_casadi__Sparsity__get_diag
  :: Ptr (Ptr StdString) -> Ptr Sparsity' -> Ptr (Ptr (StdVec CInt)) -> IO (Ptr Sparsity')

casadi__Sparsity__get_diag
  :: Sparsity -> IO (Sparsity, Vector Int)
casadi__Sparsity__get_diag x0 = do
  x0' <- marshal x0
  o1' <- new nullPtr

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Sparsity__get_diag errStrPtrP x0' o1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  o1'' <- peek o1'
  free o1'
  o1''' <- if o1'' == nullPtr then error "swig output o1' was not set in casadi__Sparsity__get_diag/c_casadi__Sparsity__get_diag" else wrapReturn o1''

  return (ret, o1''')



-- classy wrapper
sparsity_get_diag :: SparsityClass a => a -> IO (Sparsity, Vector Int)
sparsity_get_diag x = casadi__Sparsity__get_diag (castSparsity x)


-- direct wrapper
foreign import ccall unsafe "casadi__Sparsity__get_lower" c_casadi__Sparsity__get_lower
  :: Ptr (Ptr StdString) -> Ptr Sparsity' -> IO (Ptr (StdVec CInt))

casadi__Sparsity__get_lower
  :: Sparsity -> IO (Vector Int)
casadi__Sparsity__get_lower x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Sparsity__get_lower errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
sparsity_get_lower :: SparsityClass a => a -> IO (Vector Int)
sparsity_get_lower x = casadi__Sparsity__get_lower (castSparsity x)


-- direct wrapper
foreign import ccall unsafe "casadi__Sparsity__get_nz__0" c_casadi__Sparsity__get_nz__0
  :: Ptr (Ptr StdString) -> Ptr Sparsity' -> Ptr (StdVec CInt) -> IO ()

casadi__Sparsity__get_nz__0
  :: Sparsity -> Vector Int -> IO ()
casadi__Sparsity__get_nz__0 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Sparsity__get_nz__0 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ()



-- classy wrapper
sparsity_get_nz__0 :: SparsityClass a => a -> Vector Int -> IO ()
sparsity_get_nz__0 x = casadi__Sparsity__get_nz__0 (castSparsity x)


-- direct wrapper
foreign import ccall unsafe "casadi__Sparsity__get_nz__1" c_casadi__Sparsity__get_nz__1
  :: Ptr (Ptr StdString) -> Ptr Sparsity' -> Ptr (StdVec CInt) -> Ptr (StdVec CInt) -> IO (Ptr (StdVec CInt))

casadi__Sparsity__get_nz__1
  :: Sparsity -> Vector Int -> Vector Int -> IO (Vector Int)
casadi__Sparsity__get_nz__1 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Sparsity__get_nz__1 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



-- classy wrapper
sparsity_get_nz__1 :: SparsityClass a => a -> Vector Int -> Vector Int -> IO (Vector Int)
sparsity_get_nz__1 x = casadi__Sparsity__get_nz__1 (castSparsity x)


-- direct wrapper
foreign import ccall unsafe "casadi__Sparsity__get_nz__2" c_casadi__Sparsity__get_nz__2
  :: Ptr (Ptr StdString) -> Ptr Sparsity' -> CInt -> CInt -> IO CInt

casadi__Sparsity__get_nz__2
  :: Sparsity -> Int -> Int -> IO Int
casadi__Sparsity__get_nz__2 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Sparsity__get_nz__2 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



-- classy wrapper
sparsity_get_nz__2 :: SparsityClass a => a -> Int -> Int -> IO Int
sparsity_get_nz__2 x = casadi__Sparsity__get_nz__2 (castSparsity x)


-- direct wrapper
foreign import ccall unsafe "casadi__Sparsity__get_row" c_casadi__Sparsity__get_row
  :: Ptr (Ptr StdString) -> Ptr Sparsity' -> IO (Ptr (StdVec CInt))

casadi__Sparsity__get_row
  :: Sparsity -> IO (Vector Int)
casadi__Sparsity__get_row x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Sparsity__get_row errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
sparsity_get_row :: SparsityClass a => a -> IO (Vector Int)
sparsity_get_row x = casadi__Sparsity__get_row (castSparsity x)


-- direct wrapper
foreign import ccall unsafe "casadi__Sparsity__get_triplet" c_casadi__Sparsity__get_triplet
  :: Ptr (Ptr StdString) -> Ptr Sparsity' -> Ptr (Ptr (StdVec CInt)) -> Ptr (Ptr (StdVec CInt)) -> IO ()

casadi__Sparsity__get_triplet
  :: Sparsity -> IO (Vector Int, Vector Int)
casadi__Sparsity__get_triplet x0 = do
  x0' <- marshal x0
  o1' <- new nullPtr
  o2' <- new nullPtr

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Sparsity__get_triplet errStrPtrP x0' o1' o2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  o1'' <- peek o1'
  free o1'
  o1''' <- if o1'' == nullPtr then error "swig output o1' was not set in casadi__Sparsity__get_triplet/c_casadi__Sparsity__get_triplet" else wrapReturn o1''
  o2'' <- peek o2'
  free o2'
  o2''' <- if o2'' == nullPtr then error "swig output o2' was not set in casadi__Sparsity__get_triplet/c_casadi__Sparsity__get_triplet" else wrapReturn o2''

  return (o1''', o2''')



-- classy wrapper
sparsity_get_triplet :: SparsityClass a => a -> IO (Vector Int, Vector Int)
sparsity_get_triplet x = casadi__Sparsity__get_triplet (castSparsity x)


-- direct wrapper
foreign import ccall unsafe "casadi__Sparsity__get_upper" c_casadi__Sparsity__get_upper
  :: Ptr (Ptr StdString) -> Ptr Sparsity' -> IO (Ptr (StdVec CInt))

casadi__Sparsity__get_upper
  :: Sparsity -> IO (Vector Int)
casadi__Sparsity__get_upper x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Sparsity__get_upper errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
sparsity_get_upper :: SparsityClass a => a -> IO (Vector Int)
sparsity_get_upper x = casadi__Sparsity__get_upper (castSparsity x)


-- direct wrapper
foreign import ccall unsafe "casadi__Sparsity__has_nz" c_casadi__Sparsity__has_nz
  :: Ptr (Ptr StdString) -> Ptr Sparsity' -> CInt -> CInt -> IO CInt

casadi__Sparsity__has_nz
  :: Sparsity -> Int -> Int -> IO Bool
casadi__Sparsity__has_nz x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Sparsity__has_nz errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



-- classy wrapper
sparsity_has_nz :: SparsityClass a => a -> Int -> Int -> IO Bool
sparsity_has_nz x = casadi__Sparsity__has_nz (castSparsity x)


-- direct wrapper
foreign import ccall unsafe "casadi__Sparsity__hash" c_casadi__Sparsity__hash
  :: Ptr (Ptr StdString) -> Ptr Sparsity' -> IO CSize

casadi__Sparsity__hash
  :: Sparsity -> IO CSize
casadi__Sparsity__hash x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Sparsity__hash errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
sparsity_hash :: SparsityClass a => a -> IO CSize
sparsity_hash x = casadi__Sparsity__hash (castSparsity x)


-- direct wrapper
foreign import ccall unsafe "casadi__Sparsity__intersect" c_casadi__Sparsity__intersect
  :: Ptr (Ptr StdString) -> Ptr Sparsity' -> Ptr Sparsity' -> IO (Ptr Sparsity')

casadi__Sparsity__intersect
  :: Sparsity -> Sparsity -> IO Sparsity
casadi__Sparsity__intersect x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Sparsity__intersect errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
sparsity_intersect :: SparsityClass a => a -> Sparsity -> IO Sparsity
sparsity_intersect x = casadi__Sparsity__intersect (castSparsity x)


-- direct wrapper
foreign import ccall unsafe "casadi__Sparsity__isReshape" c_casadi__Sparsity__isReshape
  :: Ptr (Ptr StdString) -> Ptr Sparsity' -> Ptr Sparsity' -> IO CInt

casadi__Sparsity__isReshape
  :: Sparsity -> Sparsity -> IO Bool
casadi__Sparsity__isReshape x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Sparsity__isReshape errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
sparsity_isReshape :: SparsityClass a => a -> Sparsity -> IO Bool
sparsity_isReshape x = casadi__Sparsity__isReshape (castSparsity x)


-- direct wrapper
foreign import ccall unsafe "casadi__Sparsity__is_column" c_casadi__Sparsity__is_column
  :: Ptr (Ptr StdString) -> Ptr Sparsity' -> IO CInt

casadi__Sparsity__is_column
  :: Sparsity -> IO Bool
casadi__Sparsity__is_column x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Sparsity__is_column errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
sparsity_is_column :: SparsityClass a => a -> IO Bool
sparsity_is_column x = casadi__Sparsity__is_column (castSparsity x)


-- direct wrapper
foreign import ccall unsafe "casadi__Sparsity__is_dense" c_casadi__Sparsity__is_dense
  :: Ptr (Ptr StdString) -> Ptr Sparsity' -> IO CInt

casadi__Sparsity__is_dense
  :: Sparsity -> IO Bool
casadi__Sparsity__is_dense x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Sparsity__is_dense errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
sparsity_is_dense :: SparsityClass a => a -> IO Bool
sparsity_is_dense x = casadi__Sparsity__is_dense (castSparsity x)


-- direct wrapper
foreign import ccall unsafe "casadi__Sparsity__is_diag" c_casadi__Sparsity__is_diag
  :: Ptr (Ptr StdString) -> Ptr Sparsity' -> IO CInt

casadi__Sparsity__is_diag
  :: Sparsity -> IO Bool
casadi__Sparsity__is_diag x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Sparsity__is_diag errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
sparsity_is_diag :: SparsityClass a => a -> IO Bool
sparsity_is_diag x = casadi__Sparsity__is_diag (castSparsity x)


-- direct wrapper
foreign import ccall unsafe "casadi__Sparsity__is_empty__0" c_casadi__Sparsity__is_empty__0
  :: Ptr (Ptr StdString) -> Ptr Sparsity' -> IO CInt

casadi__Sparsity__is_empty__0
  :: Sparsity -> IO Bool
casadi__Sparsity__is_empty__0 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Sparsity__is_empty__0 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
sparsity_is_empty__0 :: SparsityClass a => a -> IO Bool
sparsity_is_empty__0 x = casadi__Sparsity__is_empty__0 (castSparsity x)


-- direct wrapper
foreign import ccall unsafe "casadi__Sparsity__is_empty__1" c_casadi__Sparsity__is_empty__1
  :: Ptr (Ptr StdString) -> Ptr Sparsity' -> CInt -> IO CInt

casadi__Sparsity__is_empty__1
  :: Sparsity -> Bool -> IO Bool
casadi__Sparsity__is_empty__1 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Sparsity__is_empty__1 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
sparsity_is_empty__1 :: SparsityClass a => a -> Bool -> IO Bool
sparsity_is_empty__1 x = casadi__Sparsity__is_empty__1 (castSparsity x)


-- direct wrapper
foreign import ccall unsafe "casadi__Sparsity__is_equal__0" c_casadi__Sparsity__is_equal__0
  :: Ptr (Ptr StdString) -> Ptr Sparsity' -> CInt -> CInt -> Ptr (StdVec CInt) -> Ptr (StdVec CInt) -> IO CInt

casadi__Sparsity__is_equal__0
  :: Sparsity -> Int -> Int -> Vector Int -> Vector Int -> IO Bool
casadi__Sparsity__is_equal__0 x0 x1 x2 x3 x4 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3
  x4' <- marshal x4

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Sparsity__is_equal__0 errStrPtrP x0' x1' x2' x3' x4'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'
  marshalFree x4 x4'

  return ret



-- classy wrapper
sparsity_is_equal__0 :: SparsityClass a => a -> Int -> Int -> Vector Int -> Vector Int -> IO Bool
sparsity_is_equal__0 x = casadi__Sparsity__is_equal__0 (castSparsity x)


-- direct wrapper
foreign import ccall unsafe "casadi__Sparsity__is_equal__1" c_casadi__Sparsity__is_equal__1
  :: Ptr (Ptr StdString) -> Ptr Sparsity' -> Ptr Sparsity' -> IO CInt

casadi__Sparsity__is_equal__1
  :: Sparsity -> Sparsity -> IO Bool
casadi__Sparsity__is_equal__1 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Sparsity__is_equal__1 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
sparsity_is_equal__1 :: SparsityClass a => a -> Sparsity -> IO Bool
sparsity_is_equal__1 x = casadi__Sparsity__is_equal__1 (castSparsity x)


-- direct wrapper
foreign import ccall unsafe "casadi__Sparsity__is_row" c_casadi__Sparsity__is_row
  :: Ptr (Ptr StdString) -> Ptr Sparsity' -> IO CInt

casadi__Sparsity__is_row
  :: Sparsity -> IO Bool
casadi__Sparsity__is_row x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Sparsity__is_row errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
sparsity_is_row :: SparsityClass a => a -> IO Bool
sparsity_is_row x = casadi__Sparsity__is_row (castSparsity x)


-- direct wrapper
foreign import ccall unsafe "casadi__Sparsity__is_scalar__0" c_casadi__Sparsity__is_scalar__0
  :: Ptr (Ptr StdString) -> Ptr Sparsity' -> IO CInt

casadi__Sparsity__is_scalar__0
  :: Sparsity -> IO Bool
casadi__Sparsity__is_scalar__0 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Sparsity__is_scalar__0 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
sparsity_is_scalar__0 :: SparsityClass a => a -> IO Bool
sparsity_is_scalar__0 x = casadi__Sparsity__is_scalar__0 (castSparsity x)


-- direct wrapper
foreign import ccall unsafe "casadi__Sparsity__is_scalar__1" c_casadi__Sparsity__is_scalar__1
  :: Ptr (Ptr StdString) -> Ptr Sparsity' -> CInt -> IO CInt

casadi__Sparsity__is_scalar__1
  :: Sparsity -> Bool -> IO Bool
casadi__Sparsity__is_scalar__1 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Sparsity__is_scalar__1 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
sparsity_is_scalar__1 :: SparsityClass a => a -> Bool -> IO Bool
sparsity_is_scalar__1 x = casadi__Sparsity__is_scalar__1 (castSparsity x)


-- direct wrapper
foreign import ccall unsafe "casadi__Sparsity__is_singular" c_casadi__Sparsity__is_singular
  :: Ptr (Ptr StdString) -> Ptr Sparsity' -> IO CInt

casadi__Sparsity__is_singular
  :: Sparsity -> IO Bool
casadi__Sparsity__is_singular x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Sparsity__is_singular errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
sparsity_is_singular :: SparsityClass a => a -> IO Bool
sparsity_is_singular x = casadi__Sparsity__is_singular (castSparsity x)


-- direct wrapper
foreign import ccall unsafe "casadi__Sparsity__is_square" c_casadi__Sparsity__is_square
  :: Ptr (Ptr StdString) -> Ptr Sparsity' -> IO CInt

casadi__Sparsity__is_square
  :: Sparsity -> IO Bool
casadi__Sparsity__is_square x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Sparsity__is_square errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
sparsity_is_square :: SparsityClass a => a -> IO Bool
sparsity_is_square x = casadi__Sparsity__is_square (castSparsity x)


-- direct wrapper
foreign import ccall unsafe "casadi__Sparsity__is_symmetric" c_casadi__Sparsity__is_symmetric
  :: Ptr (Ptr StdString) -> Ptr Sparsity' -> IO CInt

casadi__Sparsity__is_symmetric
  :: Sparsity -> IO Bool
casadi__Sparsity__is_symmetric x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Sparsity__is_symmetric errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
sparsity_is_symmetric :: SparsityClass a => a -> IO Bool
sparsity_is_symmetric x = casadi__Sparsity__is_symmetric (castSparsity x)


-- direct wrapper
foreign import ccall unsafe "casadi__Sparsity__is_transpose" c_casadi__Sparsity__is_transpose
  :: Ptr (Ptr StdString) -> Ptr Sparsity' -> Ptr Sparsity' -> IO CInt

casadi__Sparsity__is_transpose
  :: Sparsity -> Sparsity -> IO Bool
casadi__Sparsity__is_transpose x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Sparsity__is_transpose errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
sparsity_is_transpose :: SparsityClass a => a -> Sparsity -> IO Bool
sparsity_is_transpose x = casadi__Sparsity__is_transpose (castSparsity x)


-- direct wrapper
foreign import ccall unsafe "casadi__Sparsity__is_tril" c_casadi__Sparsity__is_tril
  :: Ptr (Ptr StdString) -> Ptr Sparsity' -> IO CInt

casadi__Sparsity__is_tril
  :: Sparsity -> IO Bool
casadi__Sparsity__is_tril x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Sparsity__is_tril errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
sparsity_is_tril :: SparsityClass a => a -> IO Bool
sparsity_is_tril x = casadi__Sparsity__is_tril (castSparsity x)


-- direct wrapper
foreign import ccall unsafe "casadi__Sparsity__is_triu" c_casadi__Sparsity__is_triu
  :: Ptr (Ptr StdString) -> Ptr Sparsity' -> IO CInt

casadi__Sparsity__is_triu
  :: Sparsity -> IO Bool
casadi__Sparsity__is_triu x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Sparsity__is_triu errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
sparsity_is_triu :: SparsityClass a => a -> IO Bool
sparsity_is_triu x = casadi__Sparsity__is_triu (castSparsity x)


-- direct wrapper
foreign import ccall unsafe "casadi__Sparsity__is_vector" c_casadi__Sparsity__is_vector
  :: Ptr (Ptr StdString) -> Ptr Sparsity' -> IO CInt

casadi__Sparsity__is_vector
  :: Sparsity -> IO Bool
casadi__Sparsity__is_vector x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Sparsity__is_vector errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
sparsity_is_vector :: SparsityClass a => a -> IO Bool
sparsity_is_vector x = casadi__Sparsity__is_vector (castSparsity x)


-- direct wrapper
foreign import ccall unsafe "casadi__Sparsity__largest_first" c_casadi__Sparsity__largest_first
  :: Ptr (Ptr StdString) -> Ptr Sparsity' -> IO (Ptr (StdVec CInt))

casadi__Sparsity__largest_first
  :: Sparsity -> IO (Vector Int)
casadi__Sparsity__largest_first x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Sparsity__largest_first errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
sparsity_largest_first :: SparsityClass a => a -> IO (Vector Int)
sparsity_largest_first x = casadi__Sparsity__largest_first (castSparsity x)


-- direct wrapper
foreign import ccall unsafe "casadi__Sparsity__lower" c_casadi__Sparsity__lower
  :: Ptr (Ptr StdString) -> CInt -> IO (Ptr Sparsity')

casadi__Sparsity__lower
  :: Int -> IO Sparsity
casadi__Sparsity__lower x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Sparsity__lower errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
sparsity_lower :: Int -> IO Sparsity
sparsity_lower = casadi__Sparsity__lower


-- direct wrapper
foreign import ccall unsafe "casadi__Sparsity__makeDense" c_casadi__Sparsity__makeDense
  :: Ptr (Ptr StdString) -> Ptr Sparsity' -> Ptr (Ptr (StdVec CInt)) -> IO (Ptr Sparsity')

casadi__Sparsity__makeDense
  :: Sparsity -> IO (Sparsity, Vector Int)
casadi__Sparsity__makeDense x0 = do
  x0' <- marshal x0
  o1' <- new nullPtr

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Sparsity__makeDense errStrPtrP x0' o1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  o1'' <- peek o1'
  free o1'
  o1''' <- if o1'' == nullPtr then error "swig output o1' was not set in casadi__Sparsity__makeDense/c_casadi__Sparsity__makeDense" else wrapReturn o1''

  return (ret, o1''')



-- classy wrapper
sparsity_makeDense :: SparsityClass a => a -> IO (Sparsity, Vector Int)
sparsity_makeDense x = casadi__Sparsity__makeDense (castSparsity x)


-- direct wrapper
foreign import ccall unsafe "casadi__Sparsity__nnz" c_casadi__Sparsity__nnz
  :: Ptr (Ptr StdString) -> Ptr Sparsity' -> IO CInt

casadi__Sparsity__nnz
  :: Sparsity -> IO Int
casadi__Sparsity__nnz x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Sparsity__nnz errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
sparsity_nnz :: SparsityClass a => a -> IO Int
sparsity_nnz x = casadi__Sparsity__nnz (castSparsity x)


-- direct wrapper
foreign import ccall unsafe "casadi__Sparsity__nnz_diag" c_casadi__Sparsity__nnz_diag
  :: Ptr (Ptr StdString) -> Ptr Sparsity' -> IO CInt

casadi__Sparsity__nnz_diag
  :: Sparsity -> IO Int
casadi__Sparsity__nnz_diag x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Sparsity__nnz_diag errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
sparsity_nnz_diag :: SparsityClass a => a -> IO Int
sparsity_nnz_diag x = casadi__Sparsity__nnz_diag (castSparsity x)


-- direct wrapper
foreign import ccall unsafe "casadi__Sparsity__nnz_lower__0" c_casadi__Sparsity__nnz_lower__0
  :: Ptr (Ptr StdString) -> Ptr Sparsity' -> IO CInt

casadi__Sparsity__nnz_lower__0
  :: Sparsity -> IO Int
casadi__Sparsity__nnz_lower__0 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Sparsity__nnz_lower__0 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
sparsity_nnz_lower__0 :: SparsityClass a => a -> IO Int
sparsity_nnz_lower__0 x = casadi__Sparsity__nnz_lower__0 (castSparsity x)


-- direct wrapper
foreign import ccall unsafe "casadi__Sparsity__nnz_lower__1" c_casadi__Sparsity__nnz_lower__1
  :: Ptr (Ptr StdString) -> Ptr Sparsity' -> CInt -> IO CInt

casadi__Sparsity__nnz_lower__1
  :: Sparsity -> Bool -> IO Int
casadi__Sparsity__nnz_lower__1 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Sparsity__nnz_lower__1 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
sparsity_nnz_lower__1 :: SparsityClass a => a -> Bool -> IO Int
sparsity_nnz_lower__1 x = casadi__Sparsity__nnz_lower__1 (castSparsity x)


-- direct wrapper
foreign import ccall unsafe "casadi__Sparsity__nnz_upper__0" c_casadi__Sparsity__nnz_upper__0
  :: Ptr (Ptr StdString) -> Ptr Sparsity' -> IO CInt

casadi__Sparsity__nnz_upper__0
  :: Sparsity -> IO Int
casadi__Sparsity__nnz_upper__0 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Sparsity__nnz_upper__0 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
sparsity_nnz_upper__0 :: SparsityClass a => a -> IO Int
sparsity_nnz_upper__0 x = casadi__Sparsity__nnz_upper__0 (castSparsity x)


-- direct wrapper
foreign import ccall unsafe "casadi__Sparsity__nnz_upper__1" c_casadi__Sparsity__nnz_upper__1
  :: Ptr (Ptr StdString) -> Ptr Sparsity' -> CInt -> IO CInt

casadi__Sparsity__nnz_upper__1
  :: Sparsity -> Bool -> IO Int
casadi__Sparsity__nnz_upper__1 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Sparsity__nnz_upper__1 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
sparsity_nnz_upper__1 :: SparsityClass a => a -> Bool -> IO Int
sparsity_nnz_upper__1 x = casadi__Sparsity__nnz_upper__1 (castSparsity x)


-- direct wrapper
foreign import ccall unsafe "casadi__Sparsity__numel" c_casadi__Sparsity__numel
  :: Ptr (Ptr StdString) -> Ptr Sparsity' -> IO CInt

casadi__Sparsity__numel
  :: Sparsity -> IO Int
casadi__Sparsity__numel x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Sparsity__numel errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
sparsity_numel :: SparsityClass a => a -> IO Int
sparsity_numel x = casadi__Sparsity__numel (castSparsity x)


-- direct wrapper
foreign import ccall unsafe "casadi__Sparsity__operator__nequals" c_casadi__Sparsity__operator__nequals
  :: Ptr (Ptr StdString) -> Ptr Sparsity' -> Ptr Sparsity' -> IO CInt

casadi__Sparsity__operator__nequals
  :: Sparsity -> Sparsity -> IO Bool
casadi__Sparsity__operator__nequals x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Sparsity__operator__nequals errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
sparsity_operator__nequals :: SparsityClass a => a -> Sparsity -> IO Bool
sparsity_operator__nequals x = casadi__Sparsity__operator__nequals (castSparsity x)


-- direct wrapper
foreign import ccall unsafe "casadi__Sparsity__operator__mul" c_casadi__Sparsity__operator__mul
  :: Ptr (Ptr StdString) -> Ptr Sparsity' -> Ptr Sparsity' -> IO (Ptr Sparsity')

casadi__Sparsity__operator__mul
  :: Sparsity -> Sparsity -> IO Sparsity
casadi__Sparsity__operator__mul x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Sparsity__operator__mul errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
sparsity_operator__mul :: SparsityClass a => a -> Sparsity -> IO Sparsity
sparsity_operator__mul x = casadi__Sparsity__operator__mul (castSparsity x)


-- direct wrapper
foreign import ccall unsafe "casadi__Sparsity__operator__plus" c_casadi__Sparsity__operator__plus
  :: Ptr (Ptr StdString) -> Ptr Sparsity' -> Ptr Sparsity' -> IO (Ptr Sparsity')

casadi__Sparsity__operator__plus
  :: Sparsity -> Sparsity -> IO Sparsity
casadi__Sparsity__operator__plus x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Sparsity__operator__plus errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
sparsity_operator__plus :: SparsityClass a => a -> Sparsity -> IO Sparsity
sparsity_operator__plus x = casadi__Sparsity__operator__plus (castSparsity x)


-- direct wrapper
foreign import ccall unsafe "casadi__Sparsity__operator__equals" c_casadi__Sparsity__operator__equals
  :: Ptr (Ptr StdString) -> Ptr Sparsity' -> Ptr Sparsity' -> IO CInt

casadi__Sparsity__operator__equals
  :: Sparsity -> Sparsity -> IO Bool
casadi__Sparsity__operator__equals x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Sparsity__operator__equals errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
sparsity_operator__equals :: SparsityClass a => a -> Sparsity -> IO Bool
sparsity_operator__equals x = casadi__Sparsity__operator__equals (castSparsity x)


-- direct wrapper
foreign import ccall unsafe "casadi__Sparsity__pattern_inverse" c_casadi__Sparsity__pattern_inverse
  :: Ptr (Ptr StdString) -> Ptr Sparsity' -> IO (Ptr Sparsity')

casadi__Sparsity__pattern_inverse
  :: Sparsity -> IO Sparsity
casadi__Sparsity__pattern_inverse x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Sparsity__pattern_inverse errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
sparsity_pattern_inverse :: SparsityClass a => a -> IO Sparsity
sparsity_pattern_inverse x = casadi__Sparsity__pattern_inverse (castSparsity x)


-- direct wrapper
foreign import ccall unsafe "casadi__Sparsity__pmult__0" c_casadi__Sparsity__pmult__0
  :: Ptr (Ptr StdString) -> Ptr Sparsity' -> Ptr (StdVec CInt) -> IO (Ptr Sparsity')

casadi__Sparsity__pmult__0
  :: Sparsity -> Vector Int -> IO Sparsity
casadi__Sparsity__pmult__0 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Sparsity__pmult__0 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
sparsity_pmult__0 :: SparsityClass a => a -> Vector Int -> IO Sparsity
sparsity_pmult__0 x = casadi__Sparsity__pmult__0 (castSparsity x)


-- direct wrapper
foreign import ccall unsafe "casadi__Sparsity__pmult__1" c_casadi__Sparsity__pmult__1
  :: Ptr (Ptr StdString) -> Ptr Sparsity' -> Ptr (StdVec CInt) -> CInt -> IO (Ptr Sparsity')

casadi__Sparsity__pmult__1
  :: Sparsity -> Vector Int -> Bool -> IO Sparsity
casadi__Sparsity__pmult__1 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Sparsity__pmult__1 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



-- classy wrapper
sparsity_pmult__1 :: SparsityClass a => a -> Vector Int -> Bool -> IO Sparsity
sparsity_pmult__1 x = casadi__Sparsity__pmult__1 (castSparsity x)


-- direct wrapper
foreign import ccall unsafe "casadi__Sparsity__pmult__2" c_casadi__Sparsity__pmult__2
  :: Ptr (Ptr StdString) -> Ptr Sparsity' -> Ptr (StdVec CInt) -> CInt -> CInt -> IO (Ptr Sparsity')

casadi__Sparsity__pmult__2
  :: Sparsity -> Vector Int -> Bool -> Bool -> IO Sparsity
casadi__Sparsity__pmult__2 x0 x1 x2 x3 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Sparsity__pmult__2 errStrPtrP x0' x1' x2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'

  return ret



-- classy wrapper
sparsity_pmult__2 :: SparsityClass a => a -> Vector Int -> Bool -> Bool -> IO Sparsity
sparsity_pmult__2 x = casadi__Sparsity__pmult__2 (castSparsity x)


-- direct wrapper
foreign import ccall unsafe "casadi__Sparsity__pmult__3" c_casadi__Sparsity__pmult__3
  :: Ptr (Ptr StdString) -> Ptr Sparsity' -> Ptr (StdVec CInt) -> CInt -> CInt -> CInt -> IO (Ptr Sparsity')

casadi__Sparsity__pmult__3
  :: Sparsity -> Vector Int -> Bool -> Bool -> Bool -> IO Sparsity
casadi__Sparsity__pmult__3 x0 x1 x2 x3 x4 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3
  x4' <- marshal x4

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Sparsity__pmult__3 errStrPtrP x0' x1' x2' x3' x4'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'
  marshalFree x4 x4'

  return ret



-- classy wrapper
sparsity_pmult__3 :: SparsityClass a => a -> Vector Int -> Bool -> Bool -> Bool -> IO Sparsity
sparsity_pmult__3 x = casadi__Sparsity__pmult__3 (castSparsity x)


-- direct wrapper
foreign import ccall unsafe "casadi__Sparsity__print_compact" c_casadi__Sparsity__print_compact
  :: Ptr (Ptr StdString) -> Ptr Sparsity' -> IO ()

casadi__Sparsity__print_compact
  :: Sparsity -> IO ()
casadi__Sparsity__print_compact x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Sparsity__print_compact errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ()



-- classy wrapper
sparsity_print_compact :: SparsityClass a => a -> IO ()
sparsity_print_compact x = casadi__Sparsity__print_compact (castSparsity x)


-- direct wrapper
foreign import ccall unsafe "casadi__Sparsity__removeDuplicates" c_casadi__Sparsity__removeDuplicates
  :: Ptr (Ptr StdString) -> Ptr Sparsity' -> Ptr (StdVec CInt) -> IO ()

casadi__Sparsity__removeDuplicates
  :: Sparsity -> Vector Int -> IO ()
casadi__Sparsity__removeDuplicates x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Sparsity__removeDuplicates errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ()



-- classy wrapper
sparsity_removeDuplicates :: SparsityClass a => a -> Vector Int -> IO ()
sparsity_removeDuplicates x = casadi__Sparsity__removeDuplicates (castSparsity x)


-- direct wrapper
foreign import ccall unsafe "casadi__Sparsity__repr_el" c_casadi__Sparsity__repr_el
  :: Ptr (Ptr StdString) -> Ptr Sparsity' -> CInt -> IO (Ptr StdString)

casadi__Sparsity__repr_el
  :: Sparsity -> Int -> IO String
casadi__Sparsity__repr_el x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Sparsity__repr_el errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
sparsity_repr_el :: SparsityClass a => a -> Int -> IO String
sparsity_repr_el x = casadi__Sparsity__repr_el (castSparsity x)


-- direct wrapper
foreign import ccall unsafe "casadi__Sparsity__resize" c_casadi__Sparsity__resize
  :: Ptr (Ptr StdString) -> Ptr Sparsity' -> CInt -> CInt -> IO ()

casadi__Sparsity__resize
  :: Sparsity -> Int -> Int -> IO ()
casadi__Sparsity__resize x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Sparsity__resize errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ()



-- classy wrapper
sparsity_resize :: SparsityClass a => a -> Int -> Int -> IO ()
sparsity_resize x = casadi__Sparsity__resize (castSparsity x)


-- direct wrapper
foreign import ccall unsafe "casadi__Sparsity__row" c_casadi__Sparsity__row
  :: Ptr (Ptr StdString) -> Ptr Sparsity' -> CInt -> IO CInt

casadi__Sparsity__row
  :: Sparsity -> Int -> IO Int
casadi__Sparsity__row x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Sparsity__row errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
sparsity_row :: SparsityClass a => a -> Int -> IO Int
sparsity_row x = casadi__Sparsity__row (castSparsity x)


-- direct wrapper
foreign import ccall unsafe "casadi__Sparsity__rowcol" c_casadi__Sparsity__rowcol
  :: Ptr (Ptr StdString) -> Ptr (StdVec CInt) -> Ptr (StdVec CInt) -> CInt -> CInt -> IO (Ptr Sparsity')

casadi__Sparsity__rowcol
  :: Vector Int -> Vector Int -> Int -> Int -> IO Sparsity
casadi__Sparsity__rowcol x0 x1 x2 x3 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Sparsity__rowcol errStrPtrP x0' x1' x2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'

  return ret



-- classy wrapper
sparsity_rowcol :: Vector Int -> Vector Int -> Int -> Int -> IO Sparsity
sparsity_rowcol = casadi__Sparsity__rowcol


-- direct wrapper
foreign import ccall unsafe "casadi__Sparsity__rowsSequential__0" c_casadi__Sparsity__rowsSequential__0
  :: Ptr (Ptr StdString) -> Ptr Sparsity' -> IO CInt

casadi__Sparsity__rowsSequential__0
  :: Sparsity -> IO Bool
casadi__Sparsity__rowsSequential__0 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Sparsity__rowsSequential__0 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
sparsity_rowsSequential__0 :: SparsityClass a => a -> IO Bool
sparsity_rowsSequential__0 x = casadi__Sparsity__rowsSequential__0 (castSparsity x)


-- direct wrapper
foreign import ccall unsafe "casadi__Sparsity__rowsSequential__1" c_casadi__Sparsity__rowsSequential__1
  :: Ptr (Ptr StdString) -> Ptr Sparsity' -> CInt -> IO CInt

casadi__Sparsity__rowsSequential__1
  :: Sparsity -> Bool -> IO Bool
casadi__Sparsity__rowsSequential__1 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Sparsity__rowsSequential__1 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
sparsity_rowsSequential__1 :: SparsityClass a => a -> Bool -> IO Bool
sparsity_rowsSequential__1 x = casadi__Sparsity__rowsSequential__1 (castSparsity x)


-- direct wrapper
foreign import ccall unsafe "casadi__Sparsity__sanity_check__0" c_casadi__Sparsity__sanity_check__0
  :: Ptr (Ptr StdString) -> Ptr Sparsity' -> IO ()

casadi__Sparsity__sanity_check__0
  :: Sparsity -> IO ()
casadi__Sparsity__sanity_check__0 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Sparsity__sanity_check__0 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ()



-- classy wrapper
sparsity_sanity_check__0 :: SparsityClass a => a -> IO ()
sparsity_sanity_check__0 x = casadi__Sparsity__sanity_check__0 (castSparsity x)


-- direct wrapper
foreign import ccall unsafe "casadi__Sparsity__sanity_check__1" c_casadi__Sparsity__sanity_check__1
  :: Ptr (Ptr StdString) -> Ptr Sparsity' -> CInt -> IO ()

casadi__Sparsity__sanity_check__1
  :: Sparsity -> Bool -> IO ()
casadi__Sparsity__sanity_check__1 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Sparsity__sanity_check__1 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ()



-- classy wrapper
sparsity_sanity_check__1 :: SparsityClass a => a -> Bool -> IO ()
sparsity_sanity_check__1 x = casadi__Sparsity__sanity_check__1 (castSparsity x)


-- direct wrapper
foreign import ccall unsafe "casadi__Sparsity__scalar__0" c_casadi__Sparsity__scalar__0
  :: Ptr (Ptr StdString) -> IO (Ptr Sparsity')

casadi__Sparsity__scalar__0
  :: IO Sparsity
casadi__Sparsity__scalar__0  = do


  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Sparsity__scalar__0 errStrPtrP 
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)



  return ret



-- classy wrapper
sparsity_scalar__0 :: IO Sparsity
sparsity_scalar__0 = casadi__Sparsity__scalar__0


-- direct wrapper
foreign import ccall unsafe "casadi__Sparsity__scalar__1" c_casadi__Sparsity__scalar__1
  :: Ptr (Ptr StdString) -> CInt -> IO (Ptr Sparsity')

casadi__Sparsity__scalar__1
  :: Bool -> IO Sparsity
casadi__Sparsity__scalar__1 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Sparsity__scalar__1 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
sparsity_scalar__1 :: Bool -> IO Sparsity
sparsity_scalar__1 = casadi__Sparsity__scalar__1


-- direct wrapper
foreign import ccall unsafe "casadi__Sparsity__scc" c_casadi__Sparsity__scc
  :: Ptr (Ptr StdString) -> Ptr Sparsity' -> Ptr (Ptr (StdVec CInt)) -> Ptr (Ptr (StdVec CInt)) -> IO CInt

casadi__Sparsity__scc
  :: Sparsity -> IO (Int, Vector Int, Vector Int)
casadi__Sparsity__scc x0 = do
  x0' <- marshal x0
  o1' <- new nullPtr
  o2' <- new nullPtr

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Sparsity__scc errStrPtrP x0' o1' o2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  o1'' <- peek o1'
  free o1'
  o1''' <- if o1'' == nullPtr then error "swig output o1' was not set in casadi__Sparsity__scc/c_casadi__Sparsity__scc" else wrapReturn o1''
  o2'' <- peek o2'
  free o2'
  o2''' <- if o2'' == nullPtr then error "swig output o2' was not set in casadi__Sparsity__scc/c_casadi__Sparsity__scc" else wrapReturn o2''

  return (ret, o1''', o2''')



-- classy wrapper
sparsity_scc :: SparsityClass a => a -> IO (Int, Vector Int, Vector Int)
sparsity_scc x = casadi__Sparsity__scc (castSparsity x)


-- direct wrapper
foreign import ccall unsafe "casadi__Sparsity__size__0" c_casadi__Sparsity__size__0
  :: Ptr (Ptr StdString) -> Ptr Sparsity' -> CInt -> IO CInt

casadi__Sparsity__size__0
  :: Sparsity -> Int -> IO Int
casadi__Sparsity__size__0 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Sparsity__size__0 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
sparsity_size__0 :: SparsityClass a => a -> Int -> IO Int
sparsity_size__0 x = casadi__Sparsity__size__0 (castSparsity x)


-- direct wrapper
foreign import ccall unsafe "casadi__Sparsity__size__1" c_casadi__Sparsity__size__1
  :: Ptr (Ptr StdString) -> Ptr Sparsity' -> IO (Ptr (StdPair CInt CInt))

casadi__Sparsity__size__1
  :: Sparsity -> IO (Int, Int)
casadi__Sparsity__size__1 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Sparsity__size__1 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
sparsity_size__1 :: SparsityClass a => a -> IO (Int, Int)
sparsity_size__1 x = casadi__Sparsity__size__1 (castSparsity x)


-- direct wrapper
foreign import ccall unsafe "casadi__Sparsity__size1" c_casadi__Sparsity__size1
  :: Ptr (Ptr StdString) -> Ptr Sparsity' -> IO CInt

casadi__Sparsity__size1
  :: Sparsity -> IO Int
casadi__Sparsity__size1 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Sparsity__size1 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
sparsity_size1 :: SparsityClass a => a -> IO Int
sparsity_size1 x = casadi__Sparsity__size1 (castSparsity x)


-- direct wrapper
foreign import ccall unsafe "casadi__Sparsity__size2" c_casadi__Sparsity__size2
  :: Ptr (Ptr StdString) -> Ptr Sparsity' -> IO CInt

casadi__Sparsity__size2
  :: Sparsity -> IO Int
casadi__Sparsity__size2 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Sparsity__size2 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
sparsity_size2 :: SparsityClass a => a -> IO Int
sparsity_size2 x = casadi__Sparsity__size2 (castSparsity x)


-- direct wrapper
foreign import ccall unsafe "casadi__Sparsity__spy" c_casadi__Sparsity__spy
  :: Ptr (Ptr StdString) -> Ptr Sparsity' -> IO ()

casadi__Sparsity__spy
  :: Sparsity -> IO ()
casadi__Sparsity__spy x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Sparsity__spy errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ()



-- classy wrapper
sparsity_spy :: SparsityClass a => a -> IO ()
sparsity_spy x = casadi__Sparsity__spy (castSparsity x)


-- direct wrapper
foreign import ccall unsafe "casadi__Sparsity__spy_matlab" c_casadi__Sparsity__spy_matlab
  :: Ptr (Ptr StdString) -> Ptr Sparsity' -> Ptr StdString -> IO ()

casadi__Sparsity__spy_matlab
  :: Sparsity -> String -> IO ()
casadi__Sparsity__spy_matlab x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Sparsity__spy_matlab errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ()



-- classy wrapper
sparsity_spy_matlab :: SparsityClass a => a -> String -> IO ()
sparsity_spy_matlab x = casadi__Sparsity__spy_matlab (castSparsity x)


-- direct wrapper
foreign import ccall unsafe "casadi__Sparsity__star_coloring__0" c_casadi__Sparsity__star_coloring__0
  :: Ptr (Ptr StdString) -> Ptr Sparsity' -> IO (Ptr Sparsity')

casadi__Sparsity__star_coloring__0
  :: Sparsity -> IO Sparsity
casadi__Sparsity__star_coloring__0 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Sparsity__star_coloring__0 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
sparsity_star_coloring__0 :: SparsityClass a => a -> IO Sparsity
sparsity_star_coloring__0 x = casadi__Sparsity__star_coloring__0 (castSparsity x)


-- direct wrapper
foreign import ccall unsafe "casadi__Sparsity__star_coloring__1" c_casadi__Sparsity__star_coloring__1
  :: Ptr (Ptr StdString) -> Ptr Sparsity' -> CInt -> IO (Ptr Sparsity')

casadi__Sparsity__star_coloring__1
  :: Sparsity -> Int -> IO Sparsity
casadi__Sparsity__star_coloring__1 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Sparsity__star_coloring__1 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
sparsity_star_coloring__1 :: SparsityClass a => a -> Int -> IO Sparsity
sparsity_star_coloring__1 x = casadi__Sparsity__star_coloring__1 (castSparsity x)


-- direct wrapper
foreign import ccall unsafe "casadi__Sparsity__star_coloring__2" c_casadi__Sparsity__star_coloring__2
  :: Ptr (Ptr StdString) -> Ptr Sparsity' -> CInt -> CInt -> IO (Ptr Sparsity')

casadi__Sparsity__star_coloring__2
  :: Sparsity -> Int -> Int -> IO Sparsity
casadi__Sparsity__star_coloring__2 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Sparsity__star_coloring__2 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



-- classy wrapper
sparsity_star_coloring__2 :: SparsityClass a => a -> Int -> Int -> IO Sparsity
sparsity_star_coloring__2 x = casadi__Sparsity__star_coloring__2 (castSparsity x)


-- direct wrapper
foreign import ccall unsafe "casadi__Sparsity__star_coloring2__0" c_casadi__Sparsity__star_coloring2__0
  :: Ptr (Ptr StdString) -> Ptr Sparsity' -> IO (Ptr Sparsity')

casadi__Sparsity__star_coloring2__0
  :: Sparsity -> IO Sparsity
casadi__Sparsity__star_coloring2__0 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Sparsity__star_coloring2__0 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
sparsity_star_coloring2__0 :: SparsityClass a => a -> IO Sparsity
sparsity_star_coloring2__0 x = casadi__Sparsity__star_coloring2__0 (castSparsity x)


-- direct wrapper
foreign import ccall unsafe "casadi__Sparsity__star_coloring2__1" c_casadi__Sparsity__star_coloring2__1
  :: Ptr (Ptr StdString) -> Ptr Sparsity' -> CInt -> IO (Ptr Sparsity')

casadi__Sparsity__star_coloring2__1
  :: Sparsity -> Int -> IO Sparsity
casadi__Sparsity__star_coloring2__1 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Sparsity__star_coloring2__1 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
sparsity_star_coloring2__1 :: SparsityClass a => a -> Int -> IO Sparsity
sparsity_star_coloring2__1 x = casadi__Sparsity__star_coloring2__1 (castSparsity x)


-- direct wrapper
foreign import ccall unsafe "casadi__Sparsity__star_coloring2__2" c_casadi__Sparsity__star_coloring2__2
  :: Ptr (Ptr StdString) -> Ptr Sparsity' -> CInt -> CInt -> IO (Ptr Sparsity')

casadi__Sparsity__star_coloring2__2
  :: Sparsity -> Int -> Int -> IO Sparsity
casadi__Sparsity__star_coloring2__2 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Sparsity__star_coloring2__2 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



-- classy wrapper
sparsity_star_coloring2__2 :: SparsityClass a => a -> Int -> Int -> IO Sparsity
sparsity_star_coloring2__2 x = casadi__Sparsity__star_coloring2__2 (castSparsity x)


-- direct wrapper
foreign import ccall unsafe "casadi__Sparsity__sub__0" c_casadi__Sparsity__sub__0
  :: Ptr (Ptr StdString) -> Ptr Sparsity' -> Ptr (StdVec CInt) -> Ptr Sparsity' -> Ptr (Ptr (StdVec CInt)) -> IO (Ptr Sparsity')

casadi__Sparsity__sub__0
  :: Sparsity -> Vector Int -> Sparsity -> IO (Sparsity, Vector Int)
casadi__Sparsity__sub__0 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  o3' <- new nullPtr

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Sparsity__sub__0 errStrPtrP x0' x1' x2' o3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  o3'' <- peek o3'
  free o3'
  o3''' <- if o3'' == nullPtr then error "swig output o3' was not set in casadi__Sparsity__sub__0/c_casadi__Sparsity__sub__0" else wrapReturn o3''

  return (ret, o3''')



-- classy wrapper
sparsity_sub__0 :: SparsityClass a => a -> Vector Int -> Sparsity -> IO (Sparsity, Vector Int)
sparsity_sub__0 x = casadi__Sparsity__sub__0 (castSparsity x)


-- direct wrapper
foreign import ccall unsafe "casadi__Sparsity__sub__1" c_casadi__Sparsity__sub__1
  :: Ptr (Ptr StdString) -> Ptr Sparsity' -> Ptr (StdVec CInt) -> Ptr Sparsity' -> Ptr (Ptr (StdVec CInt)) -> CInt -> IO (Ptr Sparsity')

casadi__Sparsity__sub__1
  :: Sparsity -> Vector Int -> Sparsity -> Bool -> IO (Sparsity, Vector Int)
casadi__Sparsity__sub__1 x0 x1 x2 x4 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  o3' <- new nullPtr
  x4' <- marshal x4

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Sparsity__sub__1 errStrPtrP x0' x1' x2' o3' x4'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  o3'' <- peek o3'
  free o3'
  o3''' <- if o3'' == nullPtr then error "swig output o3' was not set in casadi__Sparsity__sub__1/c_casadi__Sparsity__sub__1" else wrapReturn o3''
  marshalFree x4 x4'

  return (ret, o3''')



-- classy wrapper
sparsity_sub__1 :: SparsityClass a => a -> Vector Int -> Sparsity -> Bool -> IO (Sparsity, Vector Int)
sparsity_sub__1 x = casadi__Sparsity__sub__1 (castSparsity x)


-- direct wrapper
foreign import ccall unsafe "casadi__Sparsity__sub__2" c_casadi__Sparsity__sub__2
  :: Ptr (Ptr StdString) -> Ptr Sparsity' -> Ptr (StdVec CInt) -> Ptr (StdVec CInt) -> Ptr (Ptr (StdVec CInt)) -> IO (Ptr Sparsity')

casadi__Sparsity__sub__2
  :: Sparsity -> Vector Int -> Vector Int -> IO (Sparsity, Vector Int)
casadi__Sparsity__sub__2 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  o3' <- new nullPtr

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Sparsity__sub__2 errStrPtrP x0' x1' x2' o3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  o3'' <- peek o3'
  free o3'
  o3''' <- if o3'' == nullPtr then error "swig output o3' was not set in casadi__Sparsity__sub__2/c_casadi__Sparsity__sub__2" else wrapReturn o3''

  return (ret, o3''')



-- classy wrapper
sparsity_sub__2 :: SparsityClass a => a -> Vector Int -> Vector Int -> IO (Sparsity, Vector Int)
sparsity_sub__2 x = casadi__Sparsity__sub__2 (castSparsity x)


-- direct wrapper
foreign import ccall unsafe "casadi__Sparsity__sub__3" c_casadi__Sparsity__sub__3
  :: Ptr (Ptr StdString) -> Ptr Sparsity' -> Ptr (StdVec CInt) -> Ptr (StdVec CInt) -> Ptr (Ptr (StdVec CInt)) -> CInt -> IO (Ptr Sparsity')

casadi__Sparsity__sub__3
  :: Sparsity -> Vector Int -> Vector Int -> Bool -> IO (Sparsity, Vector Int)
casadi__Sparsity__sub__3 x0 x1 x2 x4 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  o3' <- new nullPtr
  x4' <- marshal x4

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Sparsity__sub__3 errStrPtrP x0' x1' x2' o3' x4'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  o3'' <- peek o3'
  free o3'
  o3''' <- if o3'' == nullPtr then error "swig output o3' was not set in casadi__Sparsity__sub__3/c_casadi__Sparsity__sub__3" else wrapReturn o3''
  marshalFree x4 x4'

  return (ret, o3''')



-- classy wrapper
sparsity_sub__3 :: SparsityClass a => a -> Vector Int -> Vector Int -> Bool -> IO (Sparsity, Vector Int)
sparsity_sub__3 x = casadi__Sparsity__sub__3 (castSparsity x)


-- direct wrapper
foreign import ccall unsafe "casadi__Sparsity__transpose__0" c_casadi__Sparsity__transpose__0
  :: Ptr (Ptr StdString) -> Ptr Sparsity' -> Ptr (Ptr (StdVec CInt)) -> IO (Ptr Sparsity')

casadi__Sparsity__transpose__0
  :: Sparsity -> IO (Sparsity, Vector Int)
casadi__Sparsity__transpose__0 x0 = do
  x0' <- marshal x0
  o1' <- new nullPtr

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Sparsity__transpose__0 errStrPtrP x0' o1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  o1'' <- peek o1'
  free o1'
  o1''' <- if o1'' == nullPtr then error "swig output o1' was not set in casadi__Sparsity__transpose__0/c_casadi__Sparsity__transpose__0" else wrapReturn o1''

  return (ret, o1''')



-- classy wrapper
sparsity_transpose__0 :: SparsityClass a => a -> IO (Sparsity, Vector Int)
sparsity_transpose__0 x = casadi__Sparsity__transpose__0 (castSparsity x)


-- direct wrapper
foreign import ccall unsafe "casadi__Sparsity__transpose__1" c_casadi__Sparsity__transpose__1
  :: Ptr (Ptr StdString) -> Ptr Sparsity' -> Ptr (Ptr (StdVec CInt)) -> CInt -> IO (Ptr Sparsity')

casadi__Sparsity__transpose__1
  :: Sparsity -> Bool -> IO (Sparsity, Vector Int)
casadi__Sparsity__transpose__1 x0 x2 = do
  x0' <- marshal x0
  o1' <- new nullPtr
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Sparsity__transpose__1 errStrPtrP x0' o1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  o1'' <- peek o1'
  free o1'
  o1''' <- if o1'' == nullPtr then error "swig output o1' was not set in casadi__Sparsity__transpose__1/c_casadi__Sparsity__transpose__1" else wrapReturn o1''
  marshalFree x2 x2'

  return (ret, o1''')



-- classy wrapper
sparsity_transpose__1 :: SparsityClass a => a -> Bool -> IO (Sparsity, Vector Int)
sparsity_transpose__1 x = casadi__Sparsity__transpose__1 (castSparsity x)


-- direct wrapper
foreign import ccall unsafe "casadi__Sparsity__triplet__0" c_casadi__Sparsity__triplet__0
  :: Ptr (Ptr StdString) -> CInt -> CInt -> Ptr (StdVec CInt) -> Ptr (StdVec CInt) -> IO (Ptr Sparsity')

casadi__Sparsity__triplet__0
  :: Int -> Int -> Vector Int -> Vector Int -> IO Sparsity
casadi__Sparsity__triplet__0 x0 x1 x2 x3 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Sparsity__triplet__0 errStrPtrP x0' x1' x2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'

  return ret



-- classy wrapper
sparsity_triplet__0 :: Int -> Int -> Vector Int -> Vector Int -> IO Sparsity
sparsity_triplet__0 = casadi__Sparsity__triplet__0


-- direct wrapper
foreign import ccall unsafe "casadi__Sparsity__triplet__1" c_casadi__Sparsity__triplet__1
  :: Ptr (Ptr StdString) -> CInt -> CInt -> Ptr (StdVec CInt) -> Ptr (StdVec CInt) -> Ptr (Ptr (StdVec CInt)) -> CInt -> IO (Ptr Sparsity')

casadi__Sparsity__triplet__1
  :: Int -> Int -> Vector Int -> Vector Int -> Bool -> IO (Sparsity, Vector Int)
casadi__Sparsity__triplet__1 x0 x1 x2 x3 x5 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3
  o4' <- new nullPtr
  x5' <- marshal x5

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Sparsity__triplet__1 errStrPtrP x0' x1' x2' x3' o4' x5'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'
  o4'' <- peek o4'
  free o4'
  o4''' <- if o4'' == nullPtr then error "swig output o4' was not set in casadi__Sparsity__triplet__1/c_casadi__Sparsity__triplet__1" else wrapReturn o4''
  marshalFree x5 x5'

  return (ret, o4''')



-- classy wrapper
sparsity_triplet__1 :: Int -> Int -> Vector Int -> Vector Int -> Bool -> IO (Sparsity, Vector Int)
sparsity_triplet__1 = casadi__Sparsity__triplet__1


-- direct wrapper
foreign import ccall unsafe "casadi__Sparsity__uni_coloring__0" c_casadi__Sparsity__uni_coloring__0
  :: Ptr (Ptr StdString) -> Ptr Sparsity' -> IO (Ptr Sparsity')

casadi__Sparsity__uni_coloring__0
  :: Sparsity -> IO Sparsity
casadi__Sparsity__uni_coloring__0 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Sparsity__uni_coloring__0 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
sparsity_uni_coloring__0 :: SparsityClass a => a -> IO Sparsity
sparsity_uni_coloring__0 x = casadi__Sparsity__uni_coloring__0 (castSparsity x)


-- direct wrapper
foreign import ccall unsafe "casadi__Sparsity__uni_coloring__1" c_casadi__Sparsity__uni_coloring__1
  :: Ptr (Ptr StdString) -> Ptr Sparsity' -> Ptr Sparsity' -> IO (Ptr Sparsity')

casadi__Sparsity__uni_coloring__1
  :: Sparsity -> Sparsity -> IO Sparsity
casadi__Sparsity__uni_coloring__1 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Sparsity__uni_coloring__1 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
sparsity_uni_coloring__1 :: SparsityClass a => a -> Sparsity -> IO Sparsity
sparsity_uni_coloring__1 x = casadi__Sparsity__uni_coloring__1 (castSparsity x)


-- direct wrapper
foreign import ccall unsafe "casadi__Sparsity__uni_coloring__2" c_casadi__Sparsity__uni_coloring__2
  :: Ptr (Ptr StdString) -> Ptr Sparsity' -> Ptr Sparsity' -> CInt -> IO (Ptr Sparsity')

casadi__Sparsity__uni_coloring__2
  :: Sparsity -> Sparsity -> Int -> IO Sparsity
casadi__Sparsity__uni_coloring__2 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Sparsity__uni_coloring__2 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



-- classy wrapper
sparsity_uni_coloring__2 :: SparsityClass a => a -> Sparsity -> Int -> IO Sparsity
sparsity_uni_coloring__2 x = casadi__Sparsity__uni_coloring__2 (castSparsity x)


-- direct wrapper
foreign import ccall unsafe "casadi__Sparsity__unit" c_casadi__Sparsity__unit
  :: Ptr (Ptr StdString) -> CInt -> CInt -> IO (Ptr Sparsity')

casadi__Sparsity__unit
  :: Int -> Int -> IO Sparsity
casadi__Sparsity__unit x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Sparsity__unit errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
sparsity_unit :: Int -> Int -> IO Sparsity
sparsity_unit = casadi__Sparsity__unit


-- direct wrapper
foreign import ccall unsafe "casadi__Sparsity__unite" c_casadi__Sparsity__unite
  :: Ptr (Ptr StdString) -> Ptr Sparsity' -> Ptr Sparsity' -> IO (Ptr Sparsity')

casadi__Sparsity__unite
  :: Sparsity -> Sparsity -> IO Sparsity
casadi__Sparsity__unite x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Sparsity__unite errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
sparsity_unite :: SparsityClass a => a -> Sparsity -> IO Sparsity
sparsity_unite x = casadi__Sparsity__unite (castSparsity x)


-- direct wrapper
foreign import ccall unsafe "casadi__Sparsity__upper" c_casadi__Sparsity__upper
  :: Ptr (Ptr StdString) -> CInt -> IO (Ptr Sparsity')

casadi__Sparsity__upper
  :: Int -> IO Sparsity
casadi__Sparsity__upper x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Sparsity__upper errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
sparsity_upper :: Int -> IO Sparsity
sparsity_upper = casadi__Sparsity__upper

