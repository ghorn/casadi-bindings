{-# OPTIONS_GHC -Wall -fno-cse -fno-warn-orphans #-}

module Casadi.DMatrix
       ( DMatrix
       , dsparsify
       , ddata
       ) where

import qualified Data.Vector as V
import System.IO.Unsafe ( unsafePerformIO )
import Linear.Conjugate ( Conjugate(..) )
import qualified Data.Serialize as S
import qualified Data.Binary as B
import Data.Vector.Binary () -- instances

import Casadi.Core.Classes.DMatrix
import Casadi.Core.Classes.Sparsity ( Sparsity )
import qualified Casadi.Core.Tools as C

import Casadi.Overloading ( Fmod(..), ArcTan2(..), SymOrd(..), Erf(..) )
import Casadi.CMatrix ( CMatrix(..) )

getWith :: Monad m => m Sparsity -> m (V.Vector Double) -> m DMatrix
getWith get getVector = do
  sp <- get
  data' <- getVector
  return (fromSparseData sp data')
putWith :: Monad m => (Sparsity -> m ()) -> (V.Vector Double -> m ()) -> DMatrix -> m ()
putWith put putVector x = do
  put (sparsity x)
  putVector (ddata x)

-- Data.Vector.Cereal looks deprecated, it's not in master anymore
instance S.Serialize DMatrix where
  put = putWith S.put (S.put . V.toList)
  get = getWith S.get (fmap V.fromList S.get)
instance B.Binary DMatrix where
  put = putWith B.put B.put
  get = getWith B.get B.get

instance Conjugate DMatrix where
  conjugate = id

fromSparseData :: Sparsity -> V.Vector Double -> DMatrix
fromSparseData s d = unsafePerformIO (dmatrix__6 s d)
{-# NOINLINE fromSparseData #-}

dsparsify :: DMatrix -> DMatrix
dsparsify x = unsafePerformIO (dmatrix_zz_sparsify__0 x)
{-# NOINLINE dsparsify #-}

ddata :: DMatrix -> V.Vector Double
ddata x = unsafePerformIO (dmatrix_data__0 x)
{-# NOINLINE ddata #-}

instance Show DMatrix where
  show x = unsafePerformIO (dmatrix_getDescription x)
  {-# NOINLINE show #-}

instance Eq DMatrix where
  x == y = unsafePerformIO (dmatrix_zz_isEqual__0 x y)
  {-# NOINLINE (==) #-}

instance CMatrix DMatrix where
  veccat x = unsafePerformIO (dmatrix_zz_veccat x)
  {-# NOINLINE veccat #-}
  ----  vertsplit = vertslice
  vertsplit x ks = unsafePerformIO (dmatrix_zz_vertsplit x ks)
  {-# NOINLINE vertsplit #-}
  vertcat x = unsafePerformIO (dmatrix_zz_vertcat x)
  {-# NOINLINE vertcat #-}
  --  horzsplit = horzslice
  horzsplit x ks = unsafePerformIO (dmatrix_zz_horzsplit x ks)
  {-# NOINLINE horzsplit #-}
  horzcat x = unsafePerformIO (dmatrix_zz_horzcat x)
  {-# NOINLINE horzcat #-}
  size1 x = unsafePerformIO (dmatrix_size1 x)
  {-# NOINLINE size1 #-}
  size2 x = unsafePerformIO (dmatrix_size2 x)
  {-# NOINLINE size2 #-}
  numel x = unsafePerformIO (dmatrix_numel__1 x)
  {-# NOINLINE numel #-}
  mm x y = unsafePerformIO (dmatrix_zz_mtimes__1 x y)
  {-# NOINLINE mm #-}
  innerProd x y = unsafePerformIO (dmatrix_zz_inner_prod x y)
  {-# NOINLINE innerProd #-}
  trans x = unsafePerformIO (dmatrix_T x)
  {-# NOINLINE trans #-}
  diag x = unsafePerformIO (dmatrix_zz_diag x)
  {-# NOINLINE diag #-}
  eye n = unsafePerformIO (dmatrix_eye n)
  {-# NOINLINE eye #-}
  ones (r,c) = unsafePerformIO (dmatrix_ones__3 r c)
  {-# NOINLINE ones #-}
  zeros (r,c) = unsafePerformIO (dmatrix_zeros__3 r c)
  {-# NOINLINE zeros #-}
  zerosSp sp = unsafePerformIO (dmatrix_zeros__0 sp)
  {-# NOINLINE zerosSp #-}
  solve x y = unsafePerformIO (C.solve__3 x y)
  {-# NOINLINE solve #-}
  indexed m sx sy = unsafePerformIO (dmatrix_getSub__3 m False sx sy)
  {-# NOINLINE indexed #-}
  sparsity x = unsafePerformIO (dmatrix_sparsityRef__0 x)
  {-# NOINLINE sparsity #-}
  getNZ m s = unsafePerformIO (dmatrix_getNZ__1 m False s)
  {-# NOINLINE getNZ #-}
  setNZ m y s = dmatrix_setNZ__1 m y False s
  triu x = unsafePerformIO (dmatrix_zz_triu__0 x)
  {-# NOINLINE triu #-}
  tril x = unsafePerformIO (dmatrix_zz_tril__0 x)
  {-# NOINLINE tril #-}
  triu2symm  x = unsafePerformIO (dmatrix_zz_triu2symm x)
  {-# NOINLINE triu2symm #-}
  tril2symm x = unsafePerformIO (dmatrix_zz_tril2symm x)
  {-# NOINLINE tril2symm #-}
  copy m = dmatrix__10 m
  dense x = unsafePerformIO (dmatrix_zz_dense x)
  {-# NOINLINE dense #-}
  fromDMatrix = id
  fromDVector x = unsafePerformIO (dmatrix__4 x)
  {-# NOINLINE fromDVector #-}

instance Num DMatrix where
  (+) x y = unsafePerformIO (dmatrix_zz_plus x y)
  {-# NOINLINE (+) #-}
  (-) x y = unsafePerformIO (dmatrix_zz_minus x y)
  {-# NOINLINE (-) #-}
  (*) x y = unsafePerformIO (dmatrix_zz_times x y)
  {-# NOINLINE (*) #-}
  fromInteger x = unsafePerformIO (dmatrix__5 (fromInteger x :: Double))
  {-# NOINLINE fromInteger #-}
  abs x = unsafePerformIO (dmatrix_zz_abs x)
  {-# NOINLINE abs #-}
  signum x = unsafePerformIO (dmatrix_zz_sign x)
  {-# NOINLINE signum #-}

instance Fractional DMatrix where
  (/) x y = unsafePerformIO (dmatrix___truediv____0 x y)
  {-# NOINLINE (/) #-}
  fromRational x = unsafePerformIO (dmatrix__5 (fromRational x :: Double))
  {-# NOINLINE fromRational #-}

instance Floating DMatrix where
  pi = unsafePerformIO (dmatrix__5 (pi :: Double))
  {-# NOINLINE pi #-}
  (**) x y = unsafePerformIO (dmatrix_zz_power x y)
  {-# NOINLINE (**) #-}
  exp x   = unsafePerformIO (dmatrix_zz_exp x)
  {-# NOINLINE exp #-}
  log x   = unsafePerformIO (dmatrix_zz_log x)
  {-# NOINLINE log #-}
  sin x   = unsafePerformIO (dmatrix_zz_sin x)
  {-# NOINLINE sin #-}
  cos x   = unsafePerformIO (dmatrix_zz_cos x)
  {-# NOINLINE cos #-}
  tan x   = unsafePerformIO (dmatrix_zz_tan x)
  {-# NOINLINE tan #-}
  asin x  = unsafePerformIO (dmatrix_zz_asin x)
  {-# NOINLINE asin #-}
  atan x  = unsafePerformIO (dmatrix_zz_atan x)
  {-# NOINLINE atan #-}
  acos x  = unsafePerformIO (dmatrix_zz_acos x)
  {-# NOINLINE acos #-}
  sinh x  = unsafePerformIO (dmatrix_zz_sinh x)
  {-# NOINLINE sinh #-}
  cosh x  = unsafePerformIO (dmatrix_zz_cosh x)
  {-# NOINLINE cosh #-}
  tanh x  = unsafePerformIO (dmatrix_zz_tanh x)
  {-# NOINLINE tanh #-}
  asinh x = unsafePerformIO (dmatrix_zz_asinh x)
  {-# NOINLINE asinh #-}
  atanh x = unsafePerformIO (dmatrix_zz_atanh x)
  {-# NOINLINE atanh #-}
  acosh x = unsafePerformIO (dmatrix_zz_acosh x)
  {-# NOINLINE acosh #-}

instance Fmod DMatrix where
  fmod x y = unsafePerformIO (dmatrix_zz_mod x y)
  {-# NOINLINE fmod #-}

instance ArcTan2 DMatrix where
  arctan2 x y = unsafePerformIO (dmatrix_zz_atan2 x y)
  {-# NOINLINE arctan2 #-}

instance SymOrd DMatrix where
  x `leq` y = unsafePerformIO (dmatrix_zz_le x y)
  {-# NOINLINE leq #-}
  x `geq` y = unsafePerformIO (dmatrix_zz_ge x y)
  {-# NOINLINE geq #-}
  x  `eq` y = unsafePerformIO (dmatrix_zz_eq x y)
  {-# NOINLINE eq #-}

instance Erf DMatrix where
  erf x = unsafePerformIO (dmatrix_zz_erf x)
  {-# NOINLINE erf #-}
  erfinv x = unsafePerformIO (dmatrix_zz_erfinv x)
  {-# NOINLINE erfinv #-}
