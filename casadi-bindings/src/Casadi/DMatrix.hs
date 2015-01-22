{-# OPTIONS_GHC -Wall -fno-cse -fno-warn-orphans #-}

module Casadi.DMatrix
       ( DMatrix, dcrs, dmm, dvector, ddata, ddiag
       , ddense, dsparsify, dtrans
       , dtriu, dtril
       , dtriu2symm, dtril2symm
       , dsize, dsize1, dsize2, dnumel
       , dvertcat, dhorzcat, dveccat, dvertsplit, dhorzsplit
       , deye, dones, dzeros, dzerosSp
       , dindexed
       , dgetNZ, dsetNZ
       , dcopy
       ) where

import qualified Data.Vector as V
import System.IO.Unsafe ( unsafePerformIO )
import Linear.Conjugate ( Conjugate(..) )

import Casadi.Core.Classes.Sparsity
import Casadi.Core.Classes.DMatrix
import Casadi.Core.Classes.Slice ( Slice )

import Casadi.Overloading ( Fmod(..), ArcTan2(..), SymOrd(..) )

instance Conjugate DMatrix where
  conjugate = id

instance Show DMatrix where
  show x = unsafePerformIO (dmatrix_getDescription x)
  {-# NOINLINE show #-}

instance Eq DMatrix where
  x == y = unsafePerformIO (dmatrix_zz_isEqual__0 x y)
  {-# NOINLINE (==) #-}

-- | matrix matrix product
dmm :: DMatrix -> DMatrix -> DMatrix
dmm x y = unsafePerformIO (dmatrix_zz_mtimes__1 x y)
{-# NOINLINE dmm #-}

-- | transpose
dtrans :: DMatrix -> DMatrix
dtrans x = unsafePerformIO (dmatrix_T x)
{-# NOINLINE dtrans #-}

ddense :: DMatrix -> DMatrix
ddense x = unsafePerformIO (dmatrix_zz_dense x)
{-# NOINLINE ddense #-}

dsparsify :: DMatrix -> DMatrix
dsparsify x = unsafePerformIO (dmatrix_zz_sparsify__0 x)
{-# NOINLINE dsparsify #-}

dcrs :: DMatrix -> Sparsity
dcrs x = unsafePerformIO (dmatrix_sparsityRef__0 x)
{-# NOINLINE dcrs #-}

ddiag :: DMatrix -> DMatrix
ddiag x = unsafePerformIO (dmatrix_zz_diag x)
{-# NOINLINE ddiag #-}

-- | from vector
dvector :: V.Vector Double -> DMatrix
dvector x = unsafePerformIO (dmatrix__4 x)
{-# NOINLINE dvector #-}

ddata :: DMatrix -> V.Vector Double
ddata x = unsafePerformIO (dmatrix_data__0 x)
{-# NOINLINE ddata #-}

dsize :: DMatrix -> Int
dsize x = unsafePerformIO (dmatrix_size__1 x)
{-# NOINLINE dsize #-}

dsize1 :: DMatrix -> Int
dsize1 x = unsafePerformIO (dmatrix_size1 x)
{-# NOINLINE dsize1 #-}

dsize2 :: DMatrix -> Int
dsize2 x = unsafePerformIO (dmatrix_size2 x)
{-# NOINLINE dsize2 #-}

dnumel :: DMatrix -> Int
dnumel x = unsafePerformIO (dmatrix_numel__1 x)
{-# NOINLINE dnumel #-}

dvertcat :: V.Vector DMatrix -> DMatrix
dvertcat x = unsafePerformIO (dmatrix_zz_vertcat x)
{-# NOINLINE dvertcat #-}

dveccat :: V.Vector DMatrix -> DMatrix
dveccat x = unsafePerformIO (dmatrix_zz_veccat x)
{-# NOINLINE dveccat #-}

dvertsplit :: DMatrix -> V.Vector Int -> V.Vector DMatrix
dvertsplit x ks = unsafePerformIO (dmatrix_zz_vertsplit x ks)
{-# NOINLINE dvertsplit #-}

dhorzsplit :: DMatrix -> V.Vector Int -> V.Vector DMatrix
dhorzsplit x ks = unsafePerformIO (dmatrix_zz_horzsplit x ks)
{-# NOINLINE dhorzsplit #-}

dhorzcat :: V.Vector DMatrix -> DMatrix
dhorzcat x = unsafePerformIO (dmatrix_zz_horzcat x)
{-# NOINLINE dhorzcat #-}

dtriu :: DMatrix -> DMatrix
dtriu x = unsafePerformIO (dmatrix_zz_triu__0 x)
{-# NOINLINE dtriu #-}

dtril :: DMatrix -> DMatrix
dtril x = unsafePerformIO (dmatrix_zz_tril__0 x)
{-# NOINLINE dtril #-}

dtriu2symm :: DMatrix -> DMatrix
dtriu2symm x = unsafePerformIO (dmatrix_zz_triu2symm x)
{-# NOINLINE dtriu2symm #-}

dtril2symm :: DMatrix -> DMatrix
dtril2symm x = unsafePerformIO (dmatrix_zz_tril2symm x)
{-# NOINLINE dtril2symm #-}

deye :: Int -> DMatrix
deye n = unsafePerformIO (dmatrix_eye n)
{-# NOINLINE deye #-}

dones :: (Int,Int) -> DMatrix
dones (r,c) = unsafePerformIO (dmatrix_ones__3 r c)
{-# NOINLINE dones #-}

dzeros :: (Int,Int) -> DMatrix
dzeros (r,c) = unsafePerformIO (dmatrix_zeros__3 r c)
{-# NOINLINE dzeros #-}

dzerosSp :: Sparsity -> DMatrix
dzerosSp sp = unsafePerformIO (dmatrix_zeros__0 sp)
{-# NOINLINE dzerosSp #-}

dindexed :: DMatrix -> Slice -> Slice -> DMatrix
dindexed m sx sy = unsafePerformIO (dmatrix_getSub__3 m False sx sy)
{-# NOINLINE dindexed #-}

dgetNZ :: DMatrix -> Slice -> DMatrix
dgetNZ m s = unsafePerformIO (dmatrix_getNZ__1 m False s)
{-# NOINLINE dgetNZ #-}

dsetNZ :: DMatrix -> DMatrix -> Slice -> IO ()
dsetNZ m y s = dmatrix_setNZ__1 m y False s

dcopy :: DMatrix -> IO DMatrix
dcopy m = dmatrix__10 m

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
