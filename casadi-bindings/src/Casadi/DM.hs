{-# OPTIONS_GHC -Wall -fno-cse -fno-warn-orphans #-}

module Casadi.DM
       ( DM
       , dsparsify
       , dnonzeros
       ) where

import qualified Data.Vector as V
import System.IO.Unsafe ( unsafePerformIO )
import Linear.Conjugate ( Conjugate(..) )
import qualified Data.Serialize as S
import qualified Data.Binary as B
import Data.Vector.Binary () -- instances

import Casadi.Core.Classes.DM
import Casadi.Core.Classes.Sparsity ( Sparsity )
import qualified Casadi.Core.Tools as C

import Casadi.Matrix ( CMatrix(..) )
import Casadi.GenericType ( fromGType )
import Casadi.Overloading ( Fmod(..), ArcTan2(..), SymOrd(..), Erf(..) )
import Casadi.Viewable ( Viewable(..) )

getWith :: Monad m => m Sparsity -> m (V.Vector Double) -> m DM
getWith get getVector = do
  sp <- get
  data' <- getVector
  return (fromSparseData sp data')
putWith :: Monad m => (Sparsity -> m ()) -> (V.Vector Double -> m ()) -> DM -> m ()
putWith put putVector x = do
  put (sparsity x)
  putVector (dnonzeros x)

-- Data.Vector.Cereal looks deprecated, it's not in master anymore
instance S.Serialize DM where
  put = putWith S.put (S.put . V.toList)
  get = getWith S.get (fmap V.fromList S.get)
instance B.Binary DM where
  put = putWith B.put B.put
  get = getWith B.get B.get

instance Conjugate DM where
  conjugate = id

instance Viewable DM where
  vvertcat = vertcat
  vvertsplit = vertsplit
  vsize1 = size1
  vsize2 = size2
  vrecoverDimension _ dim = zeros dim

fromSparseData :: Sparsity -> V.Vector Double -> DM
fromSparseData s d = unsafePerformIO (dm__6 s (fromDVector d))
{-# NOINLINE fromSparseData #-}

dsparsify :: DM -> DM
dsparsify x = unsafePerformIO (C.casadi_sparsify__2 x)
{-# NOINLINE dsparsify #-}

dnonzeros :: DM -> V.Vector Double
dnonzeros x = unsafePerformIO (dm_get_nonzeros x)
{-# NOINLINE dnonzeros #-}

instance Show DM where
  show x = unsafePerformIO (dm_get_str__0 x)
  {-# NOINLINE show #-}

instance Eq DM where
  x == y = unsafePerformIO (C.casadi_is_equal__4 x y)
  {-# NOINLINE (==) #-}

instance CMatrix DM where
  blocksplit x ix iy = unsafePerformIO (C.casadi_blocksplit__7 x ix iy)
  {-# NOINLINE blocksplit #-}
  blockcat x = unsafePerformIO (C.casadi_blockcat__1 x)
  {-# NOINLINE blockcat #-}
  veccat x = unsafePerformIO (C.casadi_veccat__1 x)
  {-# NOINLINE veccat #-}
  ----  vertsplit = vertslice
  vertsplit x ks = unsafePerformIO (C.casadi_vertsplit__5 x ks)
  {-# NOINLINE vertsplit #-}
  vertcat x = unsafePerformIO (C.casadi_vertcat__1 x)
  {-# NOINLINE vertcat #-}
  --  horzsplit = horzslice
  horzsplit x ks = unsafePerformIO (C.casadi_horzsplit__5 x ks)
  {-# NOINLINE horzsplit #-}
  horzcat x = unsafePerformIO (C.casadi_horzcat__1 x)
  {-# NOINLINE horzcat #-}
  size1 x = unsafePerformIO (dm_size1 x)
  {-# NOINLINE size1 #-}
  size2 x = unsafePerformIO (dm_size2 x)
  {-# NOINLINE size2 #-}
  numel x = unsafePerformIO (dm_numel x)
  {-# NOINLINE numel #-}
  mtimes x y = unsafePerformIO (C.casadi_mtimes__3 x y)
  {-# NOINLINE mtimes #-}
  dot x y = unsafePerformIO (C.casadi_dot__1 x y)
  {-# NOINLINE dot #-}
  sum1 x = unsafePerformIO (C.casadi_sum1__1 x)
  {-# NOINLINE sum1 #-}
  sum2 x = unsafePerformIO (C.casadi_sum2__1 x)
  {-# NOINLINE sum2 #-}
  trans x = unsafePerformIO (dm_T x)
  {-# NOINLINE trans #-}
  diag x = unsafePerformIO (C.casadi_diag__1 x)
  {-# NOINLINE diag #-}
  eye n = unsafePerformIO (dm_eye n)
  {-# NOINLINE eye #-}
  ones (r,c) = unsafePerformIO (dm_ones__4 r c)
  {-# NOINLINE ones #-}
  zeros (r,c) = unsafePerformIO (dm_zeros__4 r c)
  {-# NOINLINE zeros #-}
  zerosSp sp = unsafePerformIO (dm_zeros__1 sp)
  {-# NOINLINE zerosSp #-}
  solve x y s m = unsafePerformIO (mapM fromGType m >>= C.casadi_solve__4 x y s)
  {-# NOINLINE solve #-}
  solve' x y = unsafePerformIO (C.casadi_solve__5 x y)
  {-# NOINLINE solve' #-}
  indexed m spx spy = unsafePerformIO (dm_get__3 m False spx spy)
  {-# NOINLINE indexed #-}
  sparsity x = unsafePerformIO (dm_get_sparsity x)
  {-# NOINLINE sparsity #-}
  getNZ m sp = unsafePerformIO (dm_get_nz__1 m False sp)
  {-# NOINLINE getNZ #-}
  setNZ m y s = dm_set_nz__1 m y False s
  triu x = unsafePerformIO (C.casadi_triu__2 x)
  {-# NOINLINE triu #-}
  tril x = unsafePerformIO (C.casadi_tril__2 x)
  {-# NOINLINE tril #-}
  triu2symm x = unsafePerformIO (C.casadi_triu2symm__1 x)
  {-# NOINLINE triu2symm #-}
  tril2symm x = unsafePerformIO (C.casadi_tril2symm__1 x)
  {-# NOINLINE tril2symm #-}
  copy m = dm__9 m
  densify x = unsafePerformIO (C.casadi_densify__1 x)
  {-# NOINLINE densify #-}
  fromDM = id
  fromDVector x = unsafePerformIO (dm__4 (V.singleton x) >>= dm_T)
  {-# NOINLINE fromDVector #-}
  fromDouble x = unsafePerformIO (dm__5 x)
  {-# NOINLINE fromDouble #-}
  reshape x s = unsafePerformIO (C.casadi_reshape__4 x s)
  {-# NOINLINE reshape #-}
  conditional x0 x1 x2 = unsafePerformIO (C.casadi_conditional__2 x0 x1 x2)
  {-# NOINLINE conditional #-}
  conditional' x0 x1 x2 x3 = unsafePerformIO (C.casadi_conditional__3 x0 x1 x2 x3)
  {-# NOINLINE conditional' #-}
  inv x = unsafePerformIO (C.casadi_inv__5 x)
  {-# NOINLINE inv #-}
  pinv x = unsafePerformIO (C.casadi_pinv__5 x)
  {-# NOINLINE pinv #-}
  pinv' x n o = unsafePerformIO (mapM fromGType o >>= C.casadi_pinv__4 x n)
  {-# NOINLINE pinv' #-}
  cmax x y = unsafePerformIO (C.casadi_fmax__2 x y)
  {-# NOINLINE cmax #-}
  cmin x y = unsafePerformIO (C.casadi_fmin__2 x y)
  {-# NOINLINE cmin #-}
  cand x y = unsafePerformIO (C.casadi_and__2 x y)
  {-# NOINLINE cand #-}
  cor x y = unsafePerformIO (C.casadi_or__2 x y)
  {-# NOINLINE cor #-}
  repmat x (s1, s2) = unsafePerformIO (C.casadi_repmat__5 x s1 s2)
  {-# NOINLINE repmat #-}
  printme x y = unsafePerformIO (dm_printme x y)
  {-# NOINLINE printme #-}

  sumSquare x = unsafePerformIO (C.casadi_sumsqr__1 x)
  {-# NOINLINE sumSquare #-}
  invSkew x = unsafePerformIO (C.casadi_inv_skew__1 x)
  {-# NOINLINE invSkew #-}
  cnot x = unsafePerformIO (C.casadi_not__2 x)
  {-# NOINLINE cnot #-}
  nullspace x = unsafePerformIO (C.casadi_nullspace__1 x)
  {-# NOINLINE nullspace #-}
  norm1 x = unsafePerformIO (C.casadi_norm_1__1 x)
  {-# NOINLINE norm1 #-}
  norm2 x = unsafePerformIO (C.casadi_norm_2__1 x)
  {-# NOINLINE norm2 #-}
  normFro x = unsafePerformIO (C.casadi_norm_fro__1 x)
  {-# NOINLINE normFro #-}
  normInf x = unsafePerformIO (C.casadi_norm_inf__1 x)
  {-# NOINLINE normInf #-}
  kron x y = unsafePerformIO (C.casadi_kron__1 x y)
  {-# NOINLINE kron #-}
  mldivide x y = unsafePerformIO (C.casadi_mldivide__1 x y)
  {-# NOINLINE mldivide #-}
  mrdivide x y = unsafePerformIO (C.casadi_mrdivide__1 x y)
  {-# NOINLINE mrdivide #-}
  mpower x y = unsafePerformIO (C.casadi_mpower__1 x y)
  {-# NOINLINE mpower #-}
  ceil' x = unsafePerformIO (C.casadi_ceil__2 x)
  {-# NOINLINE ceil' #-}
  floor' x = unsafePerformIO (C.casadi_floor__2 x)
  {-# NOINLINE floor' #-}


instance Num DM where
  (+) x y = unsafePerformIO (C.casadi_plus__2 x y)
  {-# NOINLINE (+) #-}
  (-) x y = unsafePerformIO (C.casadi_minus__2 x y)
  {-# NOINLINE (-) #-}
  (*) x y = unsafePerformIO (C.casadi_times__2 x y)
  {-# NOINLINE (*) #-}
  fromInteger x = fromDouble (fromInteger x :: Double)
  {-# NOINLINE fromInteger #-}
  abs x = unsafePerformIO (C.casadi_abs__2 x)
  {-# NOINLINE abs #-}
  signum x = unsafePerformIO (C.casadi_sign__2 x)
  {-# NOINLINE signum #-}

instance Fractional DM where
  (/) x y = unsafePerformIO (C.casadi_rdivide__2 x y)
  {-# NOINLINE (/) #-}
  fromRational x = fromDouble (fromRational x :: Double)
  {-# NOINLINE fromRational #-}

instance Floating DM where
  pi = fromDouble (pi :: Double)
  {-# NOINLINE pi #-}
  (**) x y = unsafePerformIO (C.casadi_power__2 x y)
  {-# NOINLINE (**) #-}
  exp x   = unsafePerformIO (C.casadi_exp__2 x)
  {-# NOINLINE exp #-}
  log x   = unsafePerformIO (C.casadi_log__2 x)
  {-# NOINLINE log #-}
  sin x   = unsafePerformIO (C.casadi_sin__2 x)
  {-# NOINLINE sin #-}
  cos x   = unsafePerformIO (C.casadi_cos__2 x)
  {-# NOINLINE cos #-}
  tan x   = unsafePerformIO (C.casadi_tan__2 x)
  {-# NOINLINE tan #-}
  asin x  = unsafePerformIO (C.casadi_asin__2 x)
  {-# NOINLINE asin #-}
  atan x  = unsafePerformIO (C.casadi_atan__2 x)
  {-# NOINLINE atan #-}
  acos x  = unsafePerformIO (C.casadi_acos__2 x)
  {-# NOINLINE acos #-}
  sinh x  = unsafePerformIO (C.casadi_sinh__2 x)
  {-# NOINLINE sinh #-}
  cosh x  = unsafePerformIO (C.casadi_cosh__2 x)
  {-# NOINLINE cosh #-}
  tanh x  = unsafePerformIO (C.casadi_tanh__2 x)
  {-# NOINLINE tanh #-}
  asinh x = unsafePerformIO (C.casadi_asinh__2 x)
  {-# NOINLINE asinh #-}
  atanh x = unsafePerformIO (C.casadi_atanh__2 x)
  {-# NOINLINE atanh #-}
  acosh x = unsafePerformIO (C.casadi_acosh__2 x)
  {-# NOINLINE acosh #-}

instance Fmod DM where
  fmod x y = unsafePerformIO (C.casadi_mod__2 x y)
  {-# NOINLINE fmod #-}

instance ArcTan2 DM where
  arctan2 x y = unsafePerformIO (C.casadi_atan2__2 x y)
  {-# NOINLINE arctan2 #-}

instance SymOrd DM where
  x `leq` y = unsafePerformIO (C.casadi_le__2 x y)
  {-# NOINLINE leq #-}
  x `lt` y = unsafePerformIO (C.casadi_lt__2 x y)
  {-# NOINLINE lt #-}
  x `geq` y = unsafePerformIO (C.casadi_ge__2 x y)
  {-# NOINLINE geq #-}
  x `gt` y = unsafePerformIO (C.casadi_gt__2 x y)
  {-# NOINLINE gt #-}
  x  `eq` y = unsafePerformIO (C.casadi_eq__2 x y)
  {-# NOINLINE eq #-}
  x `ne` y = unsafePerformIO (C.casadi_ne__2 x y)
  {-# NOINLINE ne #-}
  max' x y = cmax x y
  {-# NOINLINE max' #-}
  min' x y = cmin x y
  {-# NOINLINE min' #-}

instance Erf DM where
  erf x = unsafePerformIO (C.casadi_erf__2 x)
  {-# NOINLINE erf #-}
  erfinv x = unsafePerformIO (C.casadi_erfinv__2 x)
  {-# NOINLINE erfinv #-}
