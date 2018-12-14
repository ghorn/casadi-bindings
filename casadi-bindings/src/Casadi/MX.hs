{-# OPTIONS_GHC -Wall -fno-cse -fno-warn-orphans #-}

module Casadi.MX
       ( MX
       , attachAssert
       ) where

import qualified Data.Vector as V
import Linear.Conjugate ( Conjugate(..) )
import System.IO.Unsafe ( unsafePerformIO )

import qualified Casadi.Core.Classes.Function as C
import Casadi.Core.Classes.MX
import qualified Casadi.Core.Tools as C

import Casadi.DM ()
import Casadi.GenericType ( fromGType )
import Casadi.Matrix ( CMatrix(..), SMatrix(..) )
import Casadi.Overloading ( Fmod(..), ArcTan2(..), SymOrd(..), Erf(..) )
import Casadi.SharedObject ( castSharedObject )
import Casadi.Viewable ( Viewable(..) )

instance Conjugate MX where
  conjugate = id

instance Eq MX where
  x == y = unsafePerformIO (C.casadi_is_equal__8 x y)
  {-# NOINLINE (==) #-}

instance Show MX where
  show x = show (castSharedObject x)
  {-# NOINLINE show #-}

instance Viewable MX where
  vvertcat = vertcat
  vvertsplit = vertsplit
  vsize1 = size1
  vsize2 = size2
  vrecoverDimension _ dim = zeros dim

attachAssert :: MX -> MX -> String -> MX
attachAssert x y msg = unsafePerformIO (mx_attachAssert__1 x y msg)
{-# NOINLINE attachAssert #-}

--sparsify :: MX -> MX
--sparsify x = unsafePerformIO (C.casadi_sparsify__0__3 x)
--{-# NOINLINE sparsify #-}

instance CMatrix MX where
  blocksplit x ix iy = unsafePerformIO (C.casadi_blocksplit__15 x ix iy)
  {-# NOINLINE blocksplit #-}
  blockcat x = unsafePerformIO (C.casadi_blockcat__3 x)
  {-# NOINLINE blockcat #-}
  veccat x = unsafePerformIO (C.casadi_veccat__3 x)
  {-# NOINLINE veccat #-}
  --  vertsplit = vertslice
  vertsplit x ks = unsafePerformIO (C.casadi_vertsplit__11 x ks)
  {-# NOINLINE vertsplit #-}
  vertcat x = unsafePerformIO (C.casadi_vertcat__3 x)
  {-# NOINLINE vertcat #-}
  --  horzsplit = horzslice
  horzsplit x ks = unsafePerformIO (C.casadi_horzsplit__11 x ks)
  {-# NOINLINE horzsplit #-}
  horzcat x = unsafePerformIO (C.casadi_horzcat__3 x)
  {-# NOINLINE horzcat #-}
  size1 x = unsafePerformIO (mx_size1 x)
  {-# NOINLINE size1 #-}
  size2 x = unsafePerformIO (mx_size2 x)
  {-# NOINLINE size2 #-}
  numel x = unsafePerformIO (mx_numel x)
  {-# NOINLINE numel #-}
  mtimes x y = unsafePerformIO (C.casadi_mtimes__7 x y)
  {-# NOINLINE mtimes #-}
  dot x y = unsafePerformIO (C.casadi_dot__3 x y)
  {-# NOINLINE dot #-}
  sum1 x = unsafePerformIO (C.casadi_sum1__3 x)
  {-# NOINLINE sum1 #-}
  sum2 x = unsafePerformIO (C.casadi_sum2__3 x)
  {-# NOINLINE sum2 #-}
  trans x = unsafePerformIO (mx_T x)
  {-# NOINLINE trans #-}
  diag x = unsafePerformIO (C.casadi_diag__3 x)
  {-# NOINLINE diag #-}
  eye n = unsafePerformIO (mx_eye n)
  {-# NOINLINE eye #-}
  ones (r,c) = unsafePerformIO (mx_ones__4 r c)
  {-# NOINLINE ones #-}
  zeros (r,c) = unsafePerformIO (mx_zeros__4 r c)
  {-# NOINLINE zeros #-}
  zerosSp sp = unsafePerformIO (mx_zeros__1 sp)
  {-# NOINLINE zerosSp #-}
  solve x y s m = unsafePerformIO (mapM fromGType m >>= C.casadi_solve__10 x y s)
  {-# NOINLINE solve #-}
  solve' x y = unsafePerformIO (C.casadi_solve__11 x y)
  {-# NOINLINE solve' #-}
  indexed m spx spy = unsafePerformIO (mx_get__3 m False spx spy)
  {-# NOINLINE indexed #-}
  sparsity x = unsafePerformIO (mx_get_sparsity x)
  {-# NOINLINE sparsity #-}
  getNZ m sp = unsafePerformIO (mx_get_nz__1 m False sp)
  {-# NOINLINE getNZ #-}
  setNZ m y s = mx_set_nz__1 m y False s
  triu x = unsafePerformIO (C.casadi_triu__6 x)
  {-# NOINLINE triu #-}
  tril x = unsafePerformIO (C.casadi_tril__6 x)
  {-# NOINLINE tril #-}
  triu2symm x = unsafePerformIO (C.casadi_triu2symm__3 x)
  {-# NOINLINE triu2symm #-}
  tril2symm x = unsafePerformIO (C.casadi_tril2symm__3 x)
  {-# NOINLINE tril2symm #-}
  copy m = mx__2 m
  densify x = unsafePerformIO (C.casadi_densify__3 x)
  {-# NOINLINE densify #-}
  fromDM x = unsafePerformIO (mx__0 x)
  {-# NOINLINE fromDM #-}
  fromDVector x = fromDM (fromDVector x)
  {-# NOINLINE fromDVector #-}
  fromDouble x = unsafePerformIO (mx__3 x)
  {-# NOINLINE fromDouble #-}
  reshape x s = unsafePerformIO (C.casadi_reshape__10 x s)
  {-# NOINLINE reshape #-}
  conditional x0 x1 x2 = unsafePerformIO (C.casadi_conditional__6 x0 x1 x2)
  {-# NOINLINE conditional #-}
  conditional' x0 x1 x2 x3 = unsafePerformIO (C.casadi_conditional__7 x0 x1 x2 x3)
  {-# NOINLINE conditional' #-}
  inv x = unsafePerformIO (C.casadi_inv__11 x)
  {-# NOINLINE inv #-}
  pinv x = unsafePerformIO (C.casadi_pinv__11 x)
  {-# NOINLINE pinv #-}
  pinv' x n o = unsafePerformIO (mapM fromGType o >>= C.casadi_pinv__10 x n)
  {-# NOINLINE pinv' #-}
  cmax x y = unsafePerformIO (C.casadi_fmax__4 x y)
  {-# NOINLINE cmax #-}
  cmin x y = unsafePerformIO (C.casadi_fmin__4 x y)
  {-# NOINLINE cmin #-}
  cand x y = unsafePerformIO (C.casadi_and__4 x y)
  {-# NOINLINE cand #-}
  cor x y = unsafePerformIO (C.casadi_or__4 x y)
  {-# NOINLINE cor #-}
  repmat x (s1, s2) = unsafePerformIO (C.casadi_repmat__11 x s1 s2)
  {-# NOINLINE repmat #-}
  printme x y = unsafePerformIO (mx_printme x y)
  {-# NOINLINE printme #-}

  sumSquare x = unsafePerformIO (C.casadi_sumsqr__3 x)
  {-# NOINLINE sumSquare #-}
  invSkew x = unsafePerformIO (C.casadi_inv_skew__3 x)
  {-# NOINLINE invSkew #-}
  cnot x = unsafePerformIO (C.casadi_not__4 x)
  {-# NOINLINE cnot #-}
  nullspace x = unsafePerformIO (C.casadi_nullspace__3 x)
  {-# NOINLINE nullspace #-}
  norm1 x = unsafePerformIO (C.casadi_norm_1__3 x)
  {-# NOINLINE norm1 #-}
  norm2 x = unsafePerformIO (C.casadi_norm_2__3 x)
  {-# NOINLINE norm2 #-}
  normFro x = unsafePerformIO (C.casadi_norm_fro__3 x)
  {-# NOINLINE normFro #-}
  normInf x = unsafePerformIO (C.casadi_norm_inf__3 x)
  {-# NOINLINE normInf #-}
  kron x y = unsafePerformIO (C.casadi_kron__3 x y)
  {-# NOINLINE kron #-}
  mldivide x y = unsafePerformIO (C.casadi_mldivide__3 x y)
  {-# NOINLINE mldivide #-}
  mrdivide x y = unsafePerformIO (C.casadi_mrdivide__3 x y)
  {-# NOINLINE mrdivide #-}
  mpower x y = unsafePerformIO (C.casadi_mpower__3 x y)
  {-# NOINLINE mpower #-}
  ceil' x = unsafePerformIO (C.casadi_ceil__4 x)
  {-# NOINLINE ceil' #-}
  floor' x = unsafePerformIO (C.casadi_floor__4 x)
  {-# NOINLINE floor' #-}


instance SMatrix MX where
  gradient x y = unsafePerformIO (C.casadi_gradient__3 x y)
  {-# NOINLINE gradient #-}
  jacobian x y = unsafePerformIO (C.casadi_jacobian__6 x y)
  {-# NOINLINE jacobian #-}
  hessian expr args = unsafePerformIO $ do
    grad <- mx__7
    hess <- C.casadi_hessian__3 expr args grad
    return (hess, grad)
  {-# NOINLINE hessian #-}
  jtimes x y z = unsafePerformIO (C.casadi_jtimes__6 x y z)
  {-# NOINLINE jtimes #-}
  forward x y z w = unsafePerformIO (mapM fromGType w >>= C.casadi_forward__7 x y z)
  {-# NOINLINE forward #-}
  reverse x y z w = unsafePerformIO (mapM fromGType w >>= C.casadi_reverse__7 x y z)
  {-# NOINLINE reverse #-}

  sym = mx_sym__8

  toFunction n x y opts0 = do
    opts <- mapM fromGType opts0
    C.function__5 n x y opts

  toFunction' n nxx nyy opts0 = do
    let (nx, x) = V.unzip nxx
        (ny, y) = V.unzip nyy
    opts <- mapM fromGType opts0
    C.function__3 n x y nx ny opts

  callSym f ins = unsafePerformIO (C.function_call__9 f ins)
  {-# NOINLINE callSym #-}
  callSym' f ins = unsafePerformIO (C.function_call__0 f ins)
  {-# NOINLINE callSym' #-}


instance Num MX where
  (+) x y = unsafePerformIO (C.casadi_plus__4 x y)
  {-# NOINLINE (+) #-}
  (-) x y = unsafePerformIO (C.casadi_minus__4 x y)
  {-# NOINLINE (-) #-}
  (*) x y = unsafePerformIO (C.casadi_times__4 x y)
  {-# NOINLINE (*) #-}
  fromInteger x = fromDouble (fromInteger x :: Double)
  {-# NOINLINE fromInteger #-}
  abs x = unsafePerformIO (C.casadi_abs__4 x)
  {-# NOINLINE abs #-}
  signum x = unsafePerformIO (C.casadi_sign__4 x)
  {-# NOINLINE signum #-}

instance Fractional MX where
  (/) x y = unsafePerformIO (C.casadi_rdivide__4 x y)
  {-# NOINLINE (/) #-}
  fromRational x = fromDouble (fromRational x :: Double)
  {-# NOINLINE fromRational #-}

instance Floating MX where
  pi = fromDouble (pi :: Double)
  {-# NOINLINE pi #-}
  (**) x y = unsafePerformIO (C.casadi_power__4 x y)
  {-# NOINLINE (**) #-}
  exp x   = unsafePerformIO (C.casadi_exp__4 x)
  {-# NOINLINE exp #-}
  log x   = unsafePerformIO (C.casadi_log__4 x)
  {-# NOINLINE log #-}
  sin x   = unsafePerformIO (C.casadi_sin__4 x)
  {-# NOINLINE sin #-}
  cos x   = unsafePerformIO (C.casadi_cos__4 x)
  {-# NOINLINE cos #-}
  tan x   = unsafePerformIO (C.casadi_tan__4 x)
  {-# NOINLINE tan #-}
  asin x  = unsafePerformIO (C.casadi_asin__4 x)
  {-# NOINLINE asin #-}
  atan x  = unsafePerformIO (C.casadi_atan__4 x)
  {-# NOINLINE atan #-}
  acos x  = unsafePerformIO (C.casadi_acos__4 x)
  {-# NOINLINE acos #-}
  sinh x  = unsafePerformIO (C.casadi_sinh__4 x)
  {-# NOINLINE sinh #-}
  cosh x  = unsafePerformIO (C.casadi_cosh__4 x)
  {-# NOINLINE cosh #-}
  tanh x  = unsafePerformIO (C.casadi_tanh__4 x)
  {-# NOINLINE tanh #-}
  asinh x = unsafePerformIO (C.casadi_asinh__4 x)
  {-# NOINLINE asinh #-}
  atanh x = unsafePerformIO (C.casadi_atanh__4 x)
  {-# NOINLINE atanh #-}
  acosh x = unsafePerformIO (C.casadi_acosh__4 x)
  {-# NOINLINE acosh #-}

instance Fmod MX where
  fmod x y = unsafePerformIO (C.casadi_mod__4 x y)
  {-# NOINLINE fmod #-}

instance ArcTan2 MX where
  arctan2 x y = unsafePerformIO (C.casadi_atan2__4 x y)
  {-# NOINLINE arctan2 #-}

instance SymOrd MX where
  x `leq` y = unsafePerformIO (C.casadi_le__4 x y)
  {-# NOINLINE leq #-}
  x `lt` y = unsafePerformIO (C.casadi_lt__4 x y)
  {-# NOINLINE lt #-}
  x `geq` y = unsafePerformIO (C.casadi_ge__4 x y)
  {-# NOINLINE geq #-}
  x `gt` y = unsafePerformIO (C.casadi_gt__4 x y)
  {-# NOINLINE gt #-}
  x  `eq` y = unsafePerformIO (C.casadi_eq__4 x y)
  {-# NOINLINE eq #-}
  x `ne` y = unsafePerformIO (C.casadi_ne__4 x y)
  {-# NOINLINE ne #-}
  max' x y = cmax x y
  {-# NOINLINE max' #-}
  min' x y = cmin x y
  {-# NOINLINE min' #-}

instance Erf MX where
  erf x = unsafePerformIO (C.casadi_erf__4 x)
  {-# NOINLINE erf #-}
  erfinv x = unsafePerformIO (C.casadi_erfinv__4 x)
  {-# NOINLINE erfinv #-}
