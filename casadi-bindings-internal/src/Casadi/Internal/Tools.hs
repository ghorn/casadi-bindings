{-# OPTIONS_GHC -Wall #-}

module Casadi.Wrappers.Tools
       (
         addMultiple,
         addMultiple',
         addMultiple'',
         addMultiple''',
         addMultiple'''',
         addMultiple''''',
         adj,
         adj',
         adj'',
         blkdiag,
         blkdiag',
         blkdiag'',
         blkdiag''',
         blkdiag'''',
         blockcat,
         blockcat',
         blockcat'',
         blockcat''',
         blocksplit,
         blocksplit',
         blocksplit'',
         blocksplit''',
         blocksplit'''',
         blocksplit''''',
         blocksplit'''''',
         blocksplit''''''',
         blocksplit'''''''',
         blocksplit''''''''',
         blocksplit'''''''''',
         blocksplit''''''''''',
         blocksplit'''''''''''',
         blocksplit''''''''''''',
         blocksplit'''''''''''''',
         blocksplit''''''''''''''',
         cofactor,
         cofactor',
         cofactor'',
         collocationInterpolators,
         collocationPoints,
         collocationPoints',
         complement,
         compress,
         compress',
         countNodes,
         countNodes',
         createParent,
         createParent',
         createParent'',
         cross,
         cross',
         cross'',
         cross''',
         cross'''',
         cross''''',
         cross'''''',
         cross''''''',
         dense,
         dense',
         dense'',
         dense''',
         dependsOn,
         dependsOn',
         det,
         det',
         det'',
         det''',
         diag,
         diag',
         diag'',
         diag''',
         eig_symbolic,
         evalf,
         evalf',
         expand,
         explicitRK,
         explicitRK',
         explicitRK'',
         explicitRK''',
         extractShared,
         extractShared',
         extractShared'',
         extractShared''',
         extractShared'''',
         extractShared''''',
         gauss_quadrature,
         gauss_quadrature',
         gauss_quadrature'',
         getFree,
         getMinor,
         getMinor',
         getMinor'',
         getOperatorRepresentation,
         getOperatorRepresentation',
         getSchemeEntryDoc,
         getSchemeEntryEnum,
         getSchemeEntryEnumName,
         getSchemeEntryName,
         getSchemeEntryNames,
         getSchemeName,
         getSchemeSize,
         getSymbols,
         getSymbols',
         getSymbols'',
         gradient,
         gradient',
         graph_substitute,
         graph_substitute',
         hash_combine,
         hash_sparsity,
         heaviside,
         hessian,
         hessian',
         horzcat,
         horzcat',
         horzcat'',
         horzcat''',
         horzcat'''',
         horzsplit,
         horzsplit',
         horzsplit'',
         horzsplit''',
         horzsplit'''',
         horzsplit''''',
         horzsplit'''''',
         horzsplit''''''',
         horzsplit'''''''',
         horzsplit''''''''',
         horzsplit'''''''''',
         horzsplit''''''''''',
         horzsplit'''''''''''',
         if_else,
         if_else',
         inner_prod,
         inner_prod',
         inner_prod'',
         inner_prod''',
         inv,
         inv',
         inv'',
         inv''',
         isDecreasing,
         isDecreasing',
         isEqual,
         isEqual',
         isEqual'',
         isEqual''',
         isIncreasing,
         isIncreasing',
         isMonotone,
         isMonotone',
         isNonDecreasing,
         isNonDecreasing',
         isNonIncreasing,
         isNonIncreasing',
         isRegular,
         isRegular',
         isStrictlyMonotone,
         isStrictlyMonotone',
         jacobian,
         jacobian',
         jacobianTimesVector,
         jacobianTimesVector',
         kron,
         kron',
         kron'',
         kron''',
         linspace,
         linspace',
         linspace'',
         logic_and,
         logic_and',
         logic_and'',
         logic_and''',
         logic_and'''',
         logic_and''''',
         logic_and'''''',
         logic_not,
         logic_not',
         logic_not'',
         logic_not''',
         logic_not'''',
         logic_not''''',
         logic_not'''''',
         logic_or,
         logic_or',
         logic_or'',
         logic_or''',
         logic_or'''',
         logic_or''''',
         logic_or'''''',
         lookupvector,
         matrix_expand,
         matrix_expand',
         matrix_expand'',
         matrix_expand''',
         mtaylor,
         mtaylor',
         mtaylor'',
         mul,
         mul',
         mul'',
         mul''',
         mul'''',
         mul''''',
         mul'''''',
         mul''''''',
         mul'''''''',
         mul''''''''',
         mul'''''''''',
         mul''''''''''',
         mul'''''''''''',
         norm_1,
         norm_1',
         norm_1'',
         norm_1''',
         norm_2,
         norm_2',
         norm_2'',
         norm_2''',
         norm_F,
         norm_F',
         norm_F'',
         norm_F''',
         norm_inf,
         norm_inf',
         norm_inf'',
         norm_inf''',
         nullspace,
         nullspace',
         nullspace'',
         nullspace''',
         outer_prod,
         outer_prod',
         outer_prod'',
         outer_prod''',
         pinv,
         pinv',
         pinv'',
         poly_coeff,
         poly_roots,
         polyval,
         polyval',
         polyval'',
         polyval''',
         printCompact',
         printCompact''',
         project,
         project',
         project'',
         pw_const,
         pw_lin,
         qr,
         qr',
         qr'',
         ramp,
         rank,
         rectangle,
         repmat,
         repmat',
         repmat'',
         repmat''',
         reshape,
         reshape',
         reshape'',
         reshape''',
         reshape'''',
         reshape''''',
         reshape'''''',
         reshape''''''',
         simplify,
         simplify',
         simplify'',
         solve,
         solve',
         solve'',
         solve''',
         sparse,
         sparse',
         sparse'',
         sparse''',
         sparse'''',
         sparse''''',
         sprank,
         sprank',
         sprank'',
         spy,
         substitute,
         substitute',
         substitute'',
         substitute''',
         substituteInPlace,
         substituteInPlace',
         substituteInPlace'',
         substituteInPlace''',
         substituteInPlace'''',
         substituteInPlace''''',
         substituteInPlace'''''',
         substituteInPlace''''''',
         substituteInPlace'''''''',
         substituteInPlace''''''''',
         sumAll,
         sumAll',
         sumAll'',
         sumAll''',
         sumCols,
         sumCols',
         sumCols'',
         sumCols''',
         sumRows,
         sumRows',
         sumRows'',
         sumRows''',
         tangent,
         tangent',
         taylor,
         taylor',
         taylor'',
         trace,
         trace',
         trace'',
         trace''',
         transpose,
         transpose',
         transpose'',
         transpose''',
         transpose'''',
         triangle,
         tril,
         tril',
         tril'',
         tril''',
         tril'''',
         tril''''',
         tril2symm,
         tril2symm',
         tril2symm'',
         tril2symm''',
         triu,
         triu',
         triu'',
         triu''',
         triu'''',
         triu''''',
         triu2symm,
         triu2symm',
         triu2symm'',
         triu2symm''',
         unite,
         unite',
         unite'',
         unite''',
         vec,
         vec',
         vec'',
         vec''',
         vec'''',
         vecNZ,
         vecNZ',
         vecNZ'',
         vecNZ''',
         vecNZcat,
         vecNZcat',
         vecNZcat'',
         vecNZcat''',
         veccat,
         veccat',
         veccat'',
         veccat''',
         vertcat,
         vertcat',
         vertcat'',
         vertcat''',
         vertcat'''',
         vertsplit,
         vertsplit',
         vertsplit'',
         vertsplit''',
         vertsplit'''',
         vertsplit''''',
         vertsplit'''''',
         vertsplit''''''',
         vertsplit'''''''',
         vertsplit''''''''',
         vertsplit'''''''''',
         vertsplit''''''''''',
         vertsplit'''''''''''',
       ) where


import Data.Vector ( Vector )
import Foreign.C.Types
import Foreign.Ptr ( Ptr )

import Casadi.Wrappers.Data
import Casadi.Wrappers.Enums
import Casadi.Wrappers.CToolsInstances ( )
import Casadi.MarshalTypes ( CppVec, StdString' )
import Casadi.Marshal ( withMarshal )
import Casadi.WrapReturn ( WrapReturn(..) )

foreign import ccall unsafe "CasADi__getSchemeEntryName" c_CasADi__getSchemeEntryName
  :: CInt -> CInt -> IO (Ptr StdString')
getSchemeEntryName
  :: InputOutputScheme -> Int -> IO String
getSchemeEntryName x0 x1 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  c_CasADi__getSchemeEntryName x0' x1' >>= wrapReturn

foreign import ccall unsafe "CasADi__getSchemeEntryDoc" c_CasADi__getSchemeEntryDoc
  :: CInt -> CInt -> IO (Ptr StdString')
getSchemeEntryDoc
  :: InputOutputScheme -> Int -> IO String
getSchemeEntryDoc x0 x1 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  c_CasADi__getSchemeEntryDoc x0' x1' >>= wrapReturn

foreign import ccall unsafe "CasADi__getSchemeEntryEnumName" c_CasADi__getSchemeEntryEnumName
  :: CInt -> CInt -> IO (Ptr StdString')
getSchemeEntryEnumName
  :: InputOutputScheme -> Int -> IO String
getSchemeEntryEnumName x0 x1 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  c_CasADi__getSchemeEntryEnumName x0' x1' >>= wrapReturn

foreign import ccall unsafe "CasADi__getSchemeEntryEnum" c_CasADi__getSchemeEntryEnum
  :: CInt -> Ptr StdString' -> IO CInt
getSchemeEntryEnum
  :: InputOutputScheme -> String -> IO Int
getSchemeEntryEnum x0 x1 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  c_CasADi__getSchemeEntryEnum x0' x1' >>= wrapReturn

foreign import ccall unsafe "CasADi__getSchemeSize" c_CasADi__getSchemeSize
  :: CInt -> IO CInt
getSchemeSize
  :: InputOutputScheme -> IO Int
getSchemeSize x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__getSchemeSize x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__getSchemeName" c_CasADi__getSchemeName
  :: CInt -> IO (Ptr StdString')
getSchemeName
  :: InputOutputScheme -> IO String
getSchemeName x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__getSchemeName x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__getSchemeEntryNames" c_CasADi__getSchemeEntryNames
  :: CInt -> IO (Ptr StdString')
getSchemeEntryNames
  :: InputOutputScheme -> IO String
getSchemeEntryNames x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__getSchemeEntryNames x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__complement" c_CasADi__complement
  :: Ptr (CppVec CInt) -> CInt -> IO (Ptr (CppVec CInt))
{-|
>Returns the list of all i in [0,size[ not found in supplied list.
>
>The supplied vector may contain duplicates and may be non-monotonous The
>supplied vector will be checked for bounds The result vector is guaranteed
>to be monotonously increasing
-}
complement
  :: Vector Int -> Int -> IO (Vector Int)
complement x0 x1 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  c_CasADi__complement x0' x1' >>= wrapReturn

foreign import ccall unsafe "CasADi__lookupvector" c_CasADi__lookupvector
  :: Ptr (CppVec CInt) -> CInt -> IO (Ptr (CppVec CInt))
{-|
>Returns a vector for quickly looking up entries of supplied list.
>
>lookupvector[i]!=-1 <=> v contains i v[lookupvector[i]] == i <=> v contains
>i
>
>Duplicates are treated by looking up last occurence
-}
lookupvector
  :: Vector Int -> Int -> IO (Vector Int)
lookupvector x0 x1 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  c_CasADi__lookupvector x0' x1' >>= wrapReturn

foreign import ccall unsafe "CasADi__isIncreasing" c_CasADi__isIncreasing
  :: Ptr (CppVec CInt) -> IO CInt
{-|
>Check if the vector is strictly increasing.
-}
isIncreasing
  :: Vector Int -> IO Bool
isIncreasing x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__isIncreasing x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__isDecreasing" c_CasADi__isDecreasing
  :: Ptr (CppVec CInt) -> IO CInt
{-|
>Check if the vector is strictly decreasing.
-}
isDecreasing
  :: Vector Int -> IO Bool
isDecreasing x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__isDecreasing x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__isNonIncreasing" c_CasADi__isNonIncreasing
  :: Ptr (CppVec CInt) -> IO CInt
{-|
>Check if the vector is non-increasing.
-}
isNonIncreasing
  :: Vector Int -> IO Bool
isNonIncreasing x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__isNonIncreasing x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__isNonDecreasing" c_CasADi__isNonDecreasing
  :: Ptr (CppVec CInt) -> IO CInt
{-|
>Check if the vector is non-decreasing.
-}
isNonDecreasing
  :: Vector Int -> IO Bool
isNonDecreasing x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__isNonDecreasing x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__isMonotone" c_CasADi__isMonotone
  :: Ptr (CppVec CInt) -> IO CInt
{-|
>Check if the vector is monotone.
-}
isMonotone
  :: Vector Int -> IO Bool
isMonotone x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__isMonotone x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__isStrictlyMonotone" c_CasADi__isStrictlyMonotone
  :: Ptr (CppVec CInt) -> IO CInt
{-|
>Check if the vector is strictly monotone.
-}
isStrictlyMonotone
  :: Vector Int -> IO Bool
isStrictlyMonotone x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__isStrictlyMonotone x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__isRegular" c_CasADi__isRegular
  :: Ptr (CppVec CInt) -> IO CInt
{-|
>>  bool CasADi::isRegular(const Matrix< DataType > &ex)
>
>>  bool CasADi::isRegular(const MX &ex)
>
>>  bool CasADi::isRegular(const SXElement &ex)
>
>>  bool CasADi::isRegular(const SX &ex)
>------------------------------------------------------------------------
>
>[DEPRECATED]
>
>>  bool CasADi::isRegular(const std::vector< T > &v)
>------------------------------------------------------------------------
>
>Checks if vector does not contain NaN or Inf.
-}
isRegular
  :: Vector Int -> IO Bool
isRegular x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__isRegular x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__isIncreasing_TIC" c_CasADi__isIncreasing_TIC
  :: Ptr (CppVec CDouble) -> IO CInt
isIncreasing'
  :: Vector Double -> IO Bool
isIncreasing' x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__isIncreasing_TIC x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__isDecreasing_TIC" c_CasADi__isDecreasing_TIC
  :: Ptr (CppVec CDouble) -> IO CInt
isDecreasing'
  :: Vector Double -> IO Bool
isDecreasing' x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__isDecreasing_TIC x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__isNonIncreasing_TIC" c_CasADi__isNonIncreasing_TIC
  :: Ptr (CppVec CDouble) -> IO CInt
isNonIncreasing'
  :: Vector Double -> IO Bool
isNonIncreasing' x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__isNonIncreasing_TIC x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__isNonDecreasing_TIC" c_CasADi__isNonDecreasing_TIC
  :: Ptr (CppVec CDouble) -> IO CInt
isNonDecreasing'
  :: Vector Double -> IO Bool
isNonDecreasing' x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__isNonDecreasing_TIC x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__isMonotone_TIC" c_CasADi__isMonotone_TIC
  :: Ptr (CppVec CDouble) -> IO CInt
isMonotone'
  :: Vector Double -> IO Bool
isMonotone' x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__isMonotone_TIC x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__isStrictlyMonotone_TIC" c_CasADi__isStrictlyMonotone_TIC
  :: Ptr (CppVec CDouble) -> IO CInt
isStrictlyMonotone'
  :: Vector Double -> IO Bool
isStrictlyMonotone' x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__isStrictlyMonotone_TIC x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__isRegular_TIC" c_CasADi__isRegular_TIC
  :: Ptr (CppVec CDouble) -> IO CInt
isRegular'
  :: Vector Double -> IO Bool
isRegular' x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__isRegular_TIC x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__hash_combine" c_CasADi__hash_combine
  :: CSize -> Ptr (CppVec CInt) -> IO ()
{-|
>[INTERNAL]  Generate a hash
>value incrementally (function taken from boost)
-}
hash_combine
  :: CSize -> Vector Int -> IO ()
hash_combine x0 x1 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  c_CasADi__hash_combine x0' x1' >>= wrapReturn

foreign import ccall unsafe "CasADi__hash_sparsity" c_CasADi__hash_sparsity
  :: CInt -> CInt -> Ptr (CppVec CInt) -> Ptr (CppVec CInt) -> IO CSize
{-|
>Hash a sparsity pattern.
-}
hash_sparsity
  :: Int -> Int -> Vector Int -> Vector Int -> IO CSize
hash_sparsity x0 x1 x2 x3 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  withMarshal x2 $ \x2' ->
  withMarshal x3 $ \x3' ->
  c_CasADi__hash_sparsity x0' x1' x2' x3' >>= wrapReturn

foreign import ccall unsafe "CasADi__transpose" c_CasADi__transpose
  :: Ptr IMatrix' -> IO (Ptr IMatrix')
{-|
>>  MX CasADi::transpose(const MX &x)
>------------------------------------------------------------------------
>
>Transpose an expression.
>
>>  Matrix< DataType > CasADi::transpose(const Matrix< DataType > &x)
>------------------------------------------------------------------------
>
>Transpose of a matrix.
>
>>  Sparsity CasADi::transpose(const Sparsity &a)
>------------------------------------------------------------------------
>
>Transpose the pattern.
-}
transpose
  :: IMatrix -> IO IMatrix
transpose x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__transpose x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__mul" c_CasADi__mul
  :: Ptr IMatrix' -> Ptr IMatrix' -> Ptr Sparsity' -> IO (Ptr IMatrix')
{-|
>>  MX CasADi::mul(const MX &x, const MX &y, const Sparsity &sp_z=Sparsity())
>------------------------------------------------------------------------
>
>Take the matrix product of 2 MX objects.
>
>With optional sp_z you can specify the sparsity of the result A typical use
>case might be where the product is only constructed to inspect the trace of
>it. sp_z diagonal will be more efficient then.
>
>>  MX CasADi::mul(const std::vector< MX > &x)
>------------------------------------------------------------------------
>
>Take the matrix product of n MX objects.
>
>>  Matrix< DataType > CasADi::mul(const Matrix< DataType > &x, const Matrix< DataType > &y, const Sparsity &sp_z=Sparsity())
>------------------------------------------------------------------------
>
>Matrix product of two matrices.
>
>With optional sp_z you can specify the sparsity of the result A typical use
>case might be where the product is only constructed to inspect the trace of
>it. sp_z diagonal will be more efficient then.
>
>>  Matrix< DataType > CasADi::mul(const std::vector< Matrix< DataType > > &args)
>------------------------------------------------------------------------
>
>Matrix product of n matrices.
>
>>  Sparsity CasADi::mul(const Sparsity &a, const Sparsity &b)
>------------------------------------------------------------------------
>
>Get the sparsity resulting from a matrix multiplication.
-}
mul
  :: IMatrix -> IMatrix -> Sparsity -> IO IMatrix
mul x0 x1 x2 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  withMarshal x2 $ \x2' ->
  c_CasADi__mul x0' x1' x2' >>= wrapReturn

foreign import ccall unsafe "CasADi__mul_TIC" c_CasADi__mul_TIC
  :: Ptr IMatrix' -> Ptr IMatrix' -> IO (Ptr IMatrix')
mul'
  :: IMatrix -> IMatrix -> IO IMatrix
mul' x0 x1 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  c_CasADi__mul_TIC x0' x1' >>= wrapReturn

foreign import ccall unsafe "CasADi__mul_TIC_TIC" c_CasADi__mul_TIC_TIC
  :: Ptr (CppVec (Ptr IMatrix')) -> IO (Ptr IMatrix')
mul''
  :: Vector IMatrix -> IO IMatrix
mul'' x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__mul_TIC_TIC x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__det" c_CasADi__det
  :: Ptr IMatrix' -> IO CInt
{-|
>>  MX CasADi::det(const MX &A)
>------------------------------------------------------------------------
>
>Matrix determinant (experimental)
-}
det
  :: IMatrix -> IO Int
det x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__det x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__getMinor" c_CasADi__getMinor
  :: Ptr IMatrix' -> CInt -> CInt -> IO CInt
getMinor
  :: IMatrix -> Int -> Int -> IO Int
getMinor x0 x1 x2 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  withMarshal x2 $ \x2' ->
  c_CasADi__getMinor x0' x1' x2' >>= wrapReturn

foreign import ccall unsafe "CasADi__cofactor" c_CasADi__cofactor
  :: Ptr IMatrix' -> CInt -> CInt -> IO CInt
cofactor
  :: IMatrix -> Int -> Int -> IO Int
cofactor x0 x1 x2 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  withMarshal x2 $ \x2' ->
  c_CasADi__cofactor x0' x1' x2' >>= wrapReturn

foreign import ccall unsafe "CasADi__adj" c_CasADi__adj
  :: Ptr IMatrix' -> IO (Ptr IMatrix')
adj
  :: IMatrix -> IO IMatrix
adj x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__adj x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__inv" c_CasADi__inv
  :: Ptr IMatrix' -> IO (Ptr IMatrix')
{-|
>>  MX CasADi::inv(const MX &A)
>------------------------------------------------------------------------
>
>Matrix inverse (experimental)
-}
inv
  :: IMatrix -> IO IMatrix
inv x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__inv x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__reshape" c_CasADi__reshape
  :: Ptr IMatrix' -> CInt -> CInt -> IO (Ptr IMatrix')
{-|
>>  MX CasADi::reshape(const MX &x, std::pair< int, int > rc)
>------------------------------------------------------------------------
>
>Returns a reshaped version of the MX, dimensions as a vector.
>
>>  MX CasADi::reshape(const MX &x, int nrow, int ncol)
>------------------------------------------------------------------------
>
>Returns a reshaped version of the MX.
>
>>  MX CasADi::reshape(const MX &x, const Sparsity &sp)
>------------------------------------------------------------------------
>
>Reshape the MX.
>
>>  Sparsity CasADi::reshape(const Sparsity &a, int nrow, int ncol)
>------------------------------------------------------------------------
>
>Reshape the sparsity pattern keeping the relative location of the nonzeros.
-}
reshape
  :: IMatrix -> Int -> Int -> IO IMatrix
reshape x0 x1 x2 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  withMarshal x2 $ \x2' ->
  c_CasADi__reshape x0' x1' x2' >>= wrapReturn

foreign import ccall unsafe "CasADi__reshape_TIC" c_CasADi__reshape_TIC
  :: Ptr IMatrix' -> Ptr Sparsity' -> IO (Ptr IMatrix')
reshape'
  :: IMatrix -> Sparsity -> IO IMatrix
reshape' x0 x1 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  c_CasADi__reshape_TIC x0' x1' >>= wrapReturn

foreign import ccall unsafe "CasADi__vec" c_CasADi__vec
  :: Ptr IMatrix' -> IO (Ptr IMatrix')
{-|
>>  MX CasADi::vec(const MX &x)
>------------------------------------------------------------------------
>
>Returns a vectorized version of the MX Same as reshape(x, x.numel(),1)
>
>a c b d
>
>turns into
>
>a b c d
>
>>  Matrix< DataType > CasADi::vec(const Matrix< DataType > &a)
>------------------------------------------------------------------------
>
>make a vector Reshapes/vectorizes the Matrix<DataType> such that the shape
>becomes (expr.numel(),1). Columns are stacked on top of each other. Same as
>reshape(expr, expr.numel(),1)
>
>a c b d  turns into
>
>a b c d
>
>>  Sparsity CasADi::vec(const Sparsity &a)
>------------------------------------------------------------------------
>
>Vectorize the pattern.
-}
vec
  :: IMatrix -> IO IMatrix
vec x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__vec x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__vecNZ" c_CasADi__vecNZ
  :: Ptr IMatrix' -> IO (Ptr IMatrix')
{-|
>>  MX CasADi::vecNZ(const MX &x)
>------------------------------------------------------------------------
>
>Returns a vectorized version of the MX, prseverving only nonzeros.
>
>>  Matrix< DataType > CasADi::vecNZ(const Matrix< DataType > &a)
>------------------------------------------------------------------------
>
>Returns a flattened version of the Matrix, preserving only nonzeros.
-}
vecNZ
  :: IMatrix -> IO IMatrix
vecNZ x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__vecNZ x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__blockcat" c_CasADi__blockcat
  :: Ptr (CppVec (Ptr (CppVec (Ptr IMatrix')))) -> IO (Ptr IMatrix')
{-|
>>  MX CasADi::blockcat(const std::vector< std::vector< MX > > &v)
>------------------------------------------------------------------------
>
>Construct a matrix from a list of list of blocks.
>
>blockcat(blocksplit(x,...,...)) = x
>
>>  MX CasADi::blockcat(const MX &A, const MX &B, const MX &C, const MX &D)
>
>>  Matrix< DataType > CasADi::blockcat(const std::vector< std::vector< Matrix< DataType > > > &v)
>------------------------------------------------------------------------
>
>Construct a matrix from a list of list of blocks.
>
>>  Matrix< DataType > CasADi::blockcat(const Matrix< DataType > &A, const Matrix< DataType > &B, const Matrix< DataType > &C, const Matrix< DataType > &D)
>------------------------------------------------------------------------
>[INTERNAL] 
>Construct a matrix from 4 blocks.
-}
blockcat
  :: Vector (Vector IMatrix) -> IO IMatrix
blockcat x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__blockcat x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__blocksplit" c_CasADi__blocksplit
  :: Ptr IMatrix' -> Ptr (CppVec CInt) -> Ptr (CppVec CInt) -> IO (Ptr (CppVec (Ptr (CppVec (Ptr IMatrix')))))
{-|
>>  std::vector< std::vector< MX > > CasADi::blocksplit(const MX &x, const std::vector< int > &vert_offset, const std::vector< int > &horz_offset)
>------------------------------------------------------------------------
>
>chop up into blocks
>
>vert_offset Defines the boundaries of the block cols horz_offset Defines the
>boundaries of the block rows
>
>blockcat(blocksplit(x,...,...)) = x
>
>>  std::vector< std::vector< MX > > CasADi::blocksplit(const MX &x, int vert_incr=1, int horz_incr=1)
>------------------------------------------------------------------------
>
>chop up into blocks
>
>vert_incr Defines the increment for block boundaries in col dimension
>horz_incr Defines the increment for block boundaries in row dimension
>
>blockcat(blocksplit(x,...,...)) = x
>
>>  std::vector< std::vector< Matrix< DataType > > > CasADi::blocksplit(const Matrix< DataType > &x, const std::vector< int > &vert_offset, const std::vector< int > &horz_offset)
>------------------------------------------------------------------------
>
>chop up into blocks
>
>vert_offset Defines the boundaries of the block rows horz_offset Defines the
>boundaries of the block columns
>
>blockcat(blocksplit(x,...,...)) = x
>
>>  std::vector< std::vector< Matrix< DataType > > > CasADi::blocksplit(const Matrix< DataType > &x, int vert_incr=1, int horz_incr=1)
>------------------------------------------------------------------------
>
>chop up into blocks
>
>vert_incr Defines the increment for block boundaries in row dimension
>horz_incr Defines the increment for block boundaries in column dimension
>
>blockcat(blocksplit(x,...,...)) = x
-}
blocksplit
  :: IMatrix -> Vector Int -> Vector Int -> IO (Vector (Vector IMatrix))
blocksplit x0 x1 x2 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  withMarshal x2 $ \x2' ->
  c_CasADi__blocksplit x0' x1' x2' >>= wrapReturn

foreign import ccall unsafe "CasADi__blocksplit_TIC" c_CasADi__blocksplit_TIC
  :: Ptr IMatrix' -> CInt -> CInt -> IO (Ptr (CppVec (Ptr (CppVec (Ptr IMatrix')))))
blocksplit'
  :: IMatrix -> Int -> Int -> IO (Vector (Vector IMatrix))
blocksplit' x0 x1 x2 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  withMarshal x2 $ \x2' ->
  c_CasADi__blocksplit_TIC x0' x1' x2' >>= wrapReturn

foreign import ccall unsafe "CasADi__blocksplit_TIC_TIC" c_CasADi__blocksplit_TIC_TIC
  :: Ptr IMatrix' -> CInt -> IO (Ptr (CppVec (Ptr (CppVec (Ptr IMatrix')))))
blocksplit''
  :: IMatrix -> Int -> IO (Vector (Vector IMatrix))
blocksplit'' x0 x1 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  c_CasADi__blocksplit_TIC_TIC x0' x1' >>= wrapReturn

foreign import ccall unsafe "CasADi__blocksplit_TIC_TIC_TIC" c_CasADi__blocksplit_TIC_TIC_TIC
  :: Ptr IMatrix' -> IO (Ptr (CppVec (Ptr (CppVec (Ptr IMatrix')))))
blocksplit'''
  :: IMatrix -> IO (Vector (Vector IMatrix))
blocksplit''' x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__blocksplit_TIC_TIC_TIC x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__vertcat" c_CasADi__vertcat
  :: Ptr (CppVec (Ptr IMatrix')) -> IO (Ptr IMatrix')
{-|
>>  MX CasADi::vertcat(const MX &a, const MX &b)
>------------------------------------------------------------------------
>
>concatenate horizontally, two matrices
>
>>  MX CasADi::vertcat(const std::vector< MX > &comp)
>------------------------------------------------------------------------
>
>concatenate horizontally
>
>vertcat(vertsplit(x,...)) = x
>
>>  Matrix< DataType > CasADi::vertcat(const std::vector< Matrix< DataType > > &v)
>------------------------------------------------------------------------
>
>Concatenate a list of matrices horizontally Alternative terminology:
>horizontal stack, hstack, horizontal append, [a b].
>
>vertcat(vertsplit(x,...)) = x
>
>>  Matrix< DataType > CasADi::vertcat(const Matrix< DataType > &x, const Matrix< DataType > &y)
>------------------------------------------------------------------------
>[INTERNAL]
>
>>  Sparsity CasADi::vertcat(const std::vector< Sparsity > &v)
>------------------------------------------------------------------------
>
>Concatenate a list of sparsities vertically Alternative terminology:
>vertical stack, vstack, vertical append, [a;b].
-}
vertcat
  :: Vector IMatrix -> IO IMatrix
vertcat x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__vertcat x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__vertsplit" c_CasADi__vertsplit
  :: Ptr IMatrix' -> Ptr (CppVec CInt) -> IO (Ptr (CppVec (Ptr IMatrix')))
{-|
>>  std::vector< MX > CasADi::vertsplit(const MX &x, const std::vector< int > &output_offset)
>
>>  std::vector< Matrix< DataType > > CasADi::vertsplit(const Matrix< DataType > &v, const std::vector< int > &offset)
>------------------------------------------------------------------------
>
>split horizontally, retaining groups of rows
>
>Parameters:
>-----------
>
>output_offset:  List of all start rows for each group the last row group
>will run to the end.
>
>vertcat(vertsplit(x,...)) = x
>
>>  std::vector< MX > CasADi::vertsplit(const MX &x, int incr=1)
>
>>  std::vector< Matrix< DataType > > CasADi::vertsplit(const Matrix< DataType > &v, int incr=1)
>------------------------------------------------------------------------
>
>split horizontally, retaining fixed-sized groups of rows
>
>Parameters:
>-----------
>
>incr:  Size of each group of rows
>
>vertcat(vertsplit(x,...)) = x
>
>>  std::vector< Sparsity > CasADi::vertsplit(const Sparsity &sp, const std::vector< int > &output_offset)
>------------------------------------------------------------------------
>
>Split up a sparsity pattern vertically.
-}
vertsplit
  :: IMatrix -> Vector Int -> IO (Vector IMatrix)
vertsplit x0 x1 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  c_CasADi__vertsplit x0' x1' >>= wrapReturn

foreign import ccall unsafe "CasADi__vertsplit_TIC" c_CasADi__vertsplit_TIC
  :: Ptr IMatrix' -> CInt -> IO (Ptr (CppVec (Ptr IMatrix')))
vertsplit'
  :: IMatrix -> Int -> IO (Vector IMatrix)
vertsplit' x0 x1 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  c_CasADi__vertsplit_TIC x0' x1' >>= wrapReturn

foreign import ccall unsafe "CasADi__vertsplit_TIC_TIC" c_CasADi__vertsplit_TIC_TIC
  :: Ptr IMatrix' -> IO (Ptr (CppVec (Ptr IMatrix')))
vertsplit''
  :: IMatrix -> IO (Vector IMatrix)
vertsplit'' x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__vertsplit_TIC_TIC x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__horzcat" c_CasADi__horzcat
  :: Ptr (CppVec (Ptr IMatrix')) -> IO (Ptr IMatrix')
{-|
>>  MX CasADi::horzcat(const MX &a, const MX &b)
>------------------------------------------------------------------------
>
>concatenate vertically, two matrices
>
>>  MX CasADi::horzcat(const std::vector< MX > &x)
>------------------------------------------------------------------------
>
>concatenate vertically
>
>horzcat(horzsplit(x,...)) = x
>
>>  Matrix< DataType > CasADi::horzcat(const std::vector< Matrix< DataType > > &v)
>------------------------------------------------------------------------
>
>Concatenate a list of matrices vertically Alternative terminology: vertical
>stack, vstack, vertical append, [a;b].
>
>horzcat(horzsplit(x,...)) = x
>
>>  Matrix< DataType > CasADi::horzcat(const Matrix< DataType > &x, const Matrix< DataType > &y)
>------------------------------------------------------------------------
>[INTERNAL]
>
>>  Sparsity CasADi::horzcat(const std::vector< Sparsity > &v)
>------------------------------------------------------------------------
>
>Concatenate a list of sparsities horizontally Alternative terminology:
>horizontal stack, hstack, horizontal append, [a b].
-}
horzcat
  :: Vector IMatrix -> IO IMatrix
horzcat x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__horzcat x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__horzsplit" c_CasADi__horzsplit
  :: Ptr IMatrix' -> Ptr (CppVec CInt) -> IO (Ptr (CppVec (Ptr IMatrix')))
{-|
>>  std::vector< MX > CasADi::horzsplit(const MX &x, const std::vector< int > &output_offset)
>------------------------------------------------------------------------
>
>split vertically, retaining groups of cols
>
>Parameters:
>-----------
>
>output_offset:  List of all start cols for each group the last col group
>will run to the end.
>
>horzcat(horzsplit(x,...)) = x
>
>>  std::vector< MX > CasADi::horzsplit(const MX &x, int incr=1)
>
>>  std::vector< Matrix< DataType > > CasADi::horzsplit(const Matrix< DataType > &v, int incr=1)
>------------------------------------------------------------------------
>
>split vertically, retaining fixed-sized groups of cols
>
>Parameters:
>-----------
>
>incr:  Size of each group of cols
>
>horzcat(horzsplit(x,...)) = x
>
>>  std::vector< Matrix< DataType > > CasADi::horzsplit(const Matrix< DataType > &v, const std::vector< int > &offset)
>------------------------------------------------------------------------
>
>split vertically, retaining groups of cols
>
>Parameters:
>-----------
>
>offset:  List of all start cols for each group the last col group will run
>to the end.
>
>horzcat(horzsplit(x,...)) = x
>
>>  std::vector< Sparsity > CasADi::horzsplit(const Sparsity &sp, const std::vector< int > &output_offset)
>------------------------------------------------------------------------
>
>Split up a sparsity pattern horizontally.
-}
horzsplit
  :: IMatrix -> Vector Int -> IO (Vector IMatrix)
horzsplit x0 x1 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  c_CasADi__horzsplit x0' x1' >>= wrapReturn

foreign import ccall unsafe "CasADi__horzsplit_TIC" c_CasADi__horzsplit_TIC
  :: Ptr IMatrix' -> CInt -> IO (Ptr (CppVec (Ptr IMatrix')))
horzsplit'
  :: IMatrix -> Int -> IO (Vector IMatrix)
horzsplit' x0 x1 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  c_CasADi__horzsplit_TIC x0' x1' >>= wrapReturn

foreign import ccall unsafe "CasADi__horzsplit_TIC_TIC" c_CasADi__horzsplit_TIC_TIC
  :: Ptr IMatrix' -> IO (Ptr (CppVec (Ptr IMatrix')))
horzsplit''
  :: IMatrix -> IO (Vector IMatrix)
horzsplit'' x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__horzsplit_TIC_TIC x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__inner_prod" c_CasADi__inner_prod
  :: Ptr IMatrix' -> Ptr IMatrix' -> IO (Ptr IMatrix')
{-|
>>  MX CasADi::inner_prod(const MX &x, const MX &y)
>------------------------------------------------------------------------
>
>Take the inner product of two vectors Equals.
>
>with x and y vectors
>
>>  Matrix< DataType > CasADi::inner_prod(const Matrix< DataType > &x, const Matrix< DataType > &y)
>------------------------------------------------------------------------
>
>Inner product of two matrices Equals.
>
>with x and y matrices of the same dimension
>
>>  T CasADi::inner_prod(const std::vector< T > &a, const std::vector< T > &b)
>------------------------------------------------------------------------
>[INTERNAL] 
-}
inner_prod
  :: IMatrix -> IMatrix -> IO IMatrix
inner_prod x0 x1 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  c_CasADi__inner_prod x0' x1' >>= wrapReturn

foreign import ccall unsafe "CasADi__outer_prod" c_CasADi__outer_prod
  :: Ptr IMatrix' -> Ptr IMatrix' -> IO (Ptr IMatrix')
{-|
>>  MX CasADi::outer_prod(const MX &x, const MX &y)
>------------------------------------------------------------------------
>
>Take the outer product of two vectors Equals.
>
>with x and y vectors
>
>>  Matrix< DataType > CasADi::outer_prod(const Matrix< DataType > &x, const Matrix< DataType > &y)
>------------------------------------------------------------------------
>
>Outer product of two vectors Equals.
>
>with x and y vectors
-}
outer_prod
  :: IMatrix -> IMatrix -> IO IMatrix
outer_prod x0 x1 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  c_CasADi__outer_prod x0' x1' >>= wrapReturn

foreign import ccall unsafe "CasADi__norm_1" c_CasADi__norm_1
  :: Ptr IMatrix' -> IO (Ptr IMatrix')
{-|
>>  MX CasADi::norm_1(const MX &x)
>
>>  Matrix< DataType > CasADi::norm_1(const Matrix< DataType > &x)
>------------------------------------------------------------------------
>
>1-norm
>
>>  T CasADi::norm_1(const std::vector< T > &x)
>------------------------------------------------------------------------
>[INTERNAL] 
-}
norm_1
  :: IMatrix -> IO IMatrix
norm_1 x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__norm_1 x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__norm_2" c_CasADi__norm_2
  :: Ptr IMatrix' -> IO (Ptr IMatrix')
{-|
>>  MX CasADi::norm_2(const MX &x)
>
>>  Matrix< DataType > CasADi::norm_2(const Matrix< DataType > &x)
>------------------------------------------------------------------------
>
>2-norm
>
>>  T CasADi::norm_2(const std::vector< T > &x)
>------------------------------------------------------------------------
>[INTERNAL] 
-}
norm_2
  :: IMatrix -> IO IMatrix
norm_2 x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__norm_2 x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__norm_inf" c_CasADi__norm_inf
  :: Ptr IMatrix' -> IO (Ptr IMatrix')
{-|
>>  MX CasADi::norm_inf(const MX &x)
>
>>  Matrix< DataType > CasADi::norm_inf(const Matrix< DataType > &x)
>------------------------------------------------------------------------
>
>Infinity-norm.
>
>>  T CasADi::norm_inf(const std::vector< T > &x)
>------------------------------------------------------------------------
>[INTERNAL] 
-}
norm_inf
  :: IMatrix -> IO IMatrix
norm_inf x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__norm_inf x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__norm_F" c_CasADi__norm_F
  :: Ptr IMatrix' -> IO (Ptr IMatrix')
{-|
>Frobenius norm.
-}
norm_F
  :: IMatrix -> IO IMatrix
norm_F x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__norm_F x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__qr" c_CasADi__qr
  :: Ptr IMatrix' -> Ptr IMatrix' -> Ptr IMatrix' -> IO ()
{-|
>[INTERNAL]  QR factorization using the
>modified Gram-Schmidt algorithm More stable than the classical Gram-Schmidt,
>but may break down if the rows of A are nearly linearly dependent See J.
>Demmel: Applied Numerical Linear Algebra (algorithm 3.1.). Note that in
>SWIG, Q and R are returned by value.
-}
qr
  :: IMatrix -> IMatrix -> IMatrix -> IO ()
qr x0 x1 x2 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  withMarshal x2 $ \x2' ->
  c_CasADi__qr x0' x1' x2' >>= wrapReturn

foreign import ccall unsafe "CasADi__nullspace" c_CasADi__nullspace
  :: Ptr IMatrix' -> IO (Ptr IMatrix')
{-|
>Computes the nullspace of a matrix A.
>
>Finds Z m-by-(m-n) such that AZ = 0 with A n-by-m with m > n
>
>Assumes A is full rank
>
>Inspired by Numerical Methods in Scientific Computing by Ake Bjorck
-}
nullspace
  :: IMatrix -> IO IMatrix
nullspace x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__nullspace x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__solve" c_CasADi__solve
  :: Ptr IMatrix' -> Ptr IMatrix' -> IO (Ptr IMatrix')
{-|
>>  Matrix< double > CasADi::solve(const Matrix< double > &A, const Matrix< double > &b, linearSolverCreator lsolver, const Dictionary &dict=Dictionary())
>
>>  MX CasADi::solve(const MX &A, const MX &b, linearSolverCreator lsolver=SymbolicQR::creator, const Dictionary &dict=Dictionary())
>------------------------------------------------------------------------
>
>Solve a system of equations: A*x = b.
>
>>  Matrix< DataType > CasADi::solve(const Matrix< DataType > &A, const Matrix< DataType > &b)
>------------------------------------------------------------------------
>
>Solve a system of equations: A*x = b The solve routine works similar to
>Matlab's backslash when A is square and nonsingular. The algorithm used is
>the following:
>
>A simple forward or backward substitution if A is upper or lower triangular
>
>If the linear system is at most 3-by-3, form the inverse via minor expansion
>and multiply
>
>Permute the variables and equations as to get a (structurally) nonzero
>diagonal, then perform a QR factorization without pivoting and solve the
>factorized system.
>
>Note 1: If there are entries of the linear system known to be zero, these
>will be removed. Elements that are very small, or will evaluate to be zero,
>can still cause numerical errors, due to the lack of pivoting (which is not
>possible since cannot compare the size of entries)
>
>Note 2: When permuting the linear system, a BLT (block lower triangular)
>transformation is formed. Only the permutation part of this is however used.
>An improvement would be to solve block-by-block if there are multiple BLT
>blocks.
-}
solve
  :: IMatrix -> IMatrix -> IO IMatrix
solve x0 x1 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  c_CasADi__solve x0' x1' >>= wrapReturn

foreign import ccall unsafe "CasADi__pinv" c_CasADi__pinv
  :: Ptr IMatrix' -> IO (Ptr IMatrix')
{-|
>>  Matrix< double > CasADi::pinv(const Matrix< double > &A, linearSolverCreator lsolver, const Dictionary &dict=Dictionary())
>
>>  MX CasADi::pinv(const MX &A, linearSolverCreator lsolver, const Dictionary &dict=Dictionary())
>------------------------------------------------------------------------
>
>Computes the Moore-Penrose pseudo-inverse.
>
>If the matrix A is fat (size1>size2), mul(A,pinv(A)) is unity. If the matrix
>A is slender (size2<size1), mul(pinv(A),A) is unity.
>
>>  Matrix< DataType > CasADi::pinv(const Matrix< DataType > &A)
>------------------------------------------------------------------------
>
>Computes the Moore-Penrose pseudo-inverse.
>
>If the matrix A is fat (size2>size1), mul(A,pinv(A)) is unity. If the matrix
>A is slender (size1<size2), mul(pinv(A),A) is unity.
-}
pinv
  :: IMatrix -> IO IMatrix
pinv x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__pinv x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__repmat" c_CasADi__repmat
  :: Ptr IMatrix' -> CInt -> CInt -> IO (Ptr IMatrix')
{-|
>Repeat matrix A n times vertically and m times horizontally.
-}
repmat
  :: IMatrix -> Int -> Int -> IO IMatrix
repmat x0 x1 x2 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  withMarshal x2 $ \x2' ->
  c_CasADi__repmat x0' x1' x2' >>= wrapReturn

foreign import ccall unsafe "CasADi__unite" c_CasADi__unite
  :: Ptr IMatrix' -> Ptr IMatrix' -> IO (Ptr IMatrix')
{-|
>Unite two matrices no overlapping sparsity.
-}
unite
  :: IMatrix -> IMatrix -> IO IMatrix
unite x0 x1 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  c_CasADi__unite x0' x1' >>= wrapReturn

foreign import ccall unsafe "CasADi__sumRows" c_CasADi__sumRows
  :: Ptr IMatrix' -> IO (Ptr IMatrix')
{-|
>Return a row-wise summation of elements.
-}
sumRows
  :: IMatrix -> IO IMatrix
sumRows x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__sumRows x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__sumCols" c_CasADi__sumCols
  :: Ptr IMatrix' -> IO (Ptr IMatrix')
{-|
>Return a col-wise summation of elements.
-}
sumCols
  :: IMatrix -> IO IMatrix
sumCols x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__sumCols x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__sumAll" c_CasADi__sumAll
  :: Ptr IMatrix' -> IO (Ptr IMatrix')
{-|
>Return summation of all elements.
-}
sumAll
  :: IMatrix -> IO IMatrix
sumAll x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__sumAll x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__trace" c_CasADi__trace
  :: Ptr IMatrix' -> IO CInt
{-|
>>  MX CasADi::trace(const MX &A)
>------------------------------------------------------------------------
>
>Matrix trace.
-}
trace
  :: IMatrix -> IO Int
trace x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__trace x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__diag" c_CasADi__diag
  :: Ptr IMatrix' -> IO (Ptr IMatrix')
{-|
>>  MX CasADi::diag(const MX &x)
>------------------------------------------------------------------------
>
>Get the diagonal of a matrix or construct a diagonal.
>
>When the input is square, the diagonal elements are returned. If the input
>is vector-like, a diagonal matrix is constructed with it.
>
>>  Matrix< DataType > CasADi::diag(const Matrix< DataType > &A)
>------------------------------------------------------------------------
>
>Get the diagonal of a matrix or construct a diagonal When the input is
>square, the diagonal elements are returned. If the input is vector- like, a
>diagonal matrix is constructed with it.
-}
diag
  :: IMatrix -> IO IMatrix
diag x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__diag x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__blkdiag" c_CasADi__blkdiag
  :: Ptr (CppVec (Ptr IMatrix')) -> IO (Ptr IMatrix')
{-|
>>  MX CasADi::blkdiag(const std::vector< MX > &A)
>
>>  MX CasADi::blkdiag(const MX &A, const MX &B)
>------------------------------------------------------------------------
>
>Construct a matrix with given blocks on the diagonal.
>
>>  Matrix< DataType > CasADi::blkdiag(const std::vector< Matrix< DataType > > &A)
>------------------------------------------------------------------------
>
>Construct a matrix with given block on the diagonal.
>
>>  Sparsity CasADi::blkdiag(const std::vector< Sparsity > &v)
>------------------------------------------------------------------------
>
>Construct a Sparsity with given blocks on the diagonal.
-}
blkdiag
  :: Vector IMatrix -> IO IMatrix
blkdiag x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__blkdiag x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__polyval" c_CasADi__polyval
  :: Ptr IMatrix' -> Ptr IMatrix' -> IO (Ptr IMatrix')
{-|
>Evaluate a polynomial with coefficeints p in x.
-}
polyval
  :: IMatrix -> IMatrix -> IO IMatrix
polyval x0 x1 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  c_CasADi__polyval x0' x1' >>= wrapReturn

foreign import ccall unsafe "CasADi__addMultiple" c_CasADi__addMultiple
  :: Ptr IMatrix' -> Ptr (CppVec CInt) -> Ptr (CppVec CInt) -> CInt -> IO ()
{-|
>same as: res += mul(A,v)
-}
addMultiple
  :: IMatrix -> Vector Int -> Vector Int -> Bool -> IO ()
addMultiple x0 x1 x2 x3 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  withMarshal x2 $ \x2' ->
  withMarshal x3 $ \x3' ->
  c_CasADi__addMultiple x0' x1' x2' x3' >>= wrapReturn

foreign import ccall unsafe "CasADi__addMultiple_TIC" c_CasADi__addMultiple_TIC
  :: Ptr IMatrix' -> Ptr (CppVec CInt) -> Ptr (CppVec CInt) -> IO ()
addMultiple'
  :: IMatrix -> Vector Int -> Vector Int -> IO ()
addMultiple' x0 x1 x2 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  withMarshal x2 $ \x2' ->
  c_CasADi__addMultiple_TIC x0' x1' x2' >>= wrapReturn

foreign import ccall unsafe "CasADi__veccat" c_CasADi__veccat
  :: Ptr (CppVec (Ptr IMatrix')) -> IO (Ptr IMatrix')
{-|
>>  MX CasADi::veccat(const std::vector< MX > &comp)
>------------------------------------------------------------------------
>
>Concatenate vertically while vectorizing all arguments.
>
>>  Matrix< DataType > CasADi::veccat(const std::vector< Matrix< DataType > > &comp)
>------------------------------------------------------------------------
>
>concatenate vertically while vectorizing all arguments with vec
-}
veccat
  :: Vector IMatrix -> IO IMatrix
veccat x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__veccat x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__vecNZcat" c_CasADi__vecNZcat
  :: Ptr (CppVec (Ptr IMatrix')) -> IO (Ptr IMatrix')
{-|
>>  MX CasADi::vecNZcat(const std::vector< MX > &comp)
>------------------------------------------------------------------------
>
>concatenate vertically while vecing all arguments with vecNZ
>
>>  Matrix< DataType > CasADi::vecNZcat(const std::vector< Matrix< DataType > > &comp)
>------------------------------------------------------------------------
>
>concatenate vertically while vectorizing all arguments with vecNZ
-}
vecNZcat
  :: Vector IMatrix -> IO IMatrix
vecNZcat x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__vecNZcat x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__project" c_CasADi__project
  :: Ptr IMatrix' -> Ptr Sparsity' -> IO (Ptr IMatrix')
{-|
>Create a new matrix with a given sparsity pattern but with the nonzeros
>taken from an existing matrix.
-}
project
  :: IMatrix -> Sparsity -> IO IMatrix
project x0 x1 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  c_CasADi__project x0' x1' >>= wrapReturn

foreign import ccall unsafe "CasADi__sprank" c_CasADi__sprank
  :: Ptr IMatrix' -> IO CInt
{-|
>Obtain the structural rank of a sparsity-pattern.
-}
sprank
  :: IMatrix -> IO Int
sprank x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__sprank x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__kron" c_CasADi__kron
  :: Ptr IMatrix' -> Ptr IMatrix' -> IO (Ptr IMatrix')
{-|
>Kronecker tensor product.
>
>Creates a block matrix in which each element (i,j) is a_ij*b
-}
kron
  :: IMatrix -> IMatrix -> IO IMatrix
kron x0 x1 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  c_CasADi__kron x0' x1' >>= wrapReturn

foreign import ccall unsafe "CasADi__sparse" c_CasADi__sparse
  :: Ptr IMatrix' -> CDouble -> IO (Ptr IMatrix')
{-|
>Make a matrix sparse by removing numerical zeros.
-}
sparse
  :: IMatrix -> Double -> IO IMatrix
sparse x0 x1 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  c_CasADi__sparse x0' x1' >>= wrapReturn

foreign import ccall unsafe "CasADi__sparse_TIC" c_CasADi__sparse_TIC
  :: Ptr IMatrix' -> IO (Ptr IMatrix')
sparse'
  :: IMatrix -> IO IMatrix
sparse' x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__sparse_TIC x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__dense" c_CasADi__dense
  :: Ptr IMatrix' -> IO (Ptr IMatrix')
{-|
>>  MX CasADi::dense(const MX &x)
>------------------------------------------------------------------------
>
>create a clipped view into a matrix Create a sparse matrix from a dense
>matrix A, with sparsity pattern sp
>
>MX clip(const MX& A, const Sparsity& sp) { Join the sparsity patterns
>std::vector<int> mapping; Sparsity sp =
>A.sparsity().patternIntersection(sp,mapping);
>
>Split up the mapping std::vector<int> nzA,nzB;
>
>Copy sparsity for(int k=0; k<mapping.size(); ++k){ if(mapping[k]<0){
>nzA.push_back(k); } else if(mapping[k]>0){ nzB.push_back(k); } else { throw
>CasadiException("Pattern intersection not empty"); } }
>
>Create mapping MX ret; ret.assignNode(new Mapping(sp));
>ret->assign(A,range(nzA.size()),nzA); ret->assign(B,range(nzB.size()),nzB);
>return ret;
>
>}
>
>Make the matrix dense if not already
>
>>  Matrix< DataType > CasADi::dense(const Matrix< DataType > &A)
>------------------------------------------------------------------------
>
>Make a matrix dense.
-}
dense
  :: IMatrix -> IO IMatrix
dense x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__dense x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__transpose_TIC" c_CasADi__transpose_TIC
  :: Ptr DMatrix' -> IO (Ptr DMatrix')
transpose'
  :: DMatrix -> IO DMatrix
transpose' x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__transpose_TIC x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__mul_TIC_TIC_TIC" c_CasADi__mul_TIC_TIC_TIC
  :: Ptr DMatrix' -> Ptr DMatrix' -> Ptr Sparsity' -> IO (Ptr DMatrix')
mul'''
  :: DMatrix -> DMatrix -> Sparsity -> IO DMatrix
mul''' x0 x1 x2 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  withMarshal x2 $ \x2' ->
  c_CasADi__mul_TIC_TIC_TIC x0' x1' x2' >>= wrapReturn

foreign import ccall unsafe "CasADi__mul_TIC_TIC_TIC_TIC" c_CasADi__mul_TIC_TIC_TIC_TIC
  :: Ptr DMatrix' -> Ptr DMatrix' -> IO (Ptr DMatrix')
mul''''
  :: DMatrix -> DMatrix -> IO DMatrix
mul'''' x0 x1 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  c_CasADi__mul_TIC_TIC_TIC_TIC x0' x1' >>= wrapReturn

foreign import ccall unsafe "CasADi__mul_TIC_TIC_TIC_TIC_TIC" c_CasADi__mul_TIC_TIC_TIC_TIC_TIC
  :: Ptr (CppVec (Ptr DMatrix')) -> IO (Ptr DMatrix')
mul'''''
  :: Vector DMatrix -> IO DMatrix
mul''''' x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__mul_TIC_TIC_TIC_TIC_TIC x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__det_TIC" c_CasADi__det_TIC
  :: Ptr DMatrix' -> IO CDouble
det'
  :: DMatrix -> IO Double
det' x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__det_TIC x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__getMinor_TIC" c_CasADi__getMinor_TIC
  :: Ptr DMatrix' -> CInt -> CInt -> IO CDouble
getMinor'
  :: DMatrix -> Int -> Int -> IO Double
getMinor' x0 x1 x2 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  withMarshal x2 $ \x2' ->
  c_CasADi__getMinor_TIC x0' x1' x2' >>= wrapReturn

foreign import ccall unsafe "CasADi__cofactor_TIC" c_CasADi__cofactor_TIC
  :: Ptr DMatrix' -> CInt -> CInt -> IO CDouble
cofactor'
  :: DMatrix -> Int -> Int -> IO Double
cofactor' x0 x1 x2 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  withMarshal x2 $ \x2' ->
  c_CasADi__cofactor_TIC x0' x1' x2' >>= wrapReturn

foreign import ccall unsafe "CasADi__adj_TIC" c_CasADi__adj_TIC
  :: Ptr DMatrix' -> IO (Ptr DMatrix')
adj'
  :: DMatrix -> IO DMatrix
adj' x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__adj_TIC x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__inv_TIC" c_CasADi__inv_TIC
  :: Ptr DMatrix' -> IO (Ptr DMatrix')
inv'
  :: DMatrix -> IO DMatrix
inv' x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__inv_TIC x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__reshape_TIC_TIC" c_CasADi__reshape_TIC_TIC
  :: Ptr DMatrix' -> CInt -> CInt -> IO (Ptr DMatrix')
reshape''
  :: DMatrix -> Int -> Int -> IO DMatrix
reshape'' x0 x1 x2 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  withMarshal x2 $ \x2' ->
  c_CasADi__reshape_TIC_TIC x0' x1' x2' >>= wrapReturn

foreign import ccall unsafe "CasADi__reshape_TIC_TIC_TIC" c_CasADi__reshape_TIC_TIC_TIC
  :: Ptr DMatrix' -> Ptr Sparsity' -> IO (Ptr DMatrix')
reshape'''
  :: DMatrix -> Sparsity -> IO DMatrix
reshape''' x0 x1 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  c_CasADi__reshape_TIC_TIC_TIC x0' x1' >>= wrapReturn

foreign import ccall unsafe "CasADi__vec_TIC" c_CasADi__vec_TIC
  :: Ptr DMatrix' -> IO (Ptr DMatrix')
vec'
  :: DMatrix -> IO DMatrix
vec' x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__vec_TIC x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__vecNZ_TIC" c_CasADi__vecNZ_TIC
  :: Ptr DMatrix' -> IO (Ptr DMatrix')
vecNZ'
  :: DMatrix -> IO DMatrix
vecNZ' x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__vecNZ_TIC x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__blockcat_TIC" c_CasADi__blockcat_TIC
  :: Ptr (CppVec (Ptr (CppVec (Ptr DMatrix')))) -> IO (Ptr DMatrix')
blockcat'
  :: Vector (Vector DMatrix) -> IO DMatrix
blockcat' x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__blockcat_TIC x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__blocksplit_TIC_TIC_TIC_TIC" c_CasADi__blocksplit_TIC_TIC_TIC_TIC
  :: Ptr DMatrix' -> Ptr (CppVec CInt) -> Ptr (CppVec CInt) -> IO (Ptr (CppVec (Ptr (CppVec (Ptr DMatrix')))))
blocksplit''''
  :: DMatrix -> Vector Int -> Vector Int -> IO (Vector (Vector DMatrix))
blocksplit'''' x0 x1 x2 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  withMarshal x2 $ \x2' ->
  c_CasADi__blocksplit_TIC_TIC_TIC_TIC x0' x1' x2' >>= wrapReturn

foreign import ccall unsafe "CasADi__blocksplit_TIC_TIC_TIC_TIC_TIC" c_CasADi__blocksplit_TIC_TIC_TIC_TIC_TIC
  :: Ptr DMatrix' -> CInt -> CInt -> IO (Ptr (CppVec (Ptr (CppVec (Ptr DMatrix')))))
blocksplit'''''
  :: DMatrix -> Int -> Int -> IO (Vector (Vector DMatrix))
blocksplit''''' x0 x1 x2 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  withMarshal x2 $ \x2' ->
  c_CasADi__blocksplit_TIC_TIC_TIC_TIC_TIC x0' x1' x2' >>= wrapReturn

foreign import ccall unsafe "CasADi__blocksplit_TIC_TIC_TIC_TIC_TIC_TIC" c_CasADi__blocksplit_TIC_TIC_TIC_TIC_TIC_TIC
  :: Ptr DMatrix' -> CInt -> IO (Ptr (CppVec (Ptr (CppVec (Ptr DMatrix')))))
blocksplit''''''
  :: DMatrix -> Int -> IO (Vector (Vector DMatrix))
blocksplit'''''' x0 x1 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  c_CasADi__blocksplit_TIC_TIC_TIC_TIC_TIC_TIC x0' x1' >>= wrapReturn

foreign import ccall unsafe "CasADi__blocksplit_TIC_TIC_TIC_TIC_TIC_TIC_TIC" c_CasADi__blocksplit_TIC_TIC_TIC_TIC_TIC_TIC_TIC
  :: Ptr DMatrix' -> IO (Ptr (CppVec (Ptr (CppVec (Ptr DMatrix')))))
blocksplit'''''''
  :: DMatrix -> IO (Vector (Vector DMatrix))
blocksplit''''''' x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__blocksplit_TIC_TIC_TIC_TIC_TIC_TIC_TIC x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__vertcat_TIC" c_CasADi__vertcat_TIC
  :: Ptr (CppVec (Ptr DMatrix')) -> IO (Ptr DMatrix')
vertcat'
  :: Vector DMatrix -> IO DMatrix
vertcat' x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__vertcat_TIC x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__vertsplit_TIC_TIC_TIC" c_CasADi__vertsplit_TIC_TIC_TIC
  :: Ptr DMatrix' -> Ptr (CppVec CInt) -> IO (Ptr (CppVec (Ptr DMatrix')))
vertsplit'''
  :: DMatrix -> Vector Int -> IO (Vector DMatrix)
vertsplit''' x0 x1 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  c_CasADi__vertsplit_TIC_TIC_TIC x0' x1' >>= wrapReturn

foreign import ccall unsafe "CasADi__vertsplit_TIC_TIC_TIC_TIC" c_CasADi__vertsplit_TIC_TIC_TIC_TIC
  :: Ptr DMatrix' -> CInt -> IO (Ptr (CppVec (Ptr DMatrix')))
vertsplit''''
  :: DMatrix -> Int -> IO (Vector DMatrix)
vertsplit'''' x0 x1 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  c_CasADi__vertsplit_TIC_TIC_TIC_TIC x0' x1' >>= wrapReturn

foreign import ccall unsafe "CasADi__vertsplit_TIC_TIC_TIC_TIC_TIC" c_CasADi__vertsplit_TIC_TIC_TIC_TIC_TIC
  :: Ptr DMatrix' -> IO (Ptr (CppVec (Ptr DMatrix')))
vertsplit'''''
  :: DMatrix -> IO (Vector DMatrix)
vertsplit''''' x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__vertsplit_TIC_TIC_TIC_TIC_TIC x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__horzcat_TIC" c_CasADi__horzcat_TIC
  :: Ptr (CppVec (Ptr DMatrix')) -> IO (Ptr DMatrix')
horzcat'
  :: Vector DMatrix -> IO DMatrix
horzcat' x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__horzcat_TIC x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__horzsplit_TIC_TIC_TIC" c_CasADi__horzsplit_TIC_TIC_TIC
  :: Ptr DMatrix' -> Ptr (CppVec CInt) -> IO (Ptr (CppVec (Ptr DMatrix')))
horzsplit'''
  :: DMatrix -> Vector Int -> IO (Vector DMatrix)
horzsplit''' x0 x1 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  c_CasADi__horzsplit_TIC_TIC_TIC x0' x1' >>= wrapReturn

foreign import ccall unsafe "CasADi__horzsplit_TIC_TIC_TIC_TIC" c_CasADi__horzsplit_TIC_TIC_TIC_TIC
  :: Ptr DMatrix' -> CInt -> IO (Ptr (CppVec (Ptr DMatrix')))
horzsplit''''
  :: DMatrix -> Int -> IO (Vector DMatrix)
horzsplit'''' x0 x1 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  c_CasADi__horzsplit_TIC_TIC_TIC_TIC x0' x1' >>= wrapReturn

foreign import ccall unsafe "CasADi__horzsplit_TIC_TIC_TIC_TIC_TIC" c_CasADi__horzsplit_TIC_TIC_TIC_TIC_TIC
  :: Ptr DMatrix' -> IO (Ptr (CppVec (Ptr DMatrix')))
horzsplit'''''
  :: DMatrix -> IO (Vector DMatrix)
horzsplit''''' x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__horzsplit_TIC_TIC_TIC_TIC_TIC x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__inner_prod_TIC" c_CasADi__inner_prod_TIC
  :: Ptr DMatrix' -> Ptr DMatrix' -> IO (Ptr DMatrix')
inner_prod'
  :: DMatrix -> DMatrix -> IO DMatrix
inner_prod' x0 x1 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  c_CasADi__inner_prod_TIC x0' x1' >>= wrapReturn

foreign import ccall unsafe "CasADi__outer_prod_TIC" c_CasADi__outer_prod_TIC
  :: Ptr DMatrix' -> Ptr DMatrix' -> IO (Ptr DMatrix')
outer_prod'
  :: DMatrix -> DMatrix -> IO DMatrix
outer_prod' x0 x1 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  c_CasADi__outer_prod_TIC x0' x1' >>= wrapReturn

foreign import ccall unsafe "CasADi__norm_1_TIC" c_CasADi__norm_1_TIC
  :: Ptr DMatrix' -> IO (Ptr DMatrix')
norm_1'
  :: DMatrix -> IO DMatrix
norm_1' x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__norm_1_TIC x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__norm_2_TIC" c_CasADi__norm_2_TIC
  :: Ptr DMatrix' -> IO (Ptr DMatrix')
norm_2'
  :: DMatrix -> IO DMatrix
norm_2' x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__norm_2_TIC x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__norm_inf_TIC" c_CasADi__norm_inf_TIC
  :: Ptr DMatrix' -> IO (Ptr DMatrix')
norm_inf'
  :: DMatrix -> IO DMatrix
norm_inf' x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__norm_inf_TIC x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__norm_F_TIC" c_CasADi__norm_F_TIC
  :: Ptr DMatrix' -> IO (Ptr DMatrix')
norm_F'
  :: DMatrix -> IO DMatrix
norm_F' x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__norm_F_TIC x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__qr_TIC" c_CasADi__qr_TIC
  :: Ptr DMatrix' -> Ptr DMatrix' -> Ptr DMatrix' -> IO ()
qr'
  :: DMatrix -> DMatrix -> DMatrix -> IO ()
qr' x0 x1 x2 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  withMarshal x2 $ \x2' ->
  c_CasADi__qr_TIC x0' x1' x2' >>= wrapReturn

foreign import ccall unsafe "CasADi__nullspace_TIC" c_CasADi__nullspace_TIC
  :: Ptr DMatrix' -> IO (Ptr DMatrix')
nullspace'
  :: DMatrix -> IO DMatrix
nullspace' x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__nullspace_TIC x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__solve_TIC" c_CasADi__solve_TIC
  :: Ptr DMatrix' -> Ptr DMatrix' -> IO (Ptr DMatrix')
solve'
  :: DMatrix -> DMatrix -> IO DMatrix
solve' x0 x1 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  c_CasADi__solve_TIC x0' x1' >>= wrapReturn

foreign import ccall unsafe "CasADi__pinv_TIC" c_CasADi__pinv_TIC
  :: Ptr DMatrix' -> IO (Ptr DMatrix')
pinv'
  :: DMatrix -> IO DMatrix
pinv' x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__pinv_TIC x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__repmat_TIC" c_CasADi__repmat_TIC
  :: Ptr DMatrix' -> CInt -> CInt -> IO (Ptr DMatrix')
repmat'
  :: DMatrix -> Int -> Int -> IO DMatrix
repmat' x0 x1 x2 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  withMarshal x2 $ \x2' ->
  c_CasADi__repmat_TIC x0' x1' x2' >>= wrapReturn

foreign import ccall unsafe "CasADi__unite_TIC" c_CasADi__unite_TIC
  :: Ptr DMatrix' -> Ptr DMatrix' -> IO (Ptr DMatrix')
unite'
  :: DMatrix -> DMatrix -> IO DMatrix
unite' x0 x1 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  c_CasADi__unite_TIC x0' x1' >>= wrapReturn

foreign import ccall unsafe "CasADi__sumRows_TIC" c_CasADi__sumRows_TIC
  :: Ptr DMatrix' -> IO (Ptr DMatrix')
sumRows'
  :: DMatrix -> IO DMatrix
sumRows' x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__sumRows_TIC x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__sumCols_TIC" c_CasADi__sumCols_TIC
  :: Ptr DMatrix' -> IO (Ptr DMatrix')
sumCols'
  :: DMatrix -> IO DMatrix
sumCols' x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__sumCols_TIC x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__sumAll_TIC" c_CasADi__sumAll_TIC
  :: Ptr DMatrix' -> IO (Ptr DMatrix')
sumAll'
  :: DMatrix -> IO DMatrix
sumAll' x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__sumAll_TIC x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__trace_TIC" c_CasADi__trace_TIC
  :: Ptr DMatrix' -> IO CDouble
trace'
  :: DMatrix -> IO Double
trace' x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__trace_TIC x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__diag_TIC" c_CasADi__diag_TIC
  :: Ptr DMatrix' -> IO (Ptr DMatrix')
diag'
  :: DMatrix -> IO DMatrix
diag' x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__diag_TIC x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__blkdiag_TIC" c_CasADi__blkdiag_TIC
  :: Ptr (CppVec (Ptr DMatrix')) -> IO (Ptr DMatrix')
blkdiag'
  :: Vector DMatrix -> IO DMatrix
blkdiag' x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__blkdiag_TIC x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__polyval_TIC" c_CasADi__polyval_TIC
  :: Ptr DMatrix' -> Ptr DMatrix' -> IO (Ptr DMatrix')
polyval'
  :: DMatrix -> DMatrix -> IO DMatrix
polyval' x0 x1 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  c_CasADi__polyval_TIC x0' x1' >>= wrapReturn

foreign import ccall unsafe "CasADi__addMultiple_TIC_TIC" c_CasADi__addMultiple_TIC_TIC
  :: Ptr DMatrix' -> Ptr (CppVec CDouble) -> Ptr (CppVec CDouble) -> CInt -> IO ()
addMultiple''
  :: DMatrix -> Vector Double -> Vector Double -> Bool -> IO ()
addMultiple'' x0 x1 x2 x3 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  withMarshal x2 $ \x2' ->
  withMarshal x3 $ \x3' ->
  c_CasADi__addMultiple_TIC_TIC x0' x1' x2' x3' >>= wrapReturn

foreign import ccall unsafe "CasADi__addMultiple_TIC_TIC_TIC" c_CasADi__addMultiple_TIC_TIC_TIC
  :: Ptr DMatrix' -> Ptr (CppVec CDouble) -> Ptr (CppVec CDouble) -> IO ()
addMultiple'''
  :: DMatrix -> Vector Double -> Vector Double -> IO ()
addMultiple''' x0 x1 x2 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  withMarshal x2 $ \x2' ->
  c_CasADi__addMultiple_TIC_TIC_TIC x0' x1' x2' >>= wrapReturn

foreign import ccall unsafe "CasADi__veccat_TIC" c_CasADi__veccat_TIC
  :: Ptr (CppVec (Ptr DMatrix')) -> IO (Ptr DMatrix')
veccat'
  :: Vector DMatrix -> IO DMatrix
veccat' x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__veccat_TIC x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__vecNZcat_TIC" c_CasADi__vecNZcat_TIC
  :: Ptr (CppVec (Ptr DMatrix')) -> IO (Ptr DMatrix')
vecNZcat'
  :: Vector DMatrix -> IO DMatrix
vecNZcat' x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__vecNZcat_TIC x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__project_TIC" c_CasADi__project_TIC
  :: Ptr DMatrix' -> Ptr Sparsity' -> IO (Ptr DMatrix')
project'
  :: DMatrix -> Sparsity -> IO DMatrix
project' x0 x1 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  c_CasADi__project_TIC x0' x1' >>= wrapReturn

foreign import ccall unsafe "CasADi__sprank_TIC" c_CasADi__sprank_TIC
  :: Ptr DMatrix' -> IO CInt
sprank'
  :: DMatrix -> IO Int
sprank' x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__sprank_TIC x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__kron_TIC" c_CasADi__kron_TIC
  :: Ptr DMatrix' -> Ptr DMatrix' -> IO (Ptr DMatrix')
kron'
  :: DMatrix -> DMatrix -> IO DMatrix
kron' x0 x1 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  c_CasADi__kron_TIC x0' x1' >>= wrapReturn

foreign import ccall unsafe "CasADi__sparse_TIC_TIC" c_CasADi__sparse_TIC_TIC
  :: Ptr DMatrix' -> CDouble -> IO (Ptr DMatrix')
sparse''
  :: DMatrix -> Double -> IO DMatrix
sparse'' x0 x1 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  c_CasADi__sparse_TIC_TIC x0' x1' >>= wrapReturn

foreign import ccall unsafe "CasADi__sparse_TIC_TIC_TIC" c_CasADi__sparse_TIC_TIC_TIC
  :: Ptr DMatrix' -> IO (Ptr DMatrix')
sparse'''
  :: DMatrix -> IO DMatrix
sparse''' x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__sparse_TIC_TIC_TIC x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__dense_TIC" c_CasADi__dense_TIC
  :: Ptr DMatrix' -> IO (Ptr DMatrix')
dense'
  :: DMatrix -> IO DMatrix
dense' x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__dense_TIC x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__transpose_TIC_TIC" c_CasADi__transpose_TIC_TIC
  :: Ptr SX' -> IO (Ptr SX')
transpose''
  :: SX -> IO SX
transpose'' x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__transpose_TIC_TIC x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__mul_TIC_TIC_TIC_TIC_TIC_TIC" c_CasADi__mul_TIC_TIC_TIC_TIC_TIC_TIC
  :: Ptr SX' -> Ptr SX' -> Ptr Sparsity' -> IO (Ptr SX')
mul''''''
  :: SX -> SX -> Sparsity -> IO SX
mul'''''' x0 x1 x2 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  withMarshal x2 $ \x2' ->
  c_CasADi__mul_TIC_TIC_TIC_TIC_TIC_TIC x0' x1' x2' >>= wrapReturn

foreign import ccall unsafe "CasADi__mul_TIC_TIC_TIC_TIC_TIC_TIC_TIC" c_CasADi__mul_TIC_TIC_TIC_TIC_TIC_TIC_TIC
  :: Ptr SX' -> Ptr SX' -> IO (Ptr SX')
mul'''''''
  :: SX -> SX -> IO SX
mul''''''' x0 x1 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  c_CasADi__mul_TIC_TIC_TIC_TIC_TIC_TIC_TIC x0' x1' >>= wrapReturn

foreign import ccall unsafe "CasADi__mul_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC" c_CasADi__mul_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC
  :: Ptr (CppVec (Ptr SX')) -> IO (Ptr SX')
mul''''''''
  :: Vector SX -> IO SX
mul'''''''' x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__mul_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__det_TIC_TIC" c_CasADi__det_TIC_TIC
  :: Ptr SX' -> IO (Ptr SXElement')
det''
  :: SX -> IO SXElement
det'' x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__det_TIC_TIC x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__getMinor_TIC_TIC" c_CasADi__getMinor_TIC_TIC
  :: Ptr SX' -> CInt -> CInt -> IO (Ptr SXElement')
getMinor''
  :: SX -> Int -> Int -> IO SXElement
getMinor'' x0 x1 x2 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  withMarshal x2 $ \x2' ->
  c_CasADi__getMinor_TIC_TIC x0' x1' x2' >>= wrapReturn

foreign import ccall unsafe "CasADi__cofactor_TIC_TIC" c_CasADi__cofactor_TIC_TIC
  :: Ptr SX' -> CInt -> CInt -> IO (Ptr SXElement')
cofactor''
  :: SX -> Int -> Int -> IO SXElement
cofactor'' x0 x1 x2 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  withMarshal x2 $ \x2' ->
  c_CasADi__cofactor_TIC_TIC x0' x1' x2' >>= wrapReturn

foreign import ccall unsafe "CasADi__adj_TIC_TIC" c_CasADi__adj_TIC_TIC
  :: Ptr SX' -> IO (Ptr SX')
adj''
  :: SX -> IO SX
adj'' x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__adj_TIC_TIC x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__inv_TIC_TIC" c_CasADi__inv_TIC_TIC
  :: Ptr SX' -> IO (Ptr SX')
inv''
  :: SX -> IO SX
inv'' x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__inv_TIC_TIC x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__reshape_TIC_TIC_TIC_TIC" c_CasADi__reshape_TIC_TIC_TIC_TIC
  :: Ptr SX' -> CInt -> CInt -> IO (Ptr SX')
reshape''''
  :: SX -> Int -> Int -> IO SX
reshape'''' x0 x1 x2 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  withMarshal x2 $ \x2' ->
  c_CasADi__reshape_TIC_TIC_TIC_TIC x0' x1' x2' >>= wrapReturn

foreign import ccall unsafe "CasADi__reshape_TIC_TIC_TIC_TIC_TIC" c_CasADi__reshape_TIC_TIC_TIC_TIC_TIC
  :: Ptr SX' -> Ptr Sparsity' -> IO (Ptr SX')
reshape'''''
  :: SX -> Sparsity -> IO SX
reshape''''' x0 x1 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  c_CasADi__reshape_TIC_TIC_TIC_TIC_TIC x0' x1' >>= wrapReturn

foreign import ccall unsafe "CasADi__vec_TIC_TIC" c_CasADi__vec_TIC_TIC
  :: Ptr SX' -> IO (Ptr SX')
vec''
  :: SX -> IO SX
vec'' x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__vec_TIC_TIC x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__vecNZ_TIC_TIC" c_CasADi__vecNZ_TIC_TIC
  :: Ptr SX' -> IO (Ptr SX')
vecNZ''
  :: SX -> IO SX
vecNZ'' x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__vecNZ_TIC_TIC x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__blockcat_TIC_TIC" c_CasADi__blockcat_TIC_TIC
  :: Ptr (CppVec (Ptr (CppVec (Ptr SX')))) -> IO (Ptr SX')
blockcat''
  :: Vector (Vector SX) -> IO SX
blockcat'' x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__blockcat_TIC_TIC x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__blocksplit_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC" c_CasADi__blocksplit_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC
  :: Ptr SX' -> Ptr (CppVec CInt) -> Ptr (CppVec CInt) -> IO (Ptr (CppVec (Ptr (CppVec (Ptr SX')))))
blocksplit''''''''
  :: SX -> Vector Int -> Vector Int -> IO (Vector (Vector SX))
blocksplit'''''''' x0 x1 x2 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  withMarshal x2 $ \x2' ->
  c_CasADi__blocksplit_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC x0' x1' x2' >>= wrapReturn

foreign import ccall unsafe "CasADi__blocksplit_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC" c_CasADi__blocksplit_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC
  :: Ptr SX' -> CInt -> CInt -> IO (Ptr (CppVec (Ptr (CppVec (Ptr SX')))))
blocksplit'''''''''
  :: SX -> Int -> Int -> IO (Vector (Vector SX))
blocksplit''''''''' x0 x1 x2 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  withMarshal x2 $ \x2' ->
  c_CasADi__blocksplit_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC x0' x1' x2' >>= wrapReturn

foreign import ccall unsafe "CasADi__blocksplit_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC" c_CasADi__blocksplit_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC
  :: Ptr SX' -> CInt -> IO (Ptr (CppVec (Ptr (CppVec (Ptr SX')))))
blocksplit''''''''''
  :: SX -> Int -> IO (Vector (Vector SX))
blocksplit'''''''''' x0 x1 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  c_CasADi__blocksplit_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC x0' x1' >>= wrapReturn

foreign import ccall unsafe "CasADi__blocksplit_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC" c_CasADi__blocksplit_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC
  :: Ptr SX' -> IO (Ptr (CppVec (Ptr (CppVec (Ptr SX')))))
blocksplit'''''''''''
  :: SX -> IO (Vector (Vector SX))
blocksplit''''''''''' x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__blocksplit_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__vertcat_TIC_TIC" c_CasADi__vertcat_TIC_TIC
  :: Ptr (CppVec (Ptr SX')) -> IO (Ptr SX')
vertcat''
  :: Vector SX -> IO SX
vertcat'' x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__vertcat_TIC_TIC x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__vertsplit_TIC_TIC_TIC_TIC_TIC_TIC" c_CasADi__vertsplit_TIC_TIC_TIC_TIC_TIC_TIC
  :: Ptr SX' -> Ptr (CppVec CInt) -> IO (Ptr (CppVec (Ptr SX')))
vertsplit''''''
  :: SX -> Vector Int -> IO (Vector SX)
vertsplit'''''' x0 x1 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  c_CasADi__vertsplit_TIC_TIC_TIC_TIC_TIC_TIC x0' x1' >>= wrapReturn

foreign import ccall unsafe "CasADi__vertsplit_TIC_TIC_TIC_TIC_TIC_TIC_TIC" c_CasADi__vertsplit_TIC_TIC_TIC_TIC_TIC_TIC_TIC
  :: Ptr SX' -> CInt -> IO (Ptr (CppVec (Ptr SX')))
vertsplit'''''''
  :: SX -> Int -> IO (Vector SX)
vertsplit''''''' x0 x1 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  c_CasADi__vertsplit_TIC_TIC_TIC_TIC_TIC_TIC_TIC x0' x1' >>= wrapReturn

foreign import ccall unsafe "CasADi__vertsplit_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC" c_CasADi__vertsplit_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC
  :: Ptr SX' -> IO (Ptr (CppVec (Ptr SX')))
vertsplit''''''''
  :: SX -> IO (Vector SX)
vertsplit'''''''' x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__vertsplit_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__horzcat_TIC_TIC" c_CasADi__horzcat_TIC_TIC
  :: Ptr (CppVec (Ptr SX')) -> IO (Ptr SX')
horzcat''
  :: Vector SX -> IO SX
horzcat'' x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__horzcat_TIC_TIC x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__horzsplit_TIC_TIC_TIC_TIC_TIC_TIC" c_CasADi__horzsplit_TIC_TIC_TIC_TIC_TIC_TIC
  :: Ptr SX' -> Ptr (CppVec CInt) -> IO (Ptr (CppVec (Ptr SX')))
horzsplit''''''
  :: SX -> Vector Int -> IO (Vector SX)
horzsplit'''''' x0 x1 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  c_CasADi__horzsplit_TIC_TIC_TIC_TIC_TIC_TIC x0' x1' >>= wrapReturn

foreign import ccall unsafe "CasADi__horzsplit_TIC_TIC_TIC_TIC_TIC_TIC_TIC" c_CasADi__horzsplit_TIC_TIC_TIC_TIC_TIC_TIC_TIC
  :: Ptr SX' -> CInt -> IO (Ptr (CppVec (Ptr SX')))
horzsplit'''''''
  :: SX -> Int -> IO (Vector SX)
horzsplit''''''' x0 x1 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  c_CasADi__horzsplit_TIC_TIC_TIC_TIC_TIC_TIC_TIC x0' x1' >>= wrapReturn

foreign import ccall unsafe "CasADi__horzsplit_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC" c_CasADi__horzsplit_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC
  :: Ptr SX' -> IO (Ptr (CppVec (Ptr SX')))
horzsplit''''''''
  :: SX -> IO (Vector SX)
horzsplit'''''''' x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__horzsplit_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__inner_prod_TIC_TIC" c_CasADi__inner_prod_TIC_TIC
  :: Ptr SX' -> Ptr SX' -> IO (Ptr SX')
inner_prod''
  :: SX -> SX -> IO SX
inner_prod'' x0 x1 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  c_CasADi__inner_prod_TIC_TIC x0' x1' >>= wrapReturn

foreign import ccall unsafe "CasADi__outer_prod_TIC_TIC" c_CasADi__outer_prod_TIC_TIC
  :: Ptr SX' -> Ptr SX' -> IO (Ptr SX')
outer_prod''
  :: SX -> SX -> IO SX
outer_prod'' x0 x1 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  c_CasADi__outer_prod_TIC_TIC x0' x1' >>= wrapReturn

foreign import ccall unsafe "CasADi__norm_1_TIC_TIC" c_CasADi__norm_1_TIC_TIC
  :: Ptr SX' -> IO (Ptr SX')
norm_1''
  :: SX -> IO SX
norm_1'' x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__norm_1_TIC_TIC x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__norm_2_TIC_TIC" c_CasADi__norm_2_TIC_TIC
  :: Ptr SX' -> IO (Ptr SX')
norm_2''
  :: SX -> IO SX
norm_2'' x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__norm_2_TIC_TIC x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__norm_inf_TIC_TIC" c_CasADi__norm_inf_TIC_TIC
  :: Ptr SX' -> IO (Ptr SX')
norm_inf''
  :: SX -> IO SX
norm_inf'' x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__norm_inf_TIC_TIC x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__norm_F_TIC_TIC" c_CasADi__norm_F_TIC_TIC
  :: Ptr SX' -> IO (Ptr SX')
norm_F''
  :: SX -> IO SX
norm_F'' x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__norm_F_TIC_TIC x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__qr_TIC_TIC" c_CasADi__qr_TIC_TIC
  :: Ptr SX' -> Ptr SX' -> Ptr SX' -> IO ()
qr''
  :: SX -> SX -> SX -> IO ()
qr'' x0 x1 x2 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  withMarshal x2 $ \x2' ->
  c_CasADi__qr_TIC_TIC x0' x1' x2' >>= wrapReturn

foreign import ccall unsafe "CasADi__nullspace_TIC_TIC" c_CasADi__nullspace_TIC_TIC
  :: Ptr SX' -> IO (Ptr SX')
nullspace''
  :: SX -> IO SX
nullspace'' x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__nullspace_TIC_TIC x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__solve_TIC_TIC" c_CasADi__solve_TIC_TIC
  :: Ptr SX' -> Ptr SX' -> IO (Ptr SX')
solve''
  :: SX -> SX -> IO SX
solve'' x0 x1 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  c_CasADi__solve_TIC_TIC x0' x1' >>= wrapReturn

foreign import ccall unsafe "CasADi__pinv_TIC_TIC" c_CasADi__pinv_TIC_TIC
  :: Ptr SX' -> IO (Ptr SX')
pinv''
  :: SX -> IO SX
pinv'' x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__pinv_TIC_TIC x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__repmat_TIC_TIC" c_CasADi__repmat_TIC_TIC
  :: Ptr SX' -> CInt -> CInt -> IO (Ptr SX')
repmat''
  :: SX -> Int -> Int -> IO SX
repmat'' x0 x1 x2 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  withMarshal x2 $ \x2' ->
  c_CasADi__repmat_TIC_TIC x0' x1' x2' >>= wrapReturn

foreign import ccall unsafe "CasADi__unite_TIC_TIC" c_CasADi__unite_TIC_TIC
  :: Ptr SX' -> Ptr SX' -> IO (Ptr SX')
unite''
  :: SX -> SX -> IO SX
unite'' x0 x1 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  c_CasADi__unite_TIC_TIC x0' x1' >>= wrapReturn

foreign import ccall unsafe "CasADi__sumRows_TIC_TIC" c_CasADi__sumRows_TIC_TIC
  :: Ptr SX' -> IO (Ptr SX')
sumRows''
  :: SX -> IO SX
sumRows'' x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__sumRows_TIC_TIC x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__sumCols_TIC_TIC" c_CasADi__sumCols_TIC_TIC
  :: Ptr SX' -> IO (Ptr SX')
sumCols''
  :: SX -> IO SX
sumCols'' x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__sumCols_TIC_TIC x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__sumAll_TIC_TIC" c_CasADi__sumAll_TIC_TIC
  :: Ptr SX' -> IO (Ptr SX')
sumAll''
  :: SX -> IO SX
sumAll'' x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__sumAll_TIC_TIC x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__trace_TIC_TIC" c_CasADi__trace_TIC_TIC
  :: Ptr SX' -> IO (Ptr SXElement')
trace''
  :: SX -> IO SXElement
trace'' x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__trace_TIC_TIC x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__diag_TIC_TIC" c_CasADi__diag_TIC_TIC
  :: Ptr SX' -> IO (Ptr SX')
diag''
  :: SX -> IO SX
diag'' x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__diag_TIC_TIC x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__blkdiag_TIC_TIC" c_CasADi__blkdiag_TIC_TIC
  :: Ptr (CppVec (Ptr SX')) -> IO (Ptr SX')
blkdiag''
  :: Vector SX -> IO SX
blkdiag'' x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__blkdiag_TIC_TIC x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__polyval_TIC_TIC" c_CasADi__polyval_TIC_TIC
  :: Ptr SX' -> Ptr SX' -> IO (Ptr SX')
polyval''
  :: SX -> SX -> IO SX
polyval'' x0 x1 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  c_CasADi__polyval_TIC_TIC x0' x1' >>= wrapReturn

foreign import ccall unsafe "CasADi__addMultiple_TIC_TIC_TIC_TIC" c_CasADi__addMultiple_TIC_TIC_TIC_TIC
  :: Ptr SX' -> Ptr (CppVec (Ptr SXElement')) -> Ptr (CppVec (Ptr SXElement')) -> CInt -> IO ()
addMultiple''''
  :: SX -> Vector SXElement -> Vector SXElement -> Bool -> IO ()
addMultiple'''' x0 x1 x2 x3 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  withMarshal x2 $ \x2' ->
  withMarshal x3 $ \x3' ->
  c_CasADi__addMultiple_TIC_TIC_TIC_TIC x0' x1' x2' x3' >>= wrapReturn

foreign import ccall unsafe "CasADi__addMultiple_TIC_TIC_TIC_TIC_TIC" c_CasADi__addMultiple_TIC_TIC_TIC_TIC_TIC
  :: Ptr SX' -> Ptr (CppVec (Ptr SXElement')) -> Ptr (CppVec (Ptr SXElement')) -> IO ()
addMultiple'''''
  :: SX -> Vector SXElement -> Vector SXElement -> IO ()
addMultiple''''' x0 x1 x2 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  withMarshal x2 $ \x2' ->
  c_CasADi__addMultiple_TIC_TIC_TIC_TIC_TIC x0' x1' x2' >>= wrapReturn

foreign import ccall unsafe "CasADi__veccat_TIC_TIC" c_CasADi__veccat_TIC_TIC
  :: Ptr (CppVec (Ptr SX')) -> IO (Ptr SX')
veccat''
  :: Vector SX -> IO SX
veccat'' x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__veccat_TIC_TIC x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__vecNZcat_TIC_TIC" c_CasADi__vecNZcat_TIC_TIC
  :: Ptr (CppVec (Ptr SX')) -> IO (Ptr SX')
vecNZcat''
  :: Vector SX -> IO SX
vecNZcat'' x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__vecNZcat_TIC_TIC x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__project_TIC_TIC" c_CasADi__project_TIC_TIC
  :: Ptr SX' -> Ptr Sparsity' -> IO (Ptr SX')
project''
  :: SX -> Sparsity -> IO SX
project'' x0 x1 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  c_CasADi__project_TIC_TIC x0' x1' >>= wrapReturn

foreign import ccall unsafe "CasADi__sprank_TIC_TIC" c_CasADi__sprank_TIC_TIC
  :: Ptr SX' -> IO CInt
sprank''
  :: SX -> IO Int
sprank'' x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__sprank_TIC_TIC x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__kron_TIC_TIC" c_CasADi__kron_TIC_TIC
  :: Ptr SX' -> Ptr SX' -> IO (Ptr SX')
kron''
  :: SX -> SX -> IO SX
kron'' x0 x1 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  c_CasADi__kron_TIC_TIC x0' x1' >>= wrapReturn

foreign import ccall unsafe "CasADi__sparse_TIC_TIC_TIC_TIC" c_CasADi__sparse_TIC_TIC_TIC_TIC
  :: Ptr SX' -> CDouble -> IO (Ptr SX')
sparse''''
  :: SX -> Double -> IO SX
sparse'''' x0 x1 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  c_CasADi__sparse_TIC_TIC_TIC_TIC x0' x1' >>= wrapReturn

foreign import ccall unsafe "CasADi__sparse_TIC_TIC_TIC_TIC_TIC" c_CasADi__sparse_TIC_TIC_TIC_TIC_TIC
  :: Ptr SX' -> IO (Ptr SX')
sparse'''''
  :: SX -> IO SX
sparse''''' x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__sparse_TIC_TIC_TIC_TIC_TIC x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__dense_TIC_TIC" c_CasADi__dense_TIC_TIC
  :: Ptr SX' -> IO (Ptr SX')
dense''
  :: SX -> IO SX
dense'' x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__dense_TIC_TIC x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__cross" c_CasADi__cross
  :: Ptr GenIMatrix' -> Ptr GenIMatrix' -> CInt -> IO (Ptr IMatrix')
{-|
>Matlab's cross command.
-}
cross
  :: GenIMatrix -> GenIMatrix -> Int -> IO IMatrix
cross x0 x1 x2 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  withMarshal x2 $ \x2' ->
  c_CasADi__cross x0' x1' x2' >>= wrapReturn

foreign import ccall unsafe "CasADi__cross_TIC" c_CasADi__cross_TIC
  :: Ptr GenIMatrix' -> Ptr GenIMatrix' -> IO (Ptr IMatrix')
cross'
  :: GenIMatrix -> GenIMatrix -> IO IMatrix
cross' x0 x1 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  c_CasADi__cross_TIC x0' x1' >>= wrapReturn

foreign import ccall unsafe "CasADi__tril2symm" c_CasADi__tril2symm
  :: Ptr GenIMatrix' -> IO (Ptr IMatrix')
{-|
>Convert a lower triangular matrix to a symmetric one.
-}
tril2symm
  :: GenIMatrix -> IO IMatrix
tril2symm x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__tril2symm x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__triu2symm" c_CasADi__triu2symm
  :: Ptr GenIMatrix' -> IO (Ptr IMatrix')
{-|
>Convert a upper triangular matrix to a symmetric one.
-}
triu2symm
  :: GenIMatrix -> IO IMatrix
triu2symm x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__triu2symm x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__triu" c_CasADi__triu
  :: Ptr GenIMatrix' -> IO (Ptr IMatrix')
{-|
>>  MatType CasADi::triu(const GenericMatrix< MatType > &a)
>------------------------------------------------------------------------
>
>Get the upper triangular part of a matrix.
>
>>  Sparsity CasADi::triu(const Sparsity &sp, bool includeDiagonal=true)
>------------------------------------------------------------------------
>
>Get upper triangular part.
-}
triu
  :: GenIMatrix -> IO IMatrix
triu x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__triu x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__tril" c_CasADi__tril
  :: Ptr GenIMatrix' -> IO (Ptr IMatrix')
{-|
>>  MatType CasADi::tril(const GenericMatrix< MatType > &a)
>------------------------------------------------------------------------
>
>Get the lower triangular part of a matrix.
>
>>  Sparsity CasADi::tril(const Sparsity &sp, bool includeDiagonal=true)
>------------------------------------------------------------------------
>
>Get lower triangular part.
-}
tril
  :: GenIMatrix -> IO IMatrix
tril x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__tril x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__isEqual" c_CasADi__isEqual
  :: Ptr GenIMatrix' -> Ptr GenIMatrix' -> IO CInt
{-|
>>  bool CasADi::isEqual(const GenericMatrix< MatType > &x, const GenericMatrix< MatType > &y)
>------------------------------------------------------------------------
>
>Check if two expressions are equal, assuming that they are comparible.
>
>>  bool CasADi::isEqual(const MX &ex1, const MX &ex2)
>------------------------------------------------------------------------
>[INTERNAL] 
-}
isEqual
  :: GenIMatrix -> GenIMatrix -> IO Bool
isEqual x0 x1 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  c_CasADi__isEqual x0' x1' >>= wrapReturn

foreign import ccall unsafe "CasADi__cross_TIC_TIC" c_CasADi__cross_TIC_TIC
  :: Ptr GenDMatrix' -> Ptr GenDMatrix' -> CInt -> IO (Ptr DMatrix')
cross''
  :: GenDMatrix -> GenDMatrix -> Int -> IO DMatrix
cross'' x0 x1 x2 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  withMarshal x2 $ \x2' ->
  c_CasADi__cross_TIC_TIC x0' x1' x2' >>= wrapReturn

foreign import ccall unsafe "CasADi__cross_TIC_TIC_TIC" c_CasADi__cross_TIC_TIC_TIC
  :: Ptr GenDMatrix' -> Ptr GenDMatrix' -> IO (Ptr DMatrix')
cross'''
  :: GenDMatrix -> GenDMatrix -> IO DMatrix
cross''' x0 x1 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  c_CasADi__cross_TIC_TIC_TIC x0' x1' >>= wrapReturn

foreign import ccall unsafe "CasADi__tril2symm_TIC" c_CasADi__tril2symm_TIC
  :: Ptr GenDMatrix' -> IO (Ptr DMatrix')
tril2symm'
  :: GenDMatrix -> IO DMatrix
tril2symm' x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__tril2symm_TIC x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__triu2symm_TIC" c_CasADi__triu2symm_TIC
  :: Ptr GenDMatrix' -> IO (Ptr DMatrix')
triu2symm'
  :: GenDMatrix -> IO DMatrix
triu2symm' x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__triu2symm_TIC x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__triu_TIC" c_CasADi__triu_TIC
  :: Ptr GenDMatrix' -> IO (Ptr DMatrix')
triu'
  :: GenDMatrix -> IO DMatrix
triu' x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__triu_TIC x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__tril_TIC" c_CasADi__tril_TIC
  :: Ptr GenDMatrix' -> IO (Ptr DMatrix')
tril'
  :: GenDMatrix -> IO DMatrix
tril' x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__tril_TIC x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__isEqual_TIC" c_CasADi__isEqual_TIC
  :: Ptr GenDMatrix' -> Ptr GenDMatrix' -> IO CInt
isEqual'
  :: GenDMatrix -> GenDMatrix -> IO Bool
isEqual' x0 x1 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  c_CasADi__isEqual_TIC x0' x1' >>= wrapReturn

foreign import ccall unsafe "CasADi__cross_TIC_TIC_TIC_TIC" c_CasADi__cross_TIC_TIC_TIC_TIC
  :: Ptr GenSX' -> Ptr GenSX' -> CInt -> IO (Ptr SX')
cross''''
  :: GenSX -> GenSX -> Int -> IO SX
cross'''' x0 x1 x2 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  withMarshal x2 $ \x2' ->
  c_CasADi__cross_TIC_TIC_TIC_TIC x0' x1' x2' >>= wrapReturn

foreign import ccall unsafe "CasADi__cross_TIC_TIC_TIC_TIC_TIC" c_CasADi__cross_TIC_TIC_TIC_TIC_TIC
  :: Ptr GenSX' -> Ptr GenSX' -> IO (Ptr SX')
cross'''''
  :: GenSX -> GenSX -> IO SX
cross''''' x0 x1 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  c_CasADi__cross_TIC_TIC_TIC_TIC_TIC x0' x1' >>= wrapReturn

foreign import ccall unsafe "CasADi__tril2symm_TIC_TIC" c_CasADi__tril2symm_TIC_TIC
  :: Ptr GenSX' -> IO (Ptr SX')
tril2symm''
  :: GenSX -> IO SX
tril2symm'' x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__tril2symm_TIC_TIC x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__triu2symm_TIC_TIC" c_CasADi__triu2symm_TIC_TIC
  :: Ptr GenSX' -> IO (Ptr SX')
triu2symm''
  :: GenSX -> IO SX
triu2symm'' x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__triu2symm_TIC_TIC x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__triu_TIC_TIC" c_CasADi__triu_TIC_TIC
  :: Ptr GenSX' -> IO (Ptr SX')
triu''
  :: GenSX -> IO SX
triu'' x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__triu_TIC_TIC x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__tril_TIC_TIC" c_CasADi__tril_TIC_TIC
  :: Ptr GenSX' -> IO (Ptr SX')
tril''
  :: GenSX -> IO SX
tril'' x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__tril_TIC_TIC x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__isEqual_TIC_TIC" c_CasADi__isEqual_TIC_TIC
  :: Ptr GenSX' -> Ptr GenSX' -> IO CInt
isEqual''
  :: GenSX -> GenSX -> IO Bool
isEqual'' x0 x1 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  c_CasADi__isEqual_TIC_TIC x0' x1' >>= wrapReturn

foreign import ccall unsafe "CasADi__cross_TIC_TIC_TIC_TIC_TIC_TIC" c_CasADi__cross_TIC_TIC_TIC_TIC_TIC_TIC
  :: Ptr GenMX' -> Ptr GenMX' -> CInt -> IO (Ptr MX')
cross''''''
  :: GenMX -> GenMX -> Int -> IO MX
cross'''''' x0 x1 x2 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  withMarshal x2 $ \x2' ->
  c_CasADi__cross_TIC_TIC_TIC_TIC_TIC_TIC x0' x1' x2' >>= wrapReturn

foreign import ccall unsafe "CasADi__cross_TIC_TIC_TIC_TIC_TIC_TIC_TIC" c_CasADi__cross_TIC_TIC_TIC_TIC_TIC_TIC_TIC
  :: Ptr GenMX' -> Ptr GenMX' -> IO (Ptr MX')
cross'''''''
  :: GenMX -> GenMX -> IO MX
cross''''''' x0 x1 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  c_CasADi__cross_TIC_TIC_TIC_TIC_TIC_TIC_TIC x0' x1' >>= wrapReturn

foreign import ccall unsafe "CasADi__tril2symm_TIC_TIC_TIC" c_CasADi__tril2symm_TIC_TIC_TIC
  :: Ptr GenMX' -> IO (Ptr MX')
tril2symm'''
  :: GenMX -> IO MX
tril2symm''' x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__tril2symm_TIC_TIC_TIC x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__triu2symm_TIC_TIC_TIC" c_CasADi__triu2symm_TIC_TIC_TIC
  :: Ptr GenMX' -> IO (Ptr MX')
triu2symm'''
  :: GenMX -> IO MX
triu2symm''' x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__triu2symm_TIC_TIC_TIC x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__triu_TIC_TIC_TIC" c_CasADi__triu_TIC_TIC_TIC
  :: Ptr GenMX' -> IO (Ptr MX')
triu'''
  :: GenMX -> IO MX
triu''' x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__triu_TIC_TIC_TIC x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__tril_TIC_TIC_TIC" c_CasADi__tril_TIC_TIC_TIC
  :: Ptr GenMX' -> IO (Ptr MX')
tril'''
  :: GenMX -> IO MX
tril''' x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__tril_TIC_TIC_TIC x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__isEqual_TIC_TIC_TIC" c_CasADi__isEqual_TIC_TIC_TIC
  :: Ptr GenMX' -> Ptr GenMX' -> IO CInt
isEqual'''
  :: GenMX -> GenMX -> IO Bool
isEqual''' x0 x1 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  c_CasADi__isEqual_TIC_TIC_TIC x0' x1' >>= wrapReturn

foreign import ccall unsafe "CasADi__linspace" c_CasADi__linspace
  :: Ptr GenDMatrix' -> Ptr GenDMatrix' -> CInt -> IO (Ptr DMatrix')
{-|
>>  MatType CasADi::linspace(const GenericMatrix< MatType > &a, const GenericMatrix< MatType > &b, int nsteps)
>------------------------------------------------------------------------
>
>Matlab's linspace command.
>
>>  void CasADi::linspace(std::vector< T > &v, const F &first, const L &last)
>------------------------------------------------------------------------
>[INTERNAL] 
>Matlab's linspace.
-}
linspace
  :: GenDMatrix -> GenDMatrix -> Int -> IO DMatrix
linspace x0 x1 x2 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  withMarshal x2 $ \x2' ->
  c_CasADi__linspace x0' x1' x2' >>= wrapReturn

foreign import ccall unsafe "CasADi__linspace_TIC" c_CasADi__linspace_TIC
  :: Ptr GenSX' -> Ptr GenSX' -> CInt -> IO (Ptr SX')
linspace'
  :: GenSX -> GenSX -> Int -> IO SX
linspace' x0 x1 x2 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  withMarshal x2 $ \x2' ->
  c_CasADi__linspace_TIC x0' x1' x2' >>= wrapReturn

foreign import ccall unsafe "CasADi__linspace_TIC_TIC" c_CasADi__linspace_TIC_TIC
  :: Ptr GenMX' -> Ptr GenMX' -> CInt -> IO (Ptr MX')
linspace''
  :: GenMX -> GenMX -> Int -> IO MX
linspace'' x0 x1 x2 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  withMarshal x2 $ \x2' ->
  c_CasADi__linspace_TIC_TIC x0' x1' x2' >>= wrapReturn

foreign import ccall unsafe "CasADi__logic_and" c_CasADi__logic_and
  :: CInt -> CInt -> IO CInt
{-|
>Logical and, returns (an expression evaluating to) 1 if both expressions are
>nonzero and 0 otherwise.
-}
logic_and
  :: Int -> Int -> IO Int
logic_and x0 x1 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  c_CasADi__logic_and x0' x1' >>= wrapReturn

foreign import ccall unsafe "CasADi__logic_or" c_CasADi__logic_or
  :: CInt -> CInt -> IO CInt
{-|
>Logical or, returns (an expression evaluating to) 1 if at least one
>expression is nonzero and 0 otherwise.
-}
logic_or
  :: Int -> Int -> IO Int
logic_or x0 x1 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  c_CasADi__logic_or x0' x1' >>= wrapReturn

foreign import ccall unsafe "CasADi__logic_not" c_CasADi__logic_not
  :: CInt -> IO CInt
{-|
>Logical not, returns (an expression evaluating to) 1 if expression is zero
>and 0 otherwise.
-}
logic_not
  :: Int -> IO Int
logic_not x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__logic_not x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__logic_and_TIC" c_CasADi__logic_and_TIC
  :: CDouble -> CDouble -> IO CDouble
logic_and'
  :: Double -> Double -> IO Double
logic_and' x0 x1 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  c_CasADi__logic_and_TIC x0' x1' >>= wrapReturn

foreign import ccall unsafe "CasADi__logic_or_TIC" c_CasADi__logic_or_TIC
  :: CDouble -> CDouble -> IO CDouble
logic_or'
  :: Double -> Double -> IO Double
logic_or' x0 x1 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  c_CasADi__logic_or_TIC x0' x1' >>= wrapReturn

foreign import ccall unsafe "CasADi__logic_not_TIC" c_CasADi__logic_not_TIC
  :: CDouble -> IO CDouble
logic_not'
  :: Double -> IO Double
logic_not' x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__logic_not_TIC x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__logic_and_TIC_TIC" c_CasADi__logic_and_TIC_TIC
  :: Ptr IMatrix' -> Ptr IMatrix' -> IO (Ptr IMatrix')
logic_and''
  :: IMatrix -> IMatrix -> IO IMatrix
logic_and'' x0 x1 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  c_CasADi__logic_and_TIC_TIC x0' x1' >>= wrapReturn

foreign import ccall unsafe "CasADi__logic_or_TIC_TIC" c_CasADi__logic_or_TIC_TIC
  :: Ptr IMatrix' -> Ptr IMatrix' -> IO (Ptr IMatrix')
logic_or''
  :: IMatrix -> IMatrix -> IO IMatrix
logic_or'' x0 x1 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  c_CasADi__logic_or_TIC_TIC x0' x1' >>= wrapReturn

foreign import ccall unsafe "CasADi__logic_not_TIC_TIC" c_CasADi__logic_not_TIC_TIC
  :: Ptr IMatrix' -> IO (Ptr IMatrix')
logic_not''
  :: IMatrix -> IO IMatrix
logic_not'' x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__logic_not_TIC_TIC x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__logic_and_TIC_TIC_TIC" c_CasADi__logic_and_TIC_TIC_TIC
  :: Ptr DMatrix' -> Ptr DMatrix' -> IO (Ptr DMatrix')
logic_and'''
  :: DMatrix -> DMatrix -> IO DMatrix
logic_and''' x0 x1 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  c_CasADi__logic_and_TIC_TIC_TIC x0' x1' >>= wrapReturn

foreign import ccall unsafe "CasADi__logic_or_TIC_TIC_TIC" c_CasADi__logic_or_TIC_TIC_TIC
  :: Ptr DMatrix' -> Ptr DMatrix' -> IO (Ptr DMatrix')
logic_or'''
  :: DMatrix -> DMatrix -> IO DMatrix
logic_or''' x0 x1 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  c_CasADi__logic_or_TIC_TIC_TIC x0' x1' >>= wrapReturn

foreign import ccall unsafe "CasADi__logic_not_TIC_TIC_TIC" c_CasADi__logic_not_TIC_TIC_TIC
  :: Ptr DMatrix' -> IO (Ptr DMatrix')
logic_not'''
  :: DMatrix -> IO DMatrix
logic_not''' x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__logic_not_TIC_TIC_TIC x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__logic_and_TIC_TIC_TIC_TIC" c_CasADi__logic_and_TIC_TIC_TIC_TIC
  :: Ptr SX' -> Ptr SX' -> IO (Ptr SX')
logic_and''''
  :: SX -> SX -> IO SX
logic_and'''' x0 x1 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  c_CasADi__logic_and_TIC_TIC_TIC_TIC x0' x1' >>= wrapReturn

foreign import ccall unsafe "CasADi__logic_or_TIC_TIC_TIC_TIC" c_CasADi__logic_or_TIC_TIC_TIC_TIC
  :: Ptr SX' -> Ptr SX' -> IO (Ptr SX')
logic_or''''
  :: SX -> SX -> IO SX
logic_or'''' x0 x1 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  c_CasADi__logic_or_TIC_TIC_TIC_TIC x0' x1' >>= wrapReturn

foreign import ccall unsafe "CasADi__logic_not_TIC_TIC_TIC_TIC" c_CasADi__logic_not_TIC_TIC_TIC_TIC
  :: Ptr SX' -> IO (Ptr SX')
logic_not''''
  :: SX -> IO SX
logic_not'''' x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__logic_not_TIC_TIC_TIC_TIC x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__logic_and_TIC_TIC_TIC_TIC_TIC" c_CasADi__logic_and_TIC_TIC_TIC_TIC_TIC
  :: Ptr MX' -> Ptr MX' -> IO (Ptr MX')
logic_and'''''
  :: MX -> MX -> IO MX
logic_and''''' x0 x1 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  c_CasADi__logic_and_TIC_TIC_TIC_TIC_TIC x0' x1' >>= wrapReturn

foreign import ccall unsafe "CasADi__logic_or_TIC_TIC_TIC_TIC_TIC" c_CasADi__logic_or_TIC_TIC_TIC_TIC_TIC
  :: Ptr MX' -> Ptr MX' -> IO (Ptr MX')
logic_or'''''
  :: MX -> MX -> IO MX
logic_or''''' x0 x1 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  c_CasADi__logic_or_TIC_TIC_TIC_TIC_TIC x0' x1' >>= wrapReturn

foreign import ccall unsafe "CasADi__logic_not_TIC_TIC_TIC_TIC_TIC" c_CasADi__logic_not_TIC_TIC_TIC_TIC_TIC
  :: Ptr MX' -> IO (Ptr MX')
logic_not'''''
  :: MX -> IO MX
logic_not''''' x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__logic_not_TIC_TIC_TIC_TIC_TIC x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__logic_and_TIC_TIC_TIC_TIC_TIC_TIC" c_CasADi__logic_and_TIC_TIC_TIC_TIC_TIC_TIC
  :: Ptr SXElement' -> Ptr SXElement' -> IO (Ptr SXElement')
logic_and''''''
  :: SXElement -> SXElement -> IO SXElement
logic_and'''''' x0 x1 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  c_CasADi__logic_and_TIC_TIC_TIC_TIC_TIC_TIC x0' x1' >>= wrapReturn

foreign import ccall unsafe "CasADi__logic_or_TIC_TIC_TIC_TIC_TIC_TIC" c_CasADi__logic_or_TIC_TIC_TIC_TIC_TIC_TIC
  :: Ptr SXElement' -> Ptr SXElement' -> IO (Ptr SXElement')
logic_or''''''
  :: SXElement -> SXElement -> IO SXElement
logic_or'''''' x0 x1 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  c_CasADi__logic_or_TIC_TIC_TIC_TIC_TIC_TIC x0' x1' >>= wrapReturn

foreign import ccall unsafe "CasADi__logic_not_TIC_TIC_TIC_TIC_TIC_TIC" c_CasADi__logic_not_TIC_TIC_TIC_TIC_TIC_TIC
  :: Ptr SXElement' -> IO (Ptr SXElement')
logic_not''''''
  :: SXElement -> IO SXElement
logic_not'''''' x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__logic_not_TIC_TIC_TIC_TIC_TIC_TIC x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__reshape_TIC_TIC_TIC_TIC_TIC_TIC" c_CasADi__reshape_TIC_TIC_TIC_TIC_TIC_TIC
  :: Ptr Sparsity' -> CInt -> CInt -> IO (Ptr Sparsity')
reshape''''''
  :: Sparsity -> Int -> Int -> IO Sparsity
reshape'''''' x0 x1 x2 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  withMarshal x2 $ \x2' ->
  c_CasADi__reshape_TIC_TIC_TIC_TIC_TIC_TIC x0' x1' x2' >>= wrapReturn

foreign import ccall unsafe "CasADi__transpose_TIC_TIC_TIC" c_CasADi__transpose_TIC_TIC_TIC
  :: Ptr Sparsity' -> IO (Ptr Sparsity')
transpose'''
  :: Sparsity -> IO Sparsity
transpose''' x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__transpose_TIC_TIC_TIC x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__vec_TIC_TIC_TIC" c_CasADi__vec_TIC_TIC_TIC
  :: Ptr Sparsity' -> IO (Ptr Sparsity')
vec'''
  :: Sparsity -> IO Sparsity
vec''' x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__vec_TIC_TIC_TIC x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__mul_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC" c_CasADi__mul_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC
  :: Ptr Sparsity' -> Ptr Sparsity' -> IO (Ptr Sparsity')
mul'''''''''
  :: Sparsity -> Sparsity -> IO Sparsity
mul''''''''' x0 x1 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  c_CasADi__mul_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC x0' x1' >>= wrapReturn

foreign import ccall unsafe "CasADi__vertcat_TIC_TIC_TIC" c_CasADi__vertcat_TIC_TIC_TIC
  :: Ptr (CppVec (Ptr Sparsity')) -> IO (Ptr Sparsity')
vertcat'''
  :: Vector Sparsity -> IO Sparsity
vertcat''' x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__vertcat_TIC_TIC_TIC x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__horzcat_TIC_TIC_TIC" c_CasADi__horzcat_TIC_TIC_TIC
  :: Ptr (CppVec (Ptr Sparsity')) -> IO (Ptr Sparsity')
horzcat'''
  :: Vector Sparsity -> IO Sparsity
horzcat''' x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__horzcat_TIC_TIC_TIC x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__blkdiag_TIC_TIC_TIC" c_CasADi__blkdiag_TIC_TIC_TIC
  :: Ptr (CppVec (Ptr Sparsity')) -> IO (Ptr Sparsity')
blkdiag'''
  :: Vector Sparsity -> IO Sparsity
blkdiag''' x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__blkdiag_TIC_TIC_TIC x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__horzsplit_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC" c_CasADi__horzsplit_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC
  :: Ptr Sparsity' -> Ptr (CppVec CInt) -> IO (Ptr (CppVec (Ptr Sparsity')))
horzsplit'''''''''
  :: Sparsity -> Vector Int -> IO (Vector Sparsity)
horzsplit''''''''' x0 x1 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  c_CasADi__horzsplit_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC x0' x1' >>= wrapReturn

foreign import ccall unsafe "CasADi__vertsplit_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC" c_CasADi__vertsplit_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC
  :: Ptr Sparsity' -> Ptr (CppVec CInt) -> IO (Ptr (CppVec (Ptr Sparsity')))
vertsplit'''''''''
  :: Sparsity -> Vector Int -> IO (Vector Sparsity)
vertsplit''''''''' x0 x1 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  c_CasADi__vertsplit_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC x0' x1' >>= wrapReturn

foreign import ccall unsafe "CasADi__rank" c_CasADi__rank
  :: Ptr Sparsity' -> IO CInt
{-|
>Obtain the structural rank of a sparsity-pattern.
-}
rank
  :: Sparsity -> IO Int
rank x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__rank x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__triu_TIC_TIC_TIC_TIC" c_CasADi__triu_TIC_TIC_TIC_TIC
  :: Ptr Sparsity' -> CInt -> IO (Ptr Sparsity')
triu''''
  :: Sparsity -> Bool -> IO Sparsity
triu'''' x0 x1 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  c_CasADi__triu_TIC_TIC_TIC_TIC x0' x1' >>= wrapReturn

foreign import ccall unsafe "CasADi__triu_TIC_TIC_TIC_TIC_TIC" c_CasADi__triu_TIC_TIC_TIC_TIC_TIC
  :: Ptr Sparsity' -> IO (Ptr Sparsity')
triu'''''
  :: Sparsity -> IO Sparsity
triu''''' x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__triu_TIC_TIC_TIC_TIC_TIC x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__tril_TIC_TIC_TIC_TIC" c_CasADi__tril_TIC_TIC_TIC_TIC
  :: Ptr Sparsity' -> CInt -> IO (Ptr Sparsity')
tril''''
  :: Sparsity -> Bool -> IO Sparsity
tril'''' x0 x1 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  c_CasADi__tril_TIC_TIC_TIC_TIC x0' x1' >>= wrapReturn

foreign import ccall unsafe "CasADi__tril_TIC_TIC_TIC_TIC_TIC" c_CasADi__tril_TIC_TIC_TIC_TIC_TIC
  :: Ptr Sparsity' -> IO (Ptr Sparsity')
tril'''''
  :: Sparsity -> IO Sparsity
tril''''' x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__tril_TIC_TIC_TIC_TIC_TIC x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__expand" c_CasADi__expand
  :: Ptr SX' -> Ptr SX' -> Ptr SX' -> IO ()
{-|
>Expand the expression as a weighted sum (with constant weights)
-}
expand
  :: SX -> SX -> SX -> IO ()
expand x0 x1 x2 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  withMarshal x2 $ \x2' ->
  c_CasADi__expand x0' x1' x2' >>= wrapReturn

foreign import ccall unsafe "CasADi__simplify" c_CasADi__simplify
  :: Ptr SXElement' -> IO ()
{-|
>>  void CasADi::simplify(MX &ex)
>
>>  void CasADi::simplify(SX &ex)
>------------------------------------------------------------------------
>
>Simplify an expression.
>
>>  void CasADi::simplify(SXElement &ex)
>------------------------------------------------------------------------
>
>Simplify the expression: formulates the expression as and eliminates terms.
-}
simplify
  :: SXElement -> IO ()
simplify x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__simplify x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__pw_const" c_CasADi__pw_const
  :: Ptr SX' -> Ptr SX' -> Ptr SX' -> IO (Ptr SX')
{-|
>Create a piecewise constant function Create a piecewise constant function
>with n=val.size() intervals.
>
>Inputs:
>
>Parameters:
>-----------
>
>t:  a scalar variable (e.g. time)
>
>tval:  vector with the discrete values of t at the interval transitions
>(length n-1)
>
>val:  vector with the value of the function for each interval (length n)
-}
pw_const
  :: SX -> SX -> SX -> IO SX
pw_const x0 x1 x2 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  withMarshal x2 $ \x2' ->
  c_CasADi__pw_const x0' x1' x2' >>= wrapReturn

foreign import ccall unsafe "CasADi__pw_lin" c_CasADi__pw_lin
  :: Ptr SXElement' -> Ptr SX' -> Ptr SX' -> IO (Ptr SX')
{-|
>t a scalar variable (e.g. time)
>
>Create a piecewise linear function Create a piecewise linear function:
>
>Inputs: tval vector with the the discrete values of t (monotonically
>increasing) val vector with the corresponding function values (same length
>as tval)
-}
pw_lin
  :: SXElement -> SX -> SX -> IO SX
pw_lin x0 x1 x2 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  withMarshal x2 $ \x2' ->
  c_CasADi__pw_lin x0' x1' x2' >>= wrapReturn

foreign import ccall unsafe "CasADi__if_else" c_CasADi__if_else
  :: Ptr SX' -> Ptr SX' -> Ptr SX' -> IO (Ptr SX')
{-|
>>  MX CasADi::if_else(const MX &cond, const MX &if_true, const MX &if_false)
>------------------------------------------------------------------------
>
>Branching on MX nodes Ternary operator, "cond ? if_true : if_false".
>
>>  SX CasADi::if_else(const SX &cond, const SX &if_true, const SX &if_false)
>------------------------------------------------------------------------
>
>Integrate f from a to b using Gaussian quadrature with n points.
>
>>  T CasADi::if_else(const SXElement &cond, const T &if_true, const T &if_false)
>------------------------------------------------------------------------
>[INTERNAL] 
>Expand the expression as a weighted sum (with constant weights)
-}
if_else
  :: SX -> SX -> SX -> IO SX
if_else x0 x1 x2 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  withMarshal x2 $ \x2' ->
  c_CasADi__if_else x0' x1' x2' >>= wrapReturn

foreign import ccall unsafe "CasADi__heaviside" c_CasADi__heaviside
  :: Ptr SX' -> IO (Ptr SX')
{-|
>Heaviside function.
>
>\\[ \\begin{cases} H(x) = 0 & x<0 \\\\ H(x) = 1/2 & x=0 \\\\
>H(x) = 1 & x>0 \\\\ \\end{cases} \\]
-}
heaviside
  :: SX -> IO SX
heaviside x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__heaviside x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__rectangle" c_CasADi__rectangle
  :: Ptr SX' -> IO (Ptr SX')
{-|
>rectangle function
>
>\\[ \\begin{cases} \\Pi(x) = 1 & |x| < 1/2 \\\\ \\Pi(x) = 1/2 &
>|x| = 1/2 \\\\ \\Pi(x) = 0 & |x| > 1/2 \\\\ \\end{cases} \\]
>
>Also called: gate function, block function, band function, pulse function,
>window function
-}
rectangle
  :: SX -> IO SX
rectangle x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__rectangle x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__triangle" c_CasADi__triangle
  :: Ptr SX' -> IO (Ptr SX')
{-|
>triangle function
>
>\\[ \\begin{cases} \\Lambda(x) = 0 & |x| >= 1 \\\\ \\Lambda(x) =
>1-|x| & |x| < 1 \\end{cases} \\]
-}
triangle
  :: SX -> IO SX
triangle x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__triangle x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__ramp" c_CasADi__ramp
  :: Ptr SX' -> IO (Ptr SX')
{-|
>ramp function
>
>\\[ \\begin{cases} R(x) = 0 & x <= 1 \\\\ R(x) = x & x > 1 \\\\
>\\end{cases} \\]
>
>Also called: slope function
-}
ramp
  :: SX -> IO SX
ramp x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__ramp x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__gauss_quadrature" c_CasADi__gauss_quadrature
  :: Ptr SX' -> Ptr SX' -> Ptr SX' -> Ptr SX' -> CInt -> Ptr SX' -> IO (Ptr SX')
{-|
>Integrate f from a to b using Gaussian quadrature with n points.
-}
gauss_quadrature
  :: SX -> SX -> SX -> SX -> Int -> SX -> IO SX
gauss_quadrature x0 x1 x2 x3 x4 x5 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  withMarshal x2 $ \x2' ->
  withMarshal x3 $ \x3' ->
  withMarshal x4 $ \x4' ->
  withMarshal x5 $ \x5' ->
  c_CasADi__gauss_quadrature x0' x1' x2' x3' x4' x5' >>= wrapReturn

foreign import ccall unsafe "CasADi__gauss_quadrature_TIC" c_CasADi__gauss_quadrature_TIC
  :: Ptr SX' -> Ptr SX' -> Ptr SX' -> Ptr SX' -> CInt -> IO (Ptr SX')
gauss_quadrature'
  :: SX -> SX -> SX -> SX -> Int -> IO SX
gauss_quadrature' x0 x1 x2 x3 x4 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  withMarshal x2 $ \x2' ->
  withMarshal x3 $ \x3' ->
  withMarshal x4 $ \x4' ->
  c_CasADi__gauss_quadrature_TIC x0' x1' x2' x3' x4' >>= wrapReturn

foreign import ccall unsafe "CasADi__gauss_quadrature_TIC_TIC" c_CasADi__gauss_quadrature_TIC_TIC
  :: Ptr SX' -> Ptr SX' -> Ptr SX' -> Ptr SX' -> IO (Ptr SX')
gauss_quadrature''
  :: SX -> SX -> SX -> SX -> IO SX
gauss_quadrature'' x0 x1 x2 x3 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  withMarshal x2 $ \x2' ->
  withMarshal x3 $ \x3' ->
  c_CasADi__gauss_quadrature_TIC_TIC x0' x1' x2' x3' >>= wrapReturn

foreign import ccall unsafe "CasADi__simplify_TIC" c_CasADi__simplify_TIC
  :: Ptr SX' -> IO ()
simplify'
  :: SX -> IO ()
simplify' x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__simplify_TIC x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__compress" c_CasADi__compress
  :: Ptr SX' -> CInt -> IO ()
{-|
>Remove identical calculations.
-}
compress
  :: SX -> Int -> IO ()
compress x0 x1 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  c_CasADi__compress x0' x1' >>= wrapReturn

foreign import ccall unsafe "CasADi__compress_TIC" c_CasADi__compress_TIC
  :: Ptr SX' -> IO ()
compress'
  :: SX -> IO ()
compress' x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__compress_TIC x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__substitute" c_CasADi__substitute
  :: Ptr SX' -> Ptr SX' -> Ptr SX' -> IO (Ptr SX')
{-|
>>  MX CasADi::substitute(const MX &ex, const MX &v, const MX &vdef)
>
>>  SX CasADi::substitute(const SX &ex, const SX &v, const SX &vdef)
>------------------------------------------------------------------------
>
>Substitute variable v with expression vdef in an expression ex.
>
>>  std::vector< MX > CasADi::substitute(const std::vector< MX > &ex, const std::vector< MX > &v, const std::vector< MX > &vdef)
>
>>  std::vector< SX > CasADi::substitute(const std::vector< SX > &ex, const std::vector< SX > &v, const std::vector< SX > &vdef)
>------------------------------------------------------------------------
>
>Substitute variable var with expression expr in multiple expressions.
-}
substitute
  :: SX -> SX -> SX -> IO SX
substitute x0 x1 x2 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  withMarshal x2 $ \x2' ->
  c_CasADi__substitute x0' x1' x2' >>= wrapReturn

foreign import ccall unsafe "CasADi__substitute_TIC" c_CasADi__substitute_TIC
  :: Ptr (CppVec (Ptr SX')) -> Ptr (CppVec (Ptr SX')) -> Ptr (CppVec (Ptr SX')) -> IO (Ptr (CppVec (Ptr SX')))
substitute'
  :: Vector SX -> Vector SX -> Vector SX -> IO (Vector SX)
substitute' x0 x1 x2 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  withMarshal x2 $ \x2' ->
  c_CasADi__substitute_TIC x0' x1' x2' >>= wrapReturn

foreign import ccall unsafe "CasADi__substituteInPlace" c_CasADi__substituteInPlace
  :: Ptr SX' -> Ptr SX' -> CInt -> IO ()
{-|
>>  void CasADi::substituteInPlace(const std::vector< MX > &v, std::vector< MX > &vdef, bool reverse=false)
>------------------------------------------------------------------------
>[INTERNAL] 
>Inplace substitution Substitute variables v out of the expressions
>vdef sequentially.
>
>>  void CasADi::substituteInPlace(const std::vector< MX > &v, std::vector< MX > &vdef, std::vector< MX > &ex, bool reverse=false)
>------------------------------------------------------------------------
>[INTERNAL] 
>Inplace substitution with piggyback expressions Substitute variables v
>out of the expressions vdef sequentially, as well as out of a number
>of other expressions piggyback.
>
>>  void CasADi::substituteInPlace(const SX &v, SX &vdef, bool reverse=false)
>------------------------------------------------------------------------
>
>Substitute variable var out of or into an expression expr.
>
>>  void CasADi::substituteInPlace(const SX &v, SX &vdef, std::vector< SX > &ex, bool reverse=false)
>------------------------------------------------------------------------
>
>Substitute variable var out of or into an expression expr, with an arbitrary
>number of other expressions piggyback.
>
>>  void CasADi::substituteInPlace(const std::vector< SX > &v, std::vector< SX > &vdef, std::vector< SX > &ex, bool reverse=false)
>------------------------------------------------------------------------
>
>Substitute variable var out of or into an expression expr, with an arbitrary
>number of other expressions piggyback (vector version)
-}
substituteInPlace
  :: SX -> SX -> Bool -> IO ()
substituteInPlace x0 x1 x2 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  withMarshal x2 $ \x2' ->
  c_CasADi__substituteInPlace x0' x1' x2' >>= wrapReturn

foreign import ccall unsafe "CasADi__substituteInPlace_TIC" c_CasADi__substituteInPlace_TIC
  :: Ptr SX' -> Ptr SX' -> IO ()
substituteInPlace'
  :: SX -> SX -> IO ()
substituteInPlace' x0 x1 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  c_CasADi__substituteInPlace_TIC x0' x1' >>= wrapReturn

foreign import ccall unsafe "CasADi__substituteInPlace_TIC_TIC" c_CasADi__substituteInPlace_TIC_TIC
  :: Ptr SX' -> Ptr SX' -> Ptr (CppVec (Ptr SX')) -> CInt -> IO ()
substituteInPlace''
  :: SX -> SX -> Vector SX -> Bool -> IO ()
substituteInPlace'' x0 x1 x2 x3 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  withMarshal x2 $ \x2' ->
  withMarshal x3 $ \x3' ->
  c_CasADi__substituteInPlace_TIC_TIC x0' x1' x2' x3' >>= wrapReturn

foreign import ccall unsafe "CasADi__substituteInPlace_TIC_TIC_TIC" c_CasADi__substituteInPlace_TIC_TIC_TIC
  :: Ptr SX' -> Ptr SX' -> Ptr (CppVec (Ptr SX')) -> IO ()
substituteInPlace'''
  :: SX -> SX -> Vector SX -> IO ()
substituteInPlace''' x0 x1 x2 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  withMarshal x2 $ \x2' ->
  c_CasADi__substituteInPlace_TIC_TIC_TIC x0' x1' x2' >>= wrapReturn

foreign import ccall unsafe "CasADi__substituteInPlace_TIC_TIC_TIC_TIC" c_CasADi__substituteInPlace_TIC_TIC_TIC_TIC
  :: Ptr (CppVec (Ptr SX')) -> Ptr (CppVec (Ptr SX')) -> Ptr (CppVec (Ptr SX')) -> CInt -> IO ()
substituteInPlace''''
  :: Vector SX -> Vector SX -> Vector SX -> Bool -> IO ()
substituteInPlace'''' x0 x1 x2 x3 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  withMarshal x2 $ \x2' ->
  withMarshal x3 $ \x3' ->
  c_CasADi__substituteInPlace_TIC_TIC_TIC_TIC x0' x1' x2' x3' >>= wrapReturn

foreign import ccall unsafe "CasADi__substituteInPlace_TIC_TIC_TIC_TIC_TIC" c_CasADi__substituteInPlace_TIC_TIC_TIC_TIC_TIC
  :: Ptr (CppVec (Ptr SX')) -> Ptr (CppVec (Ptr SX')) -> Ptr (CppVec (Ptr SX')) -> IO ()
substituteInPlace'''''
  :: Vector SX -> Vector SX -> Vector SX -> IO ()
substituteInPlace''''' x0 x1 x2 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  withMarshal x2 $ \x2' ->
  c_CasADi__substituteInPlace_TIC_TIC_TIC_TIC_TIC x0' x1' x2' >>= wrapReturn

foreign import ccall unsafe "CasADi__evalf" c_CasADi__evalf
  :: Ptr SX' -> IO (Ptr DMatrix')
{-|
>>  Matrix< double > CasADi::evalf(const SX &ex, const SX &v, const Matrix< double > &vdef)
>------------------------------------------------------------------------
>
>Substitute variable v with value vdef in an expression ex, and evaluate
>numerically Note: this is not efficient. For critical parts (loops) of your
>code, always use SXFunction.
>
>>  Matrix< double > CasADi::evalf(const SX &ex)
>------------------------------------------------------------------------
>
>Evaluate an SX graph numerically Note: this is not efficient. For critical
>parts (loops) of your code, always use SXFunction.
-}
evalf
  :: SX -> IO DMatrix
evalf x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__evalf x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__evalf_TIC" c_CasADi__evalf_TIC
  :: Ptr SX' -> Ptr SX' -> Ptr DMatrix' -> IO (Ptr DMatrix')
evalf'
  :: SX -> SX -> DMatrix -> IO DMatrix
evalf' x0 x1 x2 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  withMarshal x2 $ \x2' ->
  c_CasADi__evalf_TIC x0' x1' x2' >>= wrapReturn

foreign import ccall unsafe "CasADi__spy" c_CasADi__spy
  :: Ptr SX' -> IO (Ptr SX')
{-|
>Get the sparsity pattern of a matrix.
-}
spy
  :: SX -> IO SX
spy x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__spy x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__dependsOn" c_CasADi__dependsOn
  :: Ptr SX' -> Ptr SX' -> IO CInt
{-|
>>  bool CasADi::dependsOn(const SX &f, const SX &arg)
>------------------------------------------------------------------------
>
>Check if expression depends on the argument The argument must be symbolic.
>
>>  bool CasADi::dependsOn(const MX &ex, const std::vector< MX > &arg)
>------------------------------------------------------------------------
>
>Check if expression depends on any of the arguments The arguments must be
>symbolic.
-}
dependsOn
  :: SX -> SX -> IO Bool
dependsOn x0 x1 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  c_CasADi__dependsOn x0' x1' >>= wrapReturn

foreign import ccall unsafe "CasADi__getSymbols" c_CasADi__getSymbols
  :: Ptr SX' -> IO (Ptr (CppVec (Ptr SXElement')))
{-|
>>  std::vector< SXElement > CasADi::getSymbols(const SX &e)
>------------------------------------------------------------------------
>
>Get all symbols contained in the supplied expression Get all symbols on
>which the supplied expression depends.
>
>See:   SXFunction::getFree()
>
>>  std::vector< MX > CasADi::getSymbols(const MX &e)
>
>>  std::vector< MX > CasADi::getSymbols(const std::vector< MX > &e)
>------------------------------------------------------------------------
>
>Get all symbols contained in the supplied expression Get all symbols on
>which the supplied expression depends.
>
>See:   MXFunction::getFree()
-}
getSymbols
  :: SX -> IO (Vector SXElement)
getSymbols x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__getSymbols x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__jacobian" c_CasADi__jacobian
  :: Ptr SX' -> Ptr SX' -> IO (Ptr SX')
{-|
>>  MX CasADi::jacobian(const MX &ex, const MX &arg)
>------------------------------------------------------------------------
>
>Calculate jacobian via source code transformation.
>
>Uses CasADi::MXFunction::jac
>
>>  SX CasADi::jacobian(const SX &ex, const SX &arg)
>------------------------------------------------------------------------
>
>Calculate jacobian via source code transformation.
>
>Uses CasADi::SXFunction::jac
-}
jacobian
  :: SX -> SX -> IO SX
jacobian x0 x1 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  c_CasADi__jacobian x0' x1' >>= wrapReturn

foreign import ccall unsafe "CasADi__gradient" c_CasADi__gradient
  :: Ptr SX' -> Ptr SX' -> IO (Ptr SX')
{-|
>>  MX CasADi::gradient(const MX &ex, const MX &arg)
>------------------------------------------------------------------------
>
>split vertically, retaining groups of cols
>
>Parameters:
>-----------
>
>output_offset:  List of all start cols for each group the last col group
>will run to the end.
>
>horzcat(horzsplit(x,...)) = x
>
>>  SX CasADi::gradient(const SX &ex, const SX &arg)
>------------------------------------------------------------------------
>
>Integrate f from a to b using Gaussian quadrature with n points.
-}
gradient
  :: SX -> SX -> IO SX
gradient x0 x1 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  c_CasADi__gradient x0' x1' >>= wrapReturn

foreign import ccall unsafe "CasADi__tangent" c_CasADi__tangent
  :: Ptr SX' -> Ptr SX' -> IO (Ptr SX')
{-|
>>  MX CasADi::tangent(const MX &ex, const MX &arg)
>------------------------------------------------------------------------
>
>split vertically, retaining groups of cols
>
>Parameters:
>-----------
>
>output_offset:  List of all start cols for each group the last col group
>will run to the end.
>
>horzcat(horzsplit(x,...)) = x
>
>>  SX CasADi::tangent(const SX &ex, const SX &arg)
>------------------------------------------------------------------------
>
>Integrate f from a to b using Gaussian quadrature with n points.
-}
tangent
  :: SX -> SX -> IO SX
tangent x0 x1 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  c_CasADi__tangent x0' x1' >>= wrapReturn

foreign import ccall unsafe "CasADi__hessian" c_CasADi__hessian
  :: Ptr SX' -> Ptr SX' -> IO (Ptr SX')
{-|
>Integrate f from a to b using Gaussian quadrature with n points.
-}
hessian
  :: SX -> SX -> IO SX
hessian x0 x1 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  c_CasADi__hessian x0' x1' >>= wrapReturn

foreign import ccall unsafe "CasADi__hessian_TIC" c_CasADi__hessian_TIC
  :: Ptr SX' -> Ptr SX' -> Ptr SX' -> Ptr SX' -> IO ()
hessian'
  :: SX -> SX -> SX -> SX -> IO ()
hessian' x0 x1 x2 x3 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  withMarshal x2 $ \x2' ->
  withMarshal x3 $ \x3' ->
  c_CasADi__hessian_TIC x0' x1' x2' x3' >>= wrapReturn

foreign import ccall unsafe "CasADi__jacobianTimesVector" c_CasADi__jacobianTimesVector
  :: Ptr SX' -> Ptr SX' -> Ptr SX' -> CInt -> IO (Ptr SX')
{-|
>Calculate the Jacobian and multiply by a vector from the left This is
>equivalent to mul(jacobian(ex,arg),v) or mul(jacobian(ex,arg).T,v) for
>transpose_jacobian set to false and true respectively. If contrast to these
>expressions, it will use directional derivatives which is typically (but not
>necessarily) more efficient if the complete Jacobian is not needed and v has
>few rows.
-}
jacobianTimesVector
  :: SX -> SX -> SX -> Bool -> IO SX
jacobianTimesVector x0 x1 x2 x3 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  withMarshal x2 $ \x2' ->
  withMarshal x3 $ \x3' ->
  c_CasADi__jacobianTimesVector x0' x1' x2' x3' >>= wrapReturn

foreign import ccall unsafe "CasADi__jacobianTimesVector_TIC" c_CasADi__jacobianTimesVector_TIC
  :: Ptr SX' -> Ptr SX' -> Ptr SX' -> IO (Ptr SX')
jacobianTimesVector'
  :: SX -> SX -> SX -> IO SX
jacobianTimesVector' x0 x1 x2 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  withMarshal x2 $ \x2' ->
  c_CasADi__jacobianTimesVector_TIC x0' x1' x2' >>= wrapReturn

foreign import ccall unsafe "CasADi__taylor" c_CasADi__taylor
  :: Ptr SX' -> Ptr SX' -> Ptr SX' -> CInt -> IO (Ptr SX')
{-|
>univariate taylor series expansion
>
>Calculate the taylor expansion of expression 'ex' up to order 'order' with
>repsect to variable 'x' around the point 'a'
>
>$(x)=f(a)+f'(a)(x-a)+f''(a)\\frac{(x-a)^2}{2!}+f'''(a)\\frac{(x-a)^3}{3!}+\\ldots$
>
>Example usage:>>   x
-}
taylor
  :: SX -> SX -> SX -> Int -> IO SX
taylor x0 x1 x2 x3 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  withMarshal x2 $ \x2' ->
  withMarshal x3 $ \x3' ->
  c_CasADi__taylor x0' x1' x2' x3' >>= wrapReturn

foreign import ccall unsafe "CasADi__taylor_TIC" c_CasADi__taylor_TIC
  :: Ptr SX' -> Ptr SX' -> Ptr SX' -> IO (Ptr SX')
taylor'
  :: SX -> SX -> SX -> IO SX
taylor' x0 x1 x2 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  withMarshal x2 $ \x2' ->
  c_CasADi__taylor_TIC x0' x1' x2' >>= wrapReturn

foreign import ccall unsafe "CasADi__taylor_TIC_TIC" c_CasADi__taylor_TIC_TIC
  :: Ptr SX' -> Ptr SX' -> IO (Ptr SX')
taylor''
  :: SX -> SX -> IO SX
taylor'' x0 x1 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  c_CasADi__taylor_TIC_TIC x0' x1' >>= wrapReturn

foreign import ccall unsafe "CasADi__mtaylor" c_CasADi__mtaylor
  :: Ptr SX' -> Ptr SX' -> Ptr SX' -> CInt -> IO (Ptr SX')
{-|
>>  SX CasADi::mtaylor(const SX &ex, const SX &x, const SX &a, int order=1)
>------------------------------------------------------------------------
>
>multivariate taylor series expansion
>
>Do taylor expansions until the aggregated order of a term is equal to
>'order'. The aggregated order of $x^n y^m$ equals $n+m$.
>
>>  SX CasADi::mtaylor(const SX &ex, const SX &x, const SX &a, int order, const std::vector< int > &order_contributions)
>------------------------------------------------------------------------
>
>multivariate taylor series expansion
>
>Do taylor expansions until the aggregated order of a term is equal to
>'order'. The aggregated order of $x^n y^m$ equals $n+m$.
>
>The argument order_contributions can denote how match each variable
>contributes to the aggregated order. If x=[x,y] and
>order_contributions=[1,2], then the aggregated order of $x^n y^m$ equals
>$1n+2m$.
>
>Example usage
>
>$ \\sin(b+a)+\\cos(b+a)(x-a)+\\cos(b+a)(y-b) $ $ y+x-(x^3+3y x^2+3 y^2
>x+y^3)/6 $ $ (-3 x^2 y-x^3)/6+y+x $
-}
mtaylor
  :: SX -> SX -> SX -> Int -> IO SX
mtaylor x0 x1 x2 x3 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  withMarshal x2 $ \x2' ->
  withMarshal x3 $ \x3' ->
  c_CasADi__mtaylor x0' x1' x2' x3' >>= wrapReturn

foreign import ccall unsafe "CasADi__mtaylor_TIC" c_CasADi__mtaylor_TIC
  :: Ptr SX' -> Ptr SX' -> Ptr SX' -> IO (Ptr SX')
mtaylor'
  :: SX -> SX -> SX -> IO SX
mtaylor' x0 x1 x2 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  withMarshal x2 $ \x2' ->
  c_CasADi__mtaylor_TIC x0' x1' x2' >>= wrapReturn

foreign import ccall unsafe "CasADi__mtaylor_TIC_TIC" c_CasADi__mtaylor_TIC_TIC
  :: Ptr SX' -> Ptr SX' -> Ptr SX' -> CInt -> Ptr (CppVec CInt) -> IO (Ptr SX')
mtaylor''
  :: SX -> SX -> SX -> Int -> Vector Int -> IO SX
mtaylor'' x0 x1 x2 x3 x4 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  withMarshal x2 $ \x2' ->
  withMarshal x3 $ \x3' ->
  withMarshal x4 $ \x4' ->
  c_CasADi__mtaylor_TIC_TIC x0' x1' x2' x3' x4' >>= wrapReturn

foreign import ccall unsafe "CasADi__countNodes" c_CasADi__countNodes
  :: Ptr SX' -> IO CInt
{-|
>>  int CasADi::countNodes(const MX &A)
>------------------------------------------------------------------------
>
>Count number of nodes
>
>>  int CasADi::countNodes(const SX &A)
>------------------------------------------------------------------------
>
>Count number of nodes.
-}
countNodes
  :: SX -> IO Int
countNodes x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__countNodes x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__getOperatorRepresentation" c_CasADi__getOperatorRepresentation
  :: Ptr SXElement' -> Ptr (CppVec (Ptr StdString')) -> IO (Ptr StdString')
{-|
>>  std::string CasADi::getOperatorRepresentation(const MX &x, const std::vector< std::string > &args)
>------------------------------------------------------------------------
>
>Get a string representation for a binary MX, using custom arguments.
>
>>  std::string CasADi::getOperatorRepresentation(const SXElement &x, const std::vector< std::string > &args)
>------------------------------------------------------------------------
>
>Get a string representation for a binary SX, using custom arguments.
-}
getOperatorRepresentation
  :: SXElement -> Vector String -> IO String
getOperatorRepresentation x0 x1 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  c_CasADi__getOperatorRepresentation x0' x1' >>= wrapReturn

foreign import ccall unsafe "CasADi__getFree" c_CasADi__getFree
  :: Ptr SX' -> IO (Ptr SX')
{-|
>Get all the free variables in an expression.
-}
getFree
  :: SX -> IO SX
getFree x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__getFree x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__extractShared" c_CasADi__extractShared
  :: Ptr (CppVec (Ptr SXElement')) -> Ptr (CppVec (Ptr SXElement')) -> Ptr (CppVec (Ptr SXElement')) -> Ptr StdString' -> Ptr StdString' -> IO ()
{-|
>Extract shared subexpressions from an set of expressions.
-}
extractShared
  :: Vector SXElement -> Vector SXElement -> Vector SXElement -> String -> String -> IO ()
extractShared x0 x1 x2 x3 x4 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  withMarshal x2 $ \x2' ->
  withMarshal x3 $ \x3' ->
  withMarshal x4 $ \x4' ->
  c_CasADi__extractShared x0' x1' x2' x3' x4' >>= wrapReturn

foreign import ccall unsafe "CasADi__extractShared_TIC" c_CasADi__extractShared_TIC
  :: Ptr (CppVec (Ptr SXElement')) -> Ptr (CppVec (Ptr SXElement')) -> Ptr (CppVec (Ptr SXElement')) -> Ptr StdString' -> IO ()
extractShared'
  :: Vector SXElement -> Vector SXElement -> Vector SXElement -> String -> IO ()
extractShared' x0 x1 x2 x3 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  withMarshal x2 $ \x2' ->
  withMarshal x3 $ \x3' ->
  c_CasADi__extractShared_TIC x0' x1' x2' x3' >>= wrapReturn

foreign import ccall unsafe "CasADi__extractShared_TIC_TIC" c_CasADi__extractShared_TIC_TIC
  :: Ptr (CppVec (Ptr SXElement')) -> Ptr (CppVec (Ptr SXElement')) -> Ptr (CppVec (Ptr SXElement')) -> IO ()
extractShared''
  :: Vector SXElement -> Vector SXElement -> Vector SXElement -> IO ()
extractShared'' x0 x1 x2 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  withMarshal x2 $ \x2' ->
  c_CasADi__extractShared_TIC_TIC x0' x1' x2' >>= wrapReturn

foreign import ccall unsafe "CasADi__printCompact_TIC" c_CasADi__printCompact_TIC
  :: Ptr SX' -> IO ()
printCompact'
  :: SX -> IO ()
printCompact' x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__printCompact_TIC x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__poly_coeff" c_CasADi__poly_coeff
  :: Ptr SX' -> Ptr SX' -> IO (Ptr SX')
{-|
>extracts polynomial coefficients from an expression
>
>ex Scalar expression that represents a polynomial  x Scalar symbol that th
>epolynomial is build up with
-}
poly_coeff
  :: SX -> SX -> IO SX
poly_coeff x0 x1 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  c_CasADi__poly_coeff x0' x1' >>= wrapReturn

foreign import ccall unsafe "CasADi__poly_roots" c_CasADi__poly_roots
  :: Ptr SX' -> IO (Ptr SX')
{-|
>Attempts to find the roots of a polynomial.
>
>This will only work for polynomials up to order 3 It is assumed that the
>roots are real.
-}
poly_roots
  :: SX -> IO SX
poly_roots x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__poly_roots x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__eig_symbolic" c_CasADi__eig_symbolic
  :: Ptr SX' -> IO (Ptr SX')
{-|
>Attempts to find the eigenvalues of a symbolic matrix This will only work
>for up to 3x3 matrices.
>
>Bring m in block diagonal form, calculating eigenvalues of each block
>seperately 
-}
eig_symbolic
  :: SX -> IO SX
eig_symbolic x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__eig_symbolic x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__horzcat_TIC_TIC_TIC_TIC" c_CasADi__horzcat_TIC_TIC_TIC_TIC
  :: Ptr (CppVec (Ptr MX')) -> IO (Ptr MX')
horzcat''''
  :: Vector MX -> IO MX
horzcat'''' x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__horzcat_TIC_TIC_TIC_TIC x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__horzsplit_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC" c_CasADi__horzsplit_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC
  :: Ptr MX' -> Ptr (CppVec CInt) -> IO (Ptr (CppVec (Ptr MX')))
horzsplit''''''''''
  :: MX -> Vector Int -> IO (Vector MX)
horzsplit'''''''''' x0 x1 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  c_CasADi__horzsplit_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC x0' x1' >>= wrapReturn

foreign import ccall unsafe "CasADi__horzsplit_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC" c_CasADi__horzsplit_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC
  :: Ptr MX' -> CInt -> IO (Ptr (CppVec (Ptr MX')))
horzsplit'''''''''''
  :: MX -> Int -> IO (Vector MX)
horzsplit''''''''''' x0 x1 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  c_CasADi__horzsplit_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC x0' x1' >>= wrapReturn

foreign import ccall unsafe "CasADi__horzsplit_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC" c_CasADi__horzsplit_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC
  :: Ptr MX' -> IO (Ptr (CppVec (Ptr MX')))
horzsplit''''''''''''
  :: MX -> IO (Vector MX)
horzsplit'''''''''''' x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__horzsplit_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__vertcat_TIC_TIC_TIC_TIC" c_CasADi__vertcat_TIC_TIC_TIC_TIC
  :: Ptr (CppVec (Ptr MX')) -> IO (Ptr MX')
vertcat''''
  :: Vector MX -> IO MX
vertcat'''' x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__vertcat_TIC_TIC_TIC_TIC x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__vertsplit_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC" c_CasADi__vertsplit_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC
  :: Ptr MX' -> Ptr (CppVec CInt) -> IO (Ptr (CppVec (Ptr MX')))
vertsplit''''''''''
  :: MX -> Vector Int -> IO (Vector MX)
vertsplit'''''''''' x0 x1 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  c_CasADi__vertsplit_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC x0' x1' >>= wrapReturn

foreign import ccall unsafe "CasADi__vertsplit_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC" c_CasADi__vertsplit_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC
  :: Ptr MX' -> CInt -> IO (Ptr (CppVec (Ptr MX')))
vertsplit'''''''''''
  :: MX -> Int -> IO (Vector MX)
vertsplit''''''''''' x0 x1 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  c_CasADi__vertsplit_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC x0' x1' >>= wrapReturn

foreign import ccall unsafe "CasADi__vertsplit_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC" c_CasADi__vertsplit_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC
  :: Ptr MX' -> IO (Ptr (CppVec (Ptr MX')))
vertsplit''''''''''''
  :: MX -> IO (Vector MX)
vertsplit'''''''''''' x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__vertsplit_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__blockcat_TIC_TIC_TIC" c_CasADi__blockcat_TIC_TIC_TIC
  :: Ptr (CppVec (Ptr (CppVec (Ptr MX')))) -> IO (Ptr MX')
blockcat'''
  :: Vector (Vector MX) -> IO MX
blockcat''' x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__blockcat_TIC_TIC_TIC x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__blocksplit_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC" c_CasADi__blocksplit_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC
  :: Ptr MX' -> Ptr (CppVec CInt) -> Ptr (CppVec CInt) -> IO (Ptr (CppVec (Ptr (CppVec (Ptr MX')))))
blocksplit''''''''''''
  :: MX -> Vector Int -> Vector Int -> IO (Vector (Vector MX))
blocksplit'''''''''''' x0 x1 x2 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  withMarshal x2 $ \x2' ->
  c_CasADi__blocksplit_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC x0' x1' x2' >>= wrapReturn

foreign import ccall unsafe "CasADi__blocksplit_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC" c_CasADi__blocksplit_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC
  :: Ptr MX' -> CInt -> CInt -> IO (Ptr (CppVec (Ptr (CppVec (Ptr MX')))))
blocksplit'''''''''''''
  :: MX -> Int -> Int -> IO (Vector (Vector MX))
blocksplit''''''''''''' x0 x1 x2 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  withMarshal x2 $ \x2' ->
  c_CasADi__blocksplit_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC x0' x1' x2' >>= wrapReturn

foreign import ccall unsafe "CasADi__blocksplit_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC" c_CasADi__blocksplit_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC
  :: Ptr MX' -> CInt -> IO (Ptr (CppVec (Ptr (CppVec (Ptr MX')))))
blocksplit''''''''''''''
  :: MX -> Int -> IO (Vector (Vector MX))
blocksplit'''''''''''''' x0 x1 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  c_CasADi__blocksplit_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC x0' x1' >>= wrapReturn

foreign import ccall unsafe "CasADi__blocksplit_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC" c_CasADi__blocksplit_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC
  :: Ptr MX' -> IO (Ptr (CppVec (Ptr (CppVec (Ptr MX')))))
blocksplit'''''''''''''''
  :: MX -> IO (Vector (Vector MX))
blocksplit''''''''''''''' x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__blocksplit_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__veccat_TIC_TIC_TIC" c_CasADi__veccat_TIC_TIC_TIC
  :: Ptr (CppVec (Ptr MX')) -> IO (Ptr MX')
veccat'''
  :: Vector MX -> IO MX
veccat''' x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__veccat_TIC_TIC_TIC x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__vecNZcat_TIC_TIC_TIC" c_CasADi__vecNZcat_TIC_TIC_TIC
  :: Ptr (CppVec (Ptr MX')) -> IO (Ptr MX')
vecNZcat'''
  :: Vector MX -> IO MX
vecNZcat''' x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__vecNZcat_TIC_TIC_TIC x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__norm_F_TIC_TIC_TIC" c_CasADi__norm_F_TIC_TIC_TIC
  :: Ptr MX' -> IO (Ptr MX')
norm_F'''
  :: MX -> IO MX
norm_F''' x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__norm_F_TIC_TIC_TIC x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__norm_2_TIC_TIC_TIC" c_CasADi__norm_2_TIC_TIC_TIC
  :: Ptr MX' -> IO (Ptr MX')
norm_2'''
  :: MX -> IO MX
norm_2''' x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__norm_2_TIC_TIC_TIC x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__norm_1_TIC_TIC_TIC" c_CasADi__norm_1_TIC_TIC_TIC
  :: Ptr MX' -> IO (Ptr MX')
norm_1'''
  :: MX -> IO MX
norm_1''' x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__norm_1_TIC_TIC_TIC x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__norm_inf_TIC_TIC_TIC" c_CasADi__norm_inf_TIC_TIC_TIC
  :: Ptr MX' -> IO (Ptr MX')
norm_inf'''
  :: MX -> IO MX
norm_inf''' x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__norm_inf_TIC_TIC_TIC x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__transpose_TIC_TIC_TIC_TIC" c_CasADi__transpose_TIC_TIC_TIC_TIC
  :: Ptr MX' -> IO (Ptr MX')
transpose''''
  :: MX -> IO MX
transpose'''' x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__transpose_TIC_TIC_TIC_TIC x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__mul_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC" c_CasADi__mul_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC
  :: Ptr MX' -> Ptr MX' -> Ptr Sparsity' -> IO (Ptr MX')
mul''''''''''
  :: MX -> MX -> Sparsity -> IO MX
mul'''''''''' x0 x1 x2 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  withMarshal x2 $ \x2' ->
  c_CasADi__mul_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC x0' x1' x2' >>= wrapReturn

foreign import ccall unsafe "CasADi__mul_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC" c_CasADi__mul_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC
  :: Ptr MX' -> Ptr MX' -> IO (Ptr MX')
mul'''''''''''
  :: MX -> MX -> IO MX
mul''''''''''' x0 x1 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  c_CasADi__mul_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC x0' x1' >>= wrapReturn

foreign import ccall unsafe "CasADi__mul_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC" c_CasADi__mul_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC
  :: Ptr (CppVec (Ptr MX')) -> IO (Ptr MX')
mul''''''''''''
  :: Vector MX -> IO MX
mul'''''''''''' x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__mul_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__inner_prod_TIC_TIC_TIC" c_CasADi__inner_prod_TIC_TIC_TIC
  :: Ptr MX' -> Ptr MX' -> IO (Ptr MX')
inner_prod'''
  :: MX -> MX -> IO MX
inner_prod''' x0 x1 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  c_CasADi__inner_prod_TIC_TIC_TIC x0' x1' >>= wrapReturn

foreign import ccall unsafe "CasADi__outer_prod_TIC_TIC_TIC" c_CasADi__outer_prod_TIC_TIC_TIC
  :: Ptr MX' -> Ptr MX' -> IO (Ptr MX')
outer_prod'''
  :: MX -> MX -> IO MX
outer_prod''' x0 x1 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  c_CasADi__outer_prod_TIC_TIC_TIC x0' x1' >>= wrapReturn

foreign import ccall unsafe "CasADi__if_else_TIC" c_CasADi__if_else_TIC
  :: Ptr MX' -> Ptr MX' -> Ptr MX' -> IO (Ptr MX')
if_else'
  :: MX -> MX -> MX -> IO MX
if_else' x0 x1 x2 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  withMarshal x2 $ \x2' ->
  c_CasADi__if_else_TIC x0' x1' x2' >>= wrapReturn

foreign import ccall unsafe "CasADi__reshape_TIC_TIC_TIC_TIC_TIC_TIC_TIC" c_CasADi__reshape_TIC_TIC_TIC_TIC_TIC_TIC_TIC
  :: Ptr MX' -> Ptr Sparsity' -> IO (Ptr MX')
reshape'''''''
  :: MX -> Sparsity -> IO MX
reshape''''''' x0 x1 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  c_CasADi__reshape_TIC_TIC_TIC_TIC_TIC_TIC_TIC x0' x1' >>= wrapReturn

foreign import ccall unsafe "CasADi__vec_TIC_TIC_TIC_TIC" c_CasADi__vec_TIC_TIC_TIC_TIC
  :: Ptr MX' -> IO (Ptr MX')
vec''''
  :: MX -> IO MX
vec'''' x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__vec_TIC_TIC_TIC_TIC x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__vecNZ_TIC_TIC_TIC" c_CasADi__vecNZ_TIC_TIC_TIC
  :: Ptr MX' -> IO (Ptr MX')
vecNZ'''
  :: MX -> IO MX
vecNZ''' x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__vecNZ_TIC_TIC_TIC x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__unite_TIC_TIC_TIC" c_CasADi__unite_TIC_TIC_TIC
  :: Ptr MX' -> Ptr MX' -> IO (Ptr MX')
unite'''
  :: MX -> MX -> IO MX
unite''' x0 x1 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  c_CasADi__unite_TIC_TIC_TIC x0' x1' >>= wrapReturn

foreign import ccall unsafe "CasADi__simplify_TIC_TIC" c_CasADi__simplify_TIC_TIC
  :: Ptr MX' -> IO ()
simplify''
  :: MX -> IO ()
simplify'' x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__simplify_TIC_TIC x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__trace_TIC_TIC_TIC" c_CasADi__trace_TIC_TIC_TIC
  :: Ptr MX' -> IO (Ptr MX')
trace'''
  :: MX -> IO MX
trace''' x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__trace_TIC_TIC_TIC x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__repmat_TIC_TIC_TIC" c_CasADi__repmat_TIC_TIC_TIC
  :: Ptr MX' -> CInt -> CInt -> IO (Ptr MX')
repmat'''
  :: MX -> Int -> Int -> IO MX
repmat''' x0 x1 x2 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  withMarshal x2 $ \x2' ->
  c_CasADi__repmat_TIC_TIC_TIC x0' x1' x2' >>= wrapReturn

foreign import ccall unsafe "CasADi__dense_TIC_TIC_TIC" c_CasADi__dense_TIC_TIC_TIC
  :: Ptr MX' -> IO (Ptr MX')
dense'''
  :: MX -> IO MX
dense''' x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__dense_TIC_TIC_TIC x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__createParent" c_CasADi__createParent
  :: Ptr (CppVec (Ptr MX')) -> IO (Ptr MX')
{-|
>>  MX CasADi::createParent(std::vector< MX > &deps)
>------------------------------------------------------------------------
>
>Create a parent MX on which all given MX's will depend.
>
>In some sense, this function is the inverse of
>
>Parameters:
>-----------
>
>deps:  Must all be symbolic matrices.
>
>>  MX CasADi::createParent(const std::vector< Sparsity > &deps, std::vector< MX > &output_children)
>
>>  MX CasADi::createParent(const std::vector< MX > &deps, std::vector< MX > &output_children)
>------------------------------------------------------------------------
>
>Create a parent MX on which a bunch of MX's (sizes given as argument) will
>depend.
-}
createParent
  :: Vector MX -> IO MX
createParent x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__createParent x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__createParent_TIC" c_CasADi__createParent_TIC
  :: Ptr (CppVec (Ptr MX')) -> Ptr (CppVec (Ptr MX')) -> IO (Ptr MX')
createParent'
  :: Vector MX -> Vector MX -> IO MX
createParent' x0 x1 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  c_CasADi__createParent_TIC x0' x1' >>= wrapReturn

foreign import ccall unsafe "CasADi__createParent_TIC_TIC" c_CasADi__createParent_TIC_TIC
  :: Ptr (CppVec (Ptr Sparsity')) -> Ptr (CppVec (Ptr MX')) -> IO (Ptr MX')
createParent''
  :: Vector Sparsity -> Vector MX -> IO MX
createParent'' x0 x1 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  c_CasADi__createParent_TIC_TIC x0' x1' >>= wrapReturn

foreign import ccall unsafe "CasADi__countNodes_TIC" c_CasADi__countNodes_TIC
  :: Ptr MX' -> IO CInt
countNodes'
  :: MX -> IO Int
countNodes' x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__countNodes_TIC x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__diag_TIC_TIC_TIC" c_CasADi__diag_TIC_TIC_TIC
  :: Ptr MX' -> IO (Ptr MX')
diag'''
  :: MX -> IO MX
diag''' x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__diag_TIC_TIC_TIC x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__blkdiag_TIC_TIC_TIC_TIC" c_CasADi__blkdiag_TIC_TIC_TIC_TIC
  :: Ptr (CppVec (Ptr MX')) -> IO (Ptr MX')
blkdiag''''
  :: Vector MX -> IO MX
blkdiag'''' x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__blkdiag_TIC_TIC_TIC_TIC x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__sumCols_TIC_TIC_TIC" c_CasADi__sumCols_TIC_TIC_TIC
  :: Ptr MX' -> IO (Ptr MX')
sumCols'''
  :: MX -> IO MX
sumCols''' x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__sumCols_TIC_TIC_TIC x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__sumRows_TIC_TIC_TIC" c_CasADi__sumRows_TIC_TIC_TIC
  :: Ptr MX' -> IO (Ptr MX')
sumRows'''
  :: MX -> IO MX
sumRows''' x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__sumRows_TIC_TIC_TIC x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__sumAll_TIC_TIC_TIC" c_CasADi__sumAll_TIC_TIC_TIC
  :: Ptr MX' -> IO (Ptr MX')
sumAll'''
  :: MX -> IO MX
sumAll''' x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__sumAll_TIC_TIC_TIC x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__polyval_TIC_TIC_TIC" c_CasADi__polyval_TIC_TIC_TIC
  :: Ptr MX' -> Ptr MX' -> IO (Ptr MX')
polyval'''
  :: MX -> MX -> IO MX
polyval''' x0 x1 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  c_CasADi__polyval_TIC_TIC_TIC x0' x1' >>= wrapReturn

foreign import ccall unsafe "CasADi__getOperatorRepresentation_TIC" c_CasADi__getOperatorRepresentation_TIC
  :: Ptr MX' -> Ptr (CppVec (Ptr StdString')) -> IO (Ptr StdString')
getOperatorRepresentation'
  :: MX -> Vector String -> IO String
getOperatorRepresentation' x0 x1 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  c_CasADi__getOperatorRepresentation_TIC x0' x1' >>= wrapReturn

foreign import ccall unsafe "CasADi__substitute_TIC_TIC" c_CasADi__substitute_TIC_TIC
  :: Ptr MX' -> Ptr MX' -> Ptr MX' -> IO (Ptr MX')
substitute''
  :: MX -> MX -> MX -> IO MX
substitute'' x0 x1 x2 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  withMarshal x2 $ \x2' ->
  c_CasADi__substitute_TIC_TIC x0' x1' x2' >>= wrapReturn

foreign import ccall unsafe "CasADi__substitute_TIC_TIC_TIC" c_CasADi__substitute_TIC_TIC_TIC
  :: Ptr (CppVec (Ptr MX')) -> Ptr (CppVec (Ptr MX')) -> Ptr (CppVec (Ptr MX')) -> IO (Ptr (CppVec (Ptr MX')))
substitute'''
  :: Vector MX -> Vector MX -> Vector MX -> IO (Vector MX)
substitute''' x0 x1 x2 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  withMarshal x2 $ \x2' ->
  c_CasADi__substitute_TIC_TIC_TIC x0' x1' x2' >>= wrapReturn

foreign import ccall unsafe "CasADi__graph_substitute" c_CasADi__graph_substitute
  :: Ptr MX' -> Ptr (CppVec (Ptr MX')) -> Ptr (CppVec (Ptr MX')) -> IO (Ptr MX')
{-|
>>  MX CasADi::graph_substitute(const MX &ex, const std::vector< MX > &v, const std::vector< MX > &vdef)
>------------------------------------------------------------------------
>
>Substitute variable v with expression vdef in an expression ex, preserving
>nodes.
>
>>  std::vector< MX > CasADi::graph_substitute(const std::vector< MX > &ex, const std::vector< MX > &v, const std::vector< MX > &vdef)
>------------------------------------------------------------------------
>
>Substitute variable var with expression expr in multiple expressions,
>preserving nodes.
-}
graph_substitute
  :: MX -> Vector MX -> Vector MX -> IO MX
graph_substitute x0 x1 x2 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  withMarshal x2 $ \x2' ->
  c_CasADi__graph_substitute x0' x1' x2' >>= wrapReturn

foreign import ccall unsafe "CasADi__graph_substitute_TIC" c_CasADi__graph_substitute_TIC
  :: Ptr (CppVec (Ptr MX')) -> Ptr (CppVec (Ptr MX')) -> Ptr (CppVec (Ptr MX')) -> IO (Ptr (CppVec (Ptr MX')))
graph_substitute'
  :: Vector MX -> Vector MX -> Vector MX -> IO (Vector MX)
graph_substitute' x0 x1 x2 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  withMarshal x2 $ \x2' ->
  c_CasADi__graph_substitute_TIC x0' x1' x2' >>= wrapReturn

foreign import ccall unsafe "CasADi__substituteInPlace_TIC_TIC_TIC_TIC_TIC_TIC" c_CasADi__substituteInPlace_TIC_TIC_TIC_TIC_TIC_TIC
  :: Ptr (CppVec (Ptr MX')) -> Ptr (CppVec (Ptr MX')) -> CInt -> IO ()
substituteInPlace''''''
  :: Vector MX -> Vector MX -> Bool -> IO ()
substituteInPlace'''''' x0 x1 x2 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  withMarshal x2 $ \x2' ->
  c_CasADi__substituteInPlace_TIC_TIC_TIC_TIC_TIC_TIC x0' x1' x2' >>= wrapReturn

foreign import ccall unsafe "CasADi__substituteInPlace_TIC_TIC_TIC_TIC_TIC_TIC_TIC" c_CasADi__substituteInPlace_TIC_TIC_TIC_TIC_TIC_TIC_TIC
  :: Ptr (CppVec (Ptr MX')) -> Ptr (CppVec (Ptr MX')) -> IO ()
substituteInPlace'''''''
  :: Vector MX -> Vector MX -> IO ()
substituteInPlace''''''' x0 x1 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  c_CasADi__substituteInPlace_TIC_TIC_TIC_TIC_TIC_TIC_TIC x0' x1' >>= wrapReturn

foreign import ccall unsafe "CasADi__substituteInPlace_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC" c_CasADi__substituteInPlace_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC
  :: Ptr (CppVec (Ptr MX')) -> Ptr (CppVec (Ptr MX')) -> Ptr (CppVec (Ptr MX')) -> CInt -> IO ()
substituteInPlace''''''''
  :: Vector MX -> Vector MX -> Vector MX -> Bool -> IO ()
substituteInPlace'''''''' x0 x1 x2 x3 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  withMarshal x2 $ \x2' ->
  withMarshal x3 $ \x3' ->
  c_CasADi__substituteInPlace_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC x0' x1' x2' x3' >>= wrapReturn

foreign import ccall unsafe "CasADi__substituteInPlace_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC" c_CasADi__substituteInPlace_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC
  :: Ptr (CppVec (Ptr MX')) -> Ptr (CppVec (Ptr MX')) -> Ptr (CppVec (Ptr MX')) -> IO ()
substituteInPlace'''''''''
  :: Vector MX -> Vector MX -> Vector MX -> IO ()
substituteInPlace''''''''' x0 x1 x2 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  withMarshal x2 $ \x2' ->
  c_CasADi__substituteInPlace_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC x0' x1' x2' >>= wrapReturn

foreign import ccall unsafe "CasADi__extractShared_TIC_TIC_TIC" c_CasADi__extractShared_TIC_TIC_TIC
  :: Ptr (CppVec (Ptr MX')) -> Ptr (CppVec (Ptr MX')) -> Ptr (CppVec (Ptr MX')) -> Ptr StdString' -> Ptr StdString' -> IO ()
extractShared'''
  :: Vector MX -> Vector MX -> Vector MX -> String -> String -> IO ()
extractShared''' x0 x1 x2 x3 x4 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  withMarshal x2 $ \x2' ->
  withMarshal x3 $ \x3' ->
  withMarshal x4 $ \x4' ->
  c_CasADi__extractShared_TIC_TIC_TIC x0' x1' x2' x3' x4' >>= wrapReturn

foreign import ccall unsafe "CasADi__extractShared_TIC_TIC_TIC_TIC" c_CasADi__extractShared_TIC_TIC_TIC_TIC
  :: Ptr (CppVec (Ptr MX')) -> Ptr (CppVec (Ptr MX')) -> Ptr (CppVec (Ptr MX')) -> Ptr StdString' -> IO ()
extractShared''''
  :: Vector MX -> Vector MX -> Vector MX -> String -> IO ()
extractShared'''' x0 x1 x2 x3 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  withMarshal x2 $ \x2' ->
  withMarshal x3 $ \x3' ->
  c_CasADi__extractShared_TIC_TIC_TIC_TIC x0' x1' x2' x3' >>= wrapReturn

foreign import ccall unsafe "CasADi__extractShared_TIC_TIC_TIC_TIC_TIC" c_CasADi__extractShared_TIC_TIC_TIC_TIC_TIC
  :: Ptr (CppVec (Ptr MX')) -> Ptr (CppVec (Ptr MX')) -> Ptr (CppVec (Ptr MX')) -> IO ()
extractShared'''''
  :: Vector MX -> Vector MX -> Vector MX -> IO ()
extractShared''''' x0 x1 x2 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  withMarshal x2 $ \x2' ->
  c_CasADi__extractShared_TIC_TIC_TIC_TIC_TIC x0' x1' x2' >>= wrapReturn

foreign import ccall unsafe "CasADi__printCompact_TIC_TIC_TIC" c_CasADi__printCompact_TIC_TIC_TIC
  :: Ptr MX' -> IO ()
printCompact'''
  :: MX -> IO ()
printCompact''' x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__printCompact_TIC_TIC_TIC x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__jacobian_TIC" c_CasADi__jacobian_TIC
  :: Ptr MX' -> Ptr MX' -> IO (Ptr MX')
jacobian'
  :: MX -> MX -> IO MX
jacobian' x0 x1 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  c_CasADi__jacobian_TIC x0' x1' >>= wrapReturn

foreign import ccall unsafe "CasADi__gradient_TIC" c_CasADi__gradient_TIC
  :: Ptr MX' -> Ptr MX' -> IO (Ptr MX')
gradient'
  :: MX -> MX -> IO MX
gradient' x0 x1 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  c_CasADi__gradient_TIC x0' x1' >>= wrapReturn

foreign import ccall unsafe "CasADi__tangent_TIC" c_CasADi__tangent_TIC
  :: Ptr MX' -> Ptr MX' -> IO (Ptr MX')
tangent'
  :: MX -> MX -> IO MX
tangent' x0 x1 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  c_CasADi__tangent_TIC x0' x1' >>= wrapReturn

foreign import ccall unsafe "CasADi__nullspace_TIC_TIC_TIC" c_CasADi__nullspace_TIC_TIC_TIC
  :: Ptr MX' -> IO (Ptr MX')
nullspace'''
  :: MX -> IO MX
nullspace''' x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__nullspace_TIC_TIC_TIC x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__det_TIC_TIC_TIC" c_CasADi__det_TIC_TIC_TIC
  :: Ptr MX' -> IO (Ptr MX')
det'''
  :: MX -> IO MX
det''' x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__det_TIC_TIC_TIC x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__inv_TIC_TIC_TIC" c_CasADi__inv_TIC_TIC_TIC
  :: Ptr MX' -> IO (Ptr MX')
inv'''
  :: MX -> IO MX
inv''' x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__inv_TIC_TIC_TIC x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__getSymbols_TIC" c_CasADi__getSymbols_TIC
  :: Ptr MX' -> IO (Ptr (CppVec (Ptr MX')))
getSymbols'
  :: MX -> IO (Vector MX)
getSymbols' x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__getSymbols_TIC x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__getSymbols_TIC_TIC" c_CasADi__getSymbols_TIC_TIC
  :: Ptr (CppVec (Ptr MX')) -> IO (Ptr (CppVec (Ptr MX')))
getSymbols''
  :: Vector MX -> IO (Vector MX)
getSymbols'' x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__getSymbols_TIC_TIC x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__dependsOn_TIC" c_CasADi__dependsOn_TIC
  :: Ptr MX' -> Ptr (CppVec (Ptr MX')) -> IO CInt
dependsOn'
  :: MX -> Vector MX -> IO Bool
dependsOn' x0 x1 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  c_CasADi__dependsOn_TIC x0' x1' >>= wrapReturn

foreign import ccall unsafe "CasADi__matrix_expand" c_CasADi__matrix_expand
  :: Ptr MX' -> Ptr (CppVec (Ptr MX')) -> IO (Ptr MX')
{-|
>Expand MX graph to SXFunction call.
>
>Expand the given expression e, optionally supplying expressions contained in
>it at which expansion should stop.
-}
matrix_expand
  :: MX -> Vector MX -> IO MX
matrix_expand x0 x1 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  c_CasADi__matrix_expand x0' x1' >>= wrapReturn

foreign import ccall unsafe "CasADi__matrix_expand_TIC" c_CasADi__matrix_expand_TIC
  :: Ptr MX' -> IO (Ptr MX')
matrix_expand'
  :: MX -> IO MX
matrix_expand' x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__matrix_expand_TIC x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__matrix_expand_TIC_TIC" c_CasADi__matrix_expand_TIC_TIC
  :: Ptr (CppVec (Ptr MX')) -> Ptr (CppVec (Ptr MX')) -> IO (Ptr (CppVec (Ptr MX')))
matrix_expand''
  :: Vector MX -> Vector MX -> IO (Vector MX)
matrix_expand'' x0 x1 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  c_CasADi__matrix_expand_TIC_TIC x0' x1' >>= wrapReturn

foreign import ccall unsafe "CasADi__matrix_expand_TIC_TIC_TIC" c_CasADi__matrix_expand_TIC_TIC_TIC
  :: Ptr (CppVec (Ptr MX')) -> IO (Ptr (CppVec (Ptr MX')))
matrix_expand'''
  :: Vector MX -> IO (Vector MX)
matrix_expand''' x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__matrix_expand_TIC_TIC_TIC x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__kron_TIC_TIC_TIC" c_CasADi__kron_TIC_TIC_TIC
  :: Ptr MX' -> Ptr MX' -> IO (Ptr MX')
kron'''
  :: MX -> MX -> IO MX
kron''' x0 x1 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  c_CasADi__kron_TIC_TIC_TIC x0' x1' >>= wrapReturn

foreign import ccall unsafe "CasADi__solve_TIC_TIC_TIC" c_CasADi__solve_TIC_TIC_TIC
  :: Ptr MX' -> Ptr MX' -> IO (Ptr MX')
solve'''
  :: MX -> MX -> IO MX
solve''' x0 x1 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  c_CasADi__solve_TIC_TIC_TIC x0' x1' >>= wrapReturn

foreign import ccall unsafe "CasADi__collocationPoints" c_CasADi__collocationPoints
  :: CInt -> Ptr StdString' -> IO (Ptr (CppVec CDouble))
{-|
>Obtain collocation points of specific order and scheme.
>
>Parameters:
>-----------
>
>scheme:  'radau' or 'legendre'
-}
collocationPoints
  :: Int -> String -> IO (Vector Double)
collocationPoints x0 x1 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  c_CasADi__collocationPoints x0' x1' >>= wrapReturn

foreign import ccall unsafe "CasADi__collocationPoints_TIC" c_CasADi__collocationPoints_TIC
  :: CInt -> IO (Ptr (CppVec CDouble))
collocationPoints'
  :: Int -> IO (Vector Double)
collocationPoints' x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__collocationPoints_TIC x0' >>= wrapReturn

foreign import ccall unsafe "CasADi__collocationInterpolators" c_CasADi__collocationInterpolators
  :: Ptr (CppVec CDouble) -> Ptr (CppVec (Ptr (CppVec CDouble))) -> Ptr (CppVec CDouble) -> IO ()
{-|
>[INTERNAL]  Obtain
>collocation interpolating matrices.
>
>Parameters:
>-----------
>
>tau_root:  location of collocation points, as obtained from
>collocationPoints
>
>C:  interpolating coefficients to obtain derivatives Length: order+1, order
>+ 1
>
>dX/dt (j) ~ Sum_i C[j][i]*X(i)
>
>Parameters:
>-----------
>
>D:  interpolating coefficients to obtain end state Length: order+1
-}
collocationInterpolators
  :: Vector Double -> Vector (Vector Double) -> Vector Double -> IO ()
collocationInterpolators x0 x1 x2 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  withMarshal x2 $ \x2' ->
  c_CasADi__collocationInterpolators x0' x1' x2' >>= wrapReturn

foreign import ccall unsafe "CasADi__explicitRK" c_CasADi__explicitRK
  :: Ptr Function' -> Ptr MX' -> CInt -> CInt -> IO (Ptr Function')
{-|
>Construct an explicit Runge-Kutta integrator.
>
>Parameters:
>-----------
>
>f:  dynamical system
>
>>Input scheme: CasADi::DAEInput (DAE_NUM_IN = 5) [daeIn]
>+-----------+-------+----------------------------+
>| Full name | Short |        Description         |
>+===========+=======+============================+
>| DAE_X     | x     | Differential state .       |
>+-----------+-------+----------------------------+
>| DAE_Z     | z     | Algebraic state .          |
>+-----------+-------+----------------------------+
>| DAE_P     | p     | Parameter .                |
>+-----------+-------+----------------------------+
>| DAE_T     | t     | Explicit time dependence . |
>+-----------+-------+----------------------------+
>
>>Output scheme: CasADi::DAEOutput (DAE_NUM_OUT = 4) [daeOut]
>+-----------+-------+--------------------------------------------+
>| Full name | Short |                Description                 |
>+===========+=======+============================================+
>| DAE_ODE   | ode   | Right hand side of the implicit ODE .      |
>+-----------+-------+--------------------------------------------+
>| DAE_ALG   | alg   | Right hand side of algebraic equations .   |
>+-----------+-------+--------------------------------------------+
>| DAE_QUAD  | quad  | Right hand side of quadratures equations . |
>+-----------+-------+--------------------------------------------+
>
>Parameters:
>-----------
>
>tf:  Integration end time
>
>order:  Order of integration
>
>ne:  Number of times the RK primitive is repeated over the integration
>interval
-}
explicitRK
  :: Function -> MX -> Int -> Int -> IO Function
explicitRK x0 x1 x2 x3 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  withMarshal x2 $ \x2' ->
  withMarshal x3 $ \x3' ->
  c_CasADi__explicitRK x0' x1' x2' x3' >>= wrapReturn

foreign import ccall unsafe "CasADi__explicitRK_TIC" c_CasADi__explicitRK_TIC
  :: Ptr Function' -> Ptr MX' -> CInt -> IO (Ptr Function')
explicitRK'
  :: Function -> MX -> Int -> IO Function
explicitRK' x0 x1 x2 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  withMarshal x2 $ \x2' ->
  c_CasADi__explicitRK_TIC x0' x1' x2' >>= wrapReturn

foreign import ccall unsafe "CasADi__explicitRK_TIC_TIC" c_CasADi__explicitRK_TIC_TIC
  :: Ptr Function' -> Ptr MX' -> IO (Ptr Function')
explicitRK''
  :: Function -> MX -> IO Function
explicitRK'' x0 x1 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  c_CasADi__explicitRK_TIC_TIC x0' x1' >>= wrapReturn

foreign import ccall unsafe "CasADi__explicitRK_TIC_TIC_TIC" c_CasADi__explicitRK_TIC_TIC_TIC
  :: Ptr Function' -> IO (Ptr Function')
explicitRK'''
  :: Function -> IO Function
explicitRK''' x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__explicitRK_TIC_TIC_TIC x0' >>= wrapReturn

