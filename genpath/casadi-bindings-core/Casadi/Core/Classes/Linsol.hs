{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# Language ForeignFunctionInterface #-}
{-# Language FlexibleInstances #-}
{-# Language MultiParamTypeClasses #-}

module Casadi.Core.Classes.Linsol
       (
         Linsol,
         LinsolClass(..),
         linsol__0,
         linsol__1,
         linsol__2,
         linsol_cholesky__0,
         linsol_cholesky__1,
         linsol_cholesky_sparsity__0,
         linsol_cholesky_sparsity__1,
         linsol_doc,
         linsol_has_plugin,
         linsol_load_plugin,
         linsol_neig,
         linsol_plugin_name,
         linsol_rank,
         linsol_solve__0,
         linsol_solve__1,
         linsol_solve__2,
         linsol_solve__3,
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
foreign import ccall unsafe "casadi__Linsol__CONSTRUCTOR__0" c_casadi__Linsol__CONSTRUCTOR__0
  :: Ptr (Ptr StdString) -> Ptr StdString -> Ptr StdString -> IO (Ptr Linsol')

casadi__Linsol__CONSTRUCTOR__0
  :: String -> String -> IO Linsol
casadi__Linsol__CONSTRUCTOR__0 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Linsol__CONSTRUCTOR__0 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
linsol__0 :: String -> String -> IO Linsol
linsol__0 = casadi__Linsol__CONSTRUCTOR__0


-- direct wrapper
foreign import ccall unsafe "casadi__Linsol__CONSTRUCTOR__1" c_casadi__Linsol__CONSTRUCTOR__1
  :: Ptr (Ptr StdString) -> Ptr StdString -> Ptr StdString -> Ptr (StdMap StdString (Ptr GenericType')) -> IO (Ptr Linsol')

casadi__Linsol__CONSTRUCTOR__1
  :: String -> String -> M.Map String GenericType -> IO Linsol
casadi__Linsol__CONSTRUCTOR__1 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Linsol__CONSTRUCTOR__1 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



-- classy wrapper
linsol__1 :: String -> String -> M.Map String GenericType -> IO Linsol
linsol__1 = casadi__Linsol__CONSTRUCTOR__1


-- direct wrapper
foreign import ccall unsafe "casadi__Linsol__CONSTRUCTOR__2" c_casadi__Linsol__CONSTRUCTOR__2
  :: Ptr (Ptr StdString) -> IO (Ptr Linsol')

casadi__Linsol__CONSTRUCTOR__2
  :: IO Linsol
casadi__Linsol__CONSTRUCTOR__2  = do


  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Linsol__CONSTRUCTOR__2 errStrPtrP 
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)



  return ret



-- classy wrapper
linsol__2 :: IO Linsol
linsol__2 = casadi__Linsol__CONSTRUCTOR__2


-- direct wrapper
foreign import ccall unsafe "casadi__Linsol__cholesky__0" c_casadi__Linsol__cholesky__0
  :: Ptr (Ptr StdString) -> Ptr Linsol' -> IO (Ptr DM')

casadi__Linsol__cholesky__0
  :: Linsol -> IO DM
casadi__Linsol__cholesky__0 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Linsol__cholesky__0 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
linsol_cholesky__0 :: LinsolClass a => a -> IO DM
linsol_cholesky__0 x = casadi__Linsol__cholesky__0 (castLinsol x)


-- direct wrapper
foreign import ccall unsafe "casadi__Linsol__cholesky__1" c_casadi__Linsol__cholesky__1
  :: Ptr (Ptr StdString) -> Ptr Linsol' -> CInt -> IO (Ptr DM')

casadi__Linsol__cholesky__1
  :: Linsol -> Bool -> IO DM
casadi__Linsol__cholesky__1 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Linsol__cholesky__1 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
linsol_cholesky__1 :: LinsolClass a => a -> Bool -> IO DM
linsol_cholesky__1 x = casadi__Linsol__cholesky__1 (castLinsol x)


-- direct wrapper
foreign import ccall unsafe "casadi__Linsol__cholesky_sparsity__0" c_casadi__Linsol__cholesky_sparsity__0
  :: Ptr (Ptr StdString) -> Ptr Linsol' -> IO (Ptr Sparsity')

casadi__Linsol__cholesky_sparsity__0
  :: Linsol -> IO Sparsity
casadi__Linsol__cholesky_sparsity__0 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Linsol__cholesky_sparsity__0 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
linsol_cholesky_sparsity__0 :: LinsolClass a => a -> IO Sparsity
linsol_cholesky_sparsity__0 x = casadi__Linsol__cholesky_sparsity__0 (castLinsol x)


-- direct wrapper
foreign import ccall unsafe "casadi__Linsol__cholesky_sparsity__1" c_casadi__Linsol__cholesky_sparsity__1
  :: Ptr (Ptr StdString) -> Ptr Linsol' -> CInt -> IO (Ptr Sparsity')

casadi__Linsol__cholesky_sparsity__1
  :: Linsol -> Bool -> IO Sparsity
casadi__Linsol__cholesky_sparsity__1 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Linsol__cholesky_sparsity__1 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
linsol_cholesky_sparsity__1 :: LinsolClass a => a -> Bool -> IO Sparsity
linsol_cholesky_sparsity__1 x = casadi__Linsol__cholesky_sparsity__1 (castLinsol x)


-- direct wrapper
foreign import ccall unsafe "casadi__Linsol__doc" c_casadi__Linsol__doc
  :: Ptr (Ptr StdString) -> Ptr StdString -> IO (Ptr StdString)

casadi__Linsol__doc
  :: String -> IO String
casadi__Linsol__doc x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Linsol__doc errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
linsol_doc :: String -> IO String
linsol_doc = casadi__Linsol__doc


-- direct wrapper
foreign import ccall unsafe "casadi__Linsol__has_plugin" c_casadi__Linsol__has_plugin
  :: Ptr (Ptr StdString) -> Ptr StdString -> IO CInt

casadi__Linsol__has_plugin
  :: String -> IO Bool
casadi__Linsol__has_plugin x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Linsol__has_plugin errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
linsol_has_plugin :: String -> IO Bool
linsol_has_plugin = casadi__Linsol__has_plugin


-- direct wrapper
foreign import ccall unsafe "casadi__Linsol__load_plugin" c_casadi__Linsol__load_plugin
  :: Ptr (Ptr StdString) -> Ptr StdString -> IO ()

casadi__Linsol__load_plugin
  :: String -> IO ()
casadi__Linsol__load_plugin x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Linsol__load_plugin errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ()



-- classy wrapper
linsol_load_plugin :: String -> IO ()
linsol_load_plugin = casadi__Linsol__load_plugin


-- direct wrapper
foreign import ccall unsafe "casadi__Linsol__neig" c_casadi__Linsol__neig
  :: Ptr (Ptr StdString) -> Ptr Linsol' -> IO CInt

casadi__Linsol__neig
  :: Linsol -> IO Int
casadi__Linsol__neig x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Linsol__neig errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
linsol_neig :: LinsolClass a => a -> IO Int
linsol_neig x = casadi__Linsol__neig (castLinsol x)


-- direct wrapper
foreign import ccall unsafe "casadi__Linsol__plugin_name" c_casadi__Linsol__plugin_name
  :: Ptr (Ptr StdString) -> Ptr Linsol' -> IO (Ptr StdString)

casadi__Linsol__plugin_name
  :: Linsol -> IO String
casadi__Linsol__plugin_name x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Linsol__plugin_name errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
linsol_plugin_name :: LinsolClass a => a -> IO String
linsol_plugin_name x = casadi__Linsol__plugin_name (castLinsol x)


-- direct wrapper
foreign import ccall unsafe "casadi__Linsol__rank" c_casadi__Linsol__rank
  :: Ptr (Ptr StdString) -> Ptr Linsol' -> IO CInt

casadi__Linsol__rank
  :: Linsol -> IO Int
casadi__Linsol__rank x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Linsol__rank errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
linsol_rank :: LinsolClass a => a -> IO Int
linsol_rank x = casadi__Linsol__rank (castLinsol x)


-- direct wrapper
foreign import ccall unsafe "casadi__Linsol__solve__0" c_casadi__Linsol__solve__0
  :: Ptr (Ptr StdString) -> Ptr Linsol' -> Ptr MX' -> Ptr MX' -> IO (Ptr MX')

casadi__Linsol__solve__0
  :: Linsol -> MX -> MX -> IO MX
casadi__Linsol__solve__0 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Linsol__solve__0 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



-- classy wrapper
linsol_solve__0 :: LinsolClass a => a -> MX -> MX -> IO MX
linsol_solve__0 x = casadi__Linsol__solve__0 (castLinsol x)


-- direct wrapper
foreign import ccall unsafe "casadi__Linsol__solve__1" c_casadi__Linsol__solve__1
  :: Ptr (Ptr StdString) -> Ptr Linsol' -> Ptr MX' -> Ptr MX' -> CInt -> IO (Ptr MX')

casadi__Linsol__solve__1
  :: Linsol -> MX -> MX -> Bool -> IO MX
casadi__Linsol__solve__1 x0 x1 x2 x3 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Linsol__solve__1 errStrPtrP x0' x1' x2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'

  return ret



-- classy wrapper
linsol_solve__1 :: LinsolClass a => a -> MX -> MX -> Bool -> IO MX
linsol_solve__1 x = casadi__Linsol__solve__1 (castLinsol x)


-- direct wrapper
foreign import ccall unsafe "casadi__Linsol__solve__2" c_casadi__Linsol__solve__2
  :: Ptr (Ptr StdString) -> Ptr Linsol' -> Ptr DM' -> Ptr DM' -> IO (Ptr DM')

casadi__Linsol__solve__2
  :: Linsol -> DM -> DM -> IO DM
casadi__Linsol__solve__2 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Linsol__solve__2 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



-- classy wrapper
linsol_solve__2 :: LinsolClass a => a -> DM -> DM -> IO DM
linsol_solve__2 x = casadi__Linsol__solve__2 (castLinsol x)


-- direct wrapper
foreign import ccall unsafe "casadi__Linsol__solve__3" c_casadi__Linsol__solve__3
  :: Ptr (Ptr StdString) -> Ptr Linsol' -> Ptr DM' -> Ptr DM' -> CInt -> IO (Ptr DM')

casadi__Linsol__solve__3
  :: Linsol -> DM -> DM -> Bool -> IO DM
casadi__Linsol__solve__3 x0 x1 x2 x3 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Linsol__solve__3 errStrPtrP x0' x1' x2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'

  return ret



-- classy wrapper
linsol_solve__3 :: LinsolClass a => a -> DM -> DM -> Bool -> IO DM
linsol_solve__3 x = casadi__Linsol__solve__3 (castLinsol x)

