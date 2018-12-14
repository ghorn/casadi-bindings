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
         linsol_doc,
         linsol_has_plugin,
         linsol_load_plugin,
         linsol_neig,
         linsol_nfact,
         linsol_plugin_name,
         linsol_rank,
         linsol_sfact,
         linsol_solve__0,
         linsol_solve__1,
         linsol_solve__2,
         linsol_solve__3,
         linsol_sparsity,
         linsol_type_name,
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
  :: Ptr (Ptr StdString) -> Ptr StdString -> Ptr StdString -> Ptr Sparsity' -> IO (Ptr Linsol')

casadi__Linsol__CONSTRUCTOR__0
  :: String -> String -> Sparsity -> IO Linsol
casadi__Linsol__CONSTRUCTOR__0 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Linsol__CONSTRUCTOR__0 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



-- classy wrapper
linsol__0 :: String -> String -> Sparsity -> IO Linsol
linsol__0 = casadi__Linsol__CONSTRUCTOR__0


-- direct wrapper
foreign import ccall unsafe "casadi__Linsol__CONSTRUCTOR__1" c_casadi__Linsol__CONSTRUCTOR__1
  :: Ptr (Ptr StdString) -> Ptr StdString -> Ptr StdString -> Ptr Sparsity' -> Ptr (StdMap StdString (Ptr GenericType')) -> IO (Ptr Linsol')

casadi__Linsol__CONSTRUCTOR__1
  :: String -> String -> Sparsity -> M.Map String GenericType -> IO Linsol
casadi__Linsol__CONSTRUCTOR__1 x0 x1 x2 x3 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Linsol__CONSTRUCTOR__1 errStrPtrP x0' x1' x2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'

  return ret



-- classy wrapper
linsol__1 :: String -> String -> Sparsity -> M.Map String GenericType -> IO Linsol
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
  :: Ptr (Ptr StdString) -> Ptr Linsol' -> Ptr DM' -> IO CLLong

casadi__Linsol__neig
  :: Linsol -> DM -> IO Int
casadi__Linsol__neig x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Linsol__neig errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
linsol_neig :: LinsolClass a => a -> DM -> IO Int
linsol_neig x = casadi__Linsol__neig (castLinsol x)


-- direct wrapper
foreign import ccall unsafe "casadi__Linsol__nfact" c_casadi__Linsol__nfact
  :: Ptr (Ptr StdString) -> Ptr Linsol' -> Ptr DM' -> IO ()

casadi__Linsol__nfact
  :: Linsol -> DM -> IO ()
casadi__Linsol__nfact x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Linsol__nfact errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ()



-- classy wrapper
linsol_nfact :: LinsolClass a => a -> DM -> IO ()
linsol_nfact x = casadi__Linsol__nfact (castLinsol x)


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
  :: Ptr (Ptr StdString) -> Ptr Linsol' -> Ptr DM' -> IO CLLong

casadi__Linsol__rank
  :: Linsol -> DM -> IO Int
casadi__Linsol__rank x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Linsol__rank errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
linsol_rank :: LinsolClass a => a -> DM -> IO Int
linsol_rank x = casadi__Linsol__rank (castLinsol x)


-- direct wrapper
foreign import ccall unsafe "casadi__Linsol__sfact" c_casadi__Linsol__sfact
  :: Ptr (Ptr StdString) -> Ptr Linsol' -> Ptr DM' -> IO ()

casadi__Linsol__sfact
  :: Linsol -> DM -> IO ()
casadi__Linsol__sfact x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Linsol__sfact errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ()



-- classy wrapper
linsol_sfact :: LinsolClass a => a -> DM -> IO ()
linsol_sfact x = casadi__Linsol__sfact (castLinsol x)


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


-- direct wrapper
foreign import ccall unsafe "casadi__Linsol__sparsity" c_casadi__Linsol__sparsity
  :: Ptr (Ptr StdString) -> Ptr Linsol' -> IO (Ptr Sparsity')

casadi__Linsol__sparsity
  :: Linsol -> IO Sparsity
casadi__Linsol__sparsity x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Linsol__sparsity errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
linsol_sparsity :: LinsolClass a => a -> IO Sparsity
linsol_sparsity x = casadi__Linsol__sparsity (castLinsol x)


-- direct wrapper
foreign import ccall unsafe "casadi__Linsol__type_name" c_casadi__Linsol__type_name
  :: Ptr (Ptr StdString) -> IO (Ptr StdString)

casadi__Linsol__type_name
  :: IO String
casadi__Linsol__type_name  = do


  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Linsol__type_name errStrPtrP 
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)



  return ret



-- classy wrapper
linsol_type_name :: IO String
linsol_type_name = casadi__Linsol__type_name

