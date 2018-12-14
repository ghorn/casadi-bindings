{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# Language ForeignFunctionInterface #-}
{-# Language FlexibleInstances #-}
{-# Language MultiParamTypeClasses #-}

module Casadi.Core.Classes.NlpBuilder
       (
         NlpBuilder,
         NlpBuilderClass(..),
         nlpBuilder_get_str__0,
         nlpBuilder_get_str__1,
         nlpBuilder_import_nl__0,
         nlpBuilder_import_nl__1,
         nlpBuilder_type_name,
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
foreign import ccall unsafe "casadi__NlpBuilder__get_str__0" c_casadi__NlpBuilder__get_str__0
  :: Ptr (Ptr StdString) -> Ptr NlpBuilder' -> IO (Ptr StdString)

casadi__NlpBuilder__get_str__0
  :: NlpBuilder -> IO String
casadi__NlpBuilder__get_str__0 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__NlpBuilder__get_str__0 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
nlpBuilder_get_str__0 :: NlpBuilderClass a => a -> IO String
nlpBuilder_get_str__0 x = casadi__NlpBuilder__get_str__0 (castNlpBuilder x)


-- direct wrapper
foreign import ccall unsafe "casadi__NlpBuilder__get_str__1" c_casadi__NlpBuilder__get_str__1
  :: Ptr (Ptr StdString) -> Ptr NlpBuilder' -> CInt -> IO (Ptr StdString)

casadi__NlpBuilder__get_str__1
  :: NlpBuilder -> Bool -> IO String
casadi__NlpBuilder__get_str__1 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__NlpBuilder__get_str__1 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
nlpBuilder_get_str__1 :: NlpBuilderClass a => a -> Bool -> IO String
nlpBuilder_get_str__1 x = casadi__NlpBuilder__get_str__1 (castNlpBuilder x)


-- direct wrapper
foreign import ccall unsafe "casadi__NlpBuilder__import_nl__0" c_casadi__NlpBuilder__import_nl__0
  :: Ptr (Ptr StdString) -> Ptr NlpBuilder' -> Ptr StdString -> IO ()

casadi__NlpBuilder__import_nl__0
  :: NlpBuilder -> String -> IO ()
casadi__NlpBuilder__import_nl__0 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__NlpBuilder__import_nl__0 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ()



-- classy wrapper
nlpBuilder_import_nl__0 :: NlpBuilderClass a => a -> String -> IO ()
nlpBuilder_import_nl__0 x = casadi__NlpBuilder__import_nl__0 (castNlpBuilder x)


-- direct wrapper
foreign import ccall unsafe "casadi__NlpBuilder__import_nl__1" c_casadi__NlpBuilder__import_nl__1
  :: Ptr (Ptr StdString) -> Ptr NlpBuilder' -> Ptr StdString -> Ptr (StdMap StdString (Ptr GenericType')) -> IO ()

casadi__NlpBuilder__import_nl__1
  :: NlpBuilder -> String -> M.Map String GenericType -> IO ()
casadi__NlpBuilder__import_nl__1 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__NlpBuilder__import_nl__1 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ()



-- classy wrapper
nlpBuilder_import_nl__1 :: NlpBuilderClass a => a -> String -> M.Map String GenericType -> IO ()
nlpBuilder_import_nl__1 x = casadi__NlpBuilder__import_nl__1 (castNlpBuilder x)


-- direct wrapper
foreign import ccall unsafe "casadi__NlpBuilder__type_name" c_casadi__NlpBuilder__type_name
  :: Ptr (Ptr StdString) -> Ptr NlpBuilder' -> IO (Ptr StdString)

casadi__NlpBuilder__type_name
  :: NlpBuilder -> IO String
casadi__NlpBuilder__type_name x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__NlpBuilder__type_name errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
nlpBuilder_type_name :: NlpBuilderClass a => a -> IO String
nlpBuilder_type_name x = casadi__NlpBuilder__type_name (castNlpBuilder x)

