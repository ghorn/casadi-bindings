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
         nlpBuilder_getDescription,
         nlpBuilder_getRepresentation,
         nlpBuilder_import_nl__0,
         nlpBuilder_import_nl__1,
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
foreign import ccall unsafe "casadi__NlpBuilder__getRepresentation" c_casadi__NlpBuilder__getRepresentation
  :: Ptr (Ptr StdString) -> Ptr NlpBuilder' -> IO (Ptr StdString)

casadi__NlpBuilder__getRepresentation
  :: NlpBuilder -> IO String
casadi__NlpBuilder__getRepresentation x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__NlpBuilder__getRepresentation errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
nlpBuilder_getRepresentation :: NlpBuilderClass a => a -> IO String
nlpBuilder_getRepresentation x = casadi__NlpBuilder__getRepresentation (castNlpBuilder x)


-- direct wrapper
foreign import ccall unsafe "casadi__NlpBuilder__getDescription" c_casadi__NlpBuilder__getDescription
  :: Ptr (Ptr StdString) -> Ptr NlpBuilder' -> IO (Ptr StdString)

casadi__NlpBuilder__getDescription
  :: NlpBuilder -> IO String
casadi__NlpBuilder__getDescription x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__NlpBuilder__getDescription errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
nlpBuilder_getDescription :: NlpBuilderClass a => a -> IO String
nlpBuilder_getDescription x = casadi__NlpBuilder__getDescription (castNlpBuilder x)

