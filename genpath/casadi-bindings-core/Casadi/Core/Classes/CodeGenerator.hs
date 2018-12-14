{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# Language ForeignFunctionInterface #-}
{-# Language FlexibleInstances #-}
{-# Language MultiParamTypeClasses #-}

module Casadi.Core.Classes.CodeGenerator
       (
         CodeGenerator,
         CodeGeneratorClass(..),
         codeGenerator__0,
         codeGenerator__1,
         codeGenerator_add__0,
         codeGenerator_add__1,
         codeGenerator_add_include__0,
         codeGenerator_add_include__1,
         codeGenerator_add_include__2,
         codeGenerator_dump,
         codeGenerator_generate__0,
         codeGenerator_generate__1,
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
foreign import ccall unsafe "casadi__CodeGenerator__CONSTRUCTOR__0" c_casadi__CodeGenerator__CONSTRUCTOR__0
  :: Ptr (Ptr StdString) -> Ptr StdString -> IO (Ptr CodeGenerator')

casadi__CodeGenerator__CONSTRUCTOR__0
  :: String -> IO CodeGenerator
casadi__CodeGenerator__CONSTRUCTOR__0 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__CodeGenerator__CONSTRUCTOR__0 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
codeGenerator__0 :: String -> IO CodeGenerator
codeGenerator__0 = casadi__CodeGenerator__CONSTRUCTOR__0


-- direct wrapper
foreign import ccall unsafe "casadi__CodeGenerator__CONSTRUCTOR__1" c_casadi__CodeGenerator__CONSTRUCTOR__1
  :: Ptr (Ptr StdString) -> Ptr StdString -> Ptr (StdMap StdString (Ptr GenericType')) -> IO (Ptr CodeGenerator')

casadi__CodeGenerator__CONSTRUCTOR__1
  :: String -> M.Map String GenericType -> IO CodeGenerator
casadi__CodeGenerator__CONSTRUCTOR__1 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__CodeGenerator__CONSTRUCTOR__1 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
codeGenerator__1 :: String -> M.Map String GenericType -> IO CodeGenerator
codeGenerator__1 = casadi__CodeGenerator__CONSTRUCTOR__1


-- direct wrapper
foreign import ccall unsafe "casadi__CodeGenerator__add__0" c_casadi__CodeGenerator__add__0
  :: Ptr (Ptr StdString) -> Ptr CodeGenerator' -> Ptr Function' -> IO ()

casadi__CodeGenerator__add__0
  :: CodeGenerator -> Function -> IO ()
casadi__CodeGenerator__add__0 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__CodeGenerator__add__0 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ()



-- classy wrapper
codeGenerator_add__0 :: CodeGeneratorClass a => a -> Function -> IO ()
codeGenerator_add__0 x = casadi__CodeGenerator__add__0 (castCodeGenerator x)


-- direct wrapper
foreign import ccall unsafe "casadi__CodeGenerator__add__1" c_casadi__CodeGenerator__add__1
  :: Ptr (Ptr StdString) -> Ptr CodeGenerator' -> Ptr Function' -> CInt -> IO ()

casadi__CodeGenerator__add__1
  :: CodeGenerator -> Function -> Bool -> IO ()
casadi__CodeGenerator__add__1 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__CodeGenerator__add__1 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ()



-- classy wrapper
codeGenerator_add__1 :: CodeGeneratorClass a => a -> Function -> Bool -> IO ()
codeGenerator_add__1 x = casadi__CodeGenerator__add__1 (castCodeGenerator x)


-- direct wrapper
foreign import ccall unsafe "casadi__CodeGenerator__add_include__0" c_casadi__CodeGenerator__add_include__0
  :: Ptr (Ptr StdString) -> Ptr CodeGenerator' -> Ptr StdString -> IO ()

casadi__CodeGenerator__add_include__0
  :: CodeGenerator -> String -> IO ()
casadi__CodeGenerator__add_include__0 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__CodeGenerator__add_include__0 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ()



-- classy wrapper
codeGenerator_add_include__0 :: CodeGeneratorClass a => a -> String -> IO ()
codeGenerator_add_include__0 x = casadi__CodeGenerator__add_include__0 (castCodeGenerator x)


-- direct wrapper
foreign import ccall unsafe "casadi__CodeGenerator__add_include__1" c_casadi__CodeGenerator__add_include__1
  :: Ptr (Ptr StdString) -> Ptr CodeGenerator' -> Ptr StdString -> CInt -> IO ()

casadi__CodeGenerator__add_include__1
  :: CodeGenerator -> String -> Bool -> IO ()
casadi__CodeGenerator__add_include__1 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__CodeGenerator__add_include__1 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ()



-- classy wrapper
codeGenerator_add_include__1 :: CodeGeneratorClass a => a -> String -> Bool -> IO ()
codeGenerator_add_include__1 x = casadi__CodeGenerator__add_include__1 (castCodeGenerator x)


-- direct wrapper
foreign import ccall unsafe "casadi__CodeGenerator__add_include__2" c_casadi__CodeGenerator__add_include__2
  :: Ptr (Ptr StdString) -> Ptr CodeGenerator' -> Ptr StdString -> CInt -> Ptr StdString -> IO ()

casadi__CodeGenerator__add_include__2
  :: CodeGenerator -> String -> Bool -> String -> IO ()
casadi__CodeGenerator__add_include__2 x0 x1 x2 x3 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__CodeGenerator__add_include__2 errStrPtrP x0' x1' x2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'

  return ()



-- classy wrapper
codeGenerator_add_include__2 :: CodeGeneratorClass a => a -> String -> Bool -> String -> IO ()
codeGenerator_add_include__2 x = casadi__CodeGenerator__add_include__2 (castCodeGenerator x)


-- direct wrapper
foreign import ccall unsafe "casadi__CodeGenerator__dump" c_casadi__CodeGenerator__dump
  :: Ptr (Ptr StdString) -> Ptr CodeGenerator' -> IO (Ptr StdString)

casadi__CodeGenerator__dump
  :: CodeGenerator -> IO String
casadi__CodeGenerator__dump x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__CodeGenerator__dump errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
codeGenerator_dump :: CodeGeneratorClass a => a -> IO String
codeGenerator_dump x = casadi__CodeGenerator__dump (castCodeGenerator x)


-- direct wrapper
foreign import ccall unsafe "casadi__CodeGenerator__generate__0" c_casadi__CodeGenerator__generate__0
  :: Ptr (Ptr StdString) -> Ptr CodeGenerator' -> IO (Ptr StdString)

casadi__CodeGenerator__generate__0
  :: CodeGenerator -> IO String
casadi__CodeGenerator__generate__0 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__CodeGenerator__generate__0 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
codeGenerator_generate__0 :: CodeGeneratorClass a => a -> IO String
codeGenerator_generate__0 x = casadi__CodeGenerator__generate__0 (castCodeGenerator x)


-- direct wrapper
foreign import ccall unsafe "casadi__CodeGenerator__generate__1" c_casadi__CodeGenerator__generate__1
  :: Ptr (Ptr StdString) -> Ptr CodeGenerator' -> Ptr StdString -> IO (Ptr StdString)

casadi__CodeGenerator__generate__1
  :: CodeGenerator -> String -> IO String
casadi__CodeGenerator__generate__1 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__CodeGenerator__generate__1 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
codeGenerator_generate__1 :: CodeGeneratorClass a => a -> String -> IO String
codeGenerator_generate__1 x = casadi__CodeGenerator__generate__1 (castCodeGenerator x)

