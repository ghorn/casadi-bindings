{-# OPTIONS_GHC -Wall #-}
{-# Language OverloadedStrings #-}

module Types ( Class(..)
             , Function(..)
             , Method(..)
             , Name(..)
             , Type(..)
             , MethodType(..)
             , Primitive(..)
             , Primitive'(..)
             , CasadiClass(..)
             , ThreeVectors
             , hsType
--             , hsType'
             , ffiType
--             , ffiType'
             , cppType
--             , cType
--             , toCName
             , cWrapperType
--             , cppMarshallType
             ) where

import qualified Data.Text as T

data Method = Method { fName :: Name
                     , fType :: Type
                     , fArgs :: [Type]
                     , fMethodType :: MethodType
                     } deriving Show
data MethodType = Constructor | Static | Normal deriving Show

data Class = Class CasadiClass [Method] deriving Show
data Function = Function Name Type [Type] deriving Show

data Name = Name String deriving Show

data CasadiClass = SXMatrix
                 | FX
                 | MX
                 | SXFunction
                 deriving Show

data Primitive = CInt
               | CDouble
               | StdString
               | CasadiClass CasadiClass
               deriving Show

data Primitive' a = NonVec Primitive
                  | Vec a
                  deriving Show

type ThreeVectors = (Primitive' (Primitive' (Primitive' (Primitive' ()))))

data Type = Val ThreeVectors
          | Ref ThreeVectors
          | ConstRef ThreeVectors
          deriving Show

--getMethods :: CasadiClass -> [Method]
--
--getConstructors :: CasadiClass -> [Constructor]
--
--allFunctions :: [Function]

raw :: String
raw = "'"

maybeParens :: Bool -> String -> String
maybeParens False x = x
maybeParens True x = "(" ++ x ++ ")"

-- haskell type which end-user sees
hsType :: Bool -> Type -> String
hsType p (Val x) = hsTypeTV p x
hsType p (Ref x) = hsTypeTV p x
hsType p (ConstRef x) = hsTypeTV p x

hsTypeTV :: Bool -> ThreeVectors -> String
hsTypeTV _ (NonVec x) = hsTypePrim x
hsTypeTV p (Vec (NonVec x)) =
  maybeParens p $ "Vector " ++ hsTypePrim x
hsTypeTV p (Vec (Vec (NonVec x))) =
  maybeParens p $ "Vector (Vector " ++ hsTypePrim x ++ ")"
hsTypeTV p (Vec (Vec (Vec (NonVec x)))) =
  maybeParens p $ "Vector (Vector (Vector " ++ hsTypePrim x ++ "))"
hsTypeTV _ (Vec (Vec (Vec (Vec ())))) = error $ "hsTypeTV: Vec (Vec (Vec (Vec ())))"

hsTypePrim :: Primitive -> String
hsTypePrim CInt = "Int"
hsTypePrim CDouble = "Double"
hsTypePrim StdString = "String"
hsTypePrim (CasadiClass x) = show x

---- haskell type that appears in foreign import
ffiType :: Bool -> Type -> String
ffiType p (Val x) = ffiTypeTV p x
ffiType p (Ref x) = ffiTypeTV p x
ffiType p (ConstRef x) = ffiTypeTV p x

ffiTypeTV :: Bool -> ThreeVectors -> String
ffiTypeTV p (NonVec x) = ffiTypePrim p x
ffiTypeTV p (Vec (NonVec x)) =
  maybeParens p $ "Ptr " ++ ffiTypePrim True x
ffiTypeTV p (Vec (Vec (NonVec x))) =
  maybeParens p $ "Ptr (Ptr " ++ ffiTypePrim True x ++ ")"
ffiTypeTV p (Vec (Vec (Vec (NonVec x)))) =
  maybeParens p $ "Ptr (Ptr (Ptr " ++ ffiTypePrim True x ++ "))"
ffiTypeTV _ (Vec (Vec (Vec (Vec ())))) = error $ "ffiTypeTV: Vec (Vec (Vec (Vec ())))"

ffiTypePrim :: Bool -> Primitive -> String
ffiTypePrim _ CInt = "CInt"
ffiTypePrim _ CDouble = "CDouble"
ffiTypePrim _ StdString = "CString"
ffiTypePrim p (CasadiClass x) = maybeParens p $ "Ptr " ++ show x ++ raw

-- type which appears in the casadi library
cppType :: Type -> String
cppType (Val x) = cppTypeTV x
cppType (Ref x) = cppTypeTV x ++ "&"
cppType (ConstRef x) = "const " ++ cppTypeTV x ++ "&"

cppTypeTV :: ThreeVectors -> String
cppTypeTV (NonVec x) = cppTypePrim x
cppTypeTV (Vec (NonVec x)) =
  "std::vector<"++cppTypePrim x ++ ">"
cppTypeTV (Vec (Vec (NonVec x))) =
  "std::vector<std::vector<"++cppTypePrim x ++ ">>"
cppTypeTV (Vec (Vec (Vec (NonVec x)))) =
  "std::vector<std::vector<std::vector<"++cppTypePrim x ++ ">>>"
cppTypeTV (Vec (Vec (Vec (Vec ())))) = error $ "cppTypeTV: Vec (Vec (Vec (Vec ())))"

cppTypePrim :: Primitive -> String
cppTypePrim CInt = "int"
cppTypePrim CDouble = "double"
cppTypePrim StdString = "std::string"
cppTypePrim (CasadiClass MX) = "CasADi::MX"
cppTypePrim (CasadiClass FX) = "CasADi::MX"
cppTypePrim (CasadiClass SXMatrix) = "CasADi::SXMatrix"
cppTypePrim (CasadiClass SXFunction) = "CasADi::SXFunction"

-- type which appears in the C++ wrapper
cWrapperType :: Type -> String
cWrapperType (Val x) = cWrapperTypeTV x ++ "*"
cWrapperType (Ref x) = cWrapperTypeTV x ++ "&"
cWrapperType (ConstRef x) = "const " ++ cWrapperTypeTV x ++ "&"

cWrapperTypeTV :: ThreeVectors -> String
cWrapperTypeTV (NonVec x) = cWrapperTypePrim x
cWrapperTypeTV (Vec (NonVec x)) =
  "std::vector<"++cWrapperTypePrim x ++ "*>"
cWrapperTypeTV (Vec (Vec (NonVec x))) =
  "std::vector<std::vector<"++cWrapperTypePrim x ++ "*>>"
cWrapperTypeTV (Vec (Vec (Vec (NonVec x)))) =
  "std::vector<std::vector<std::vector<"++cWrapperTypePrim x ++ "*>>>"
cWrapperTypeTV (Vec (Vec (Vec (Vec ())))) = error $ "cWrapperTypeTV: Vec (Vec (Vec (Vec ())))"

cWrapperTypePrim :: Primitive -> String
cWrapperTypePrim StdString = "char *"
cWrapperTypePrim x = cppTypePrim x


--cppMarshallType :: Type -> String
--cppMarshallType (Ref x) = cppType (Prim x)
--cppMarshallType (ConstRef x) = cppType (Prim x)
--cppMarshallType x = cppType x
--
--cppType :: Type -> String
--cppType (Prim (SP CInt)) = "int"
--cppType (Prim (SP CDouble)) = "double"
--cppType (Prim (SP StdString)) = "std::string"
--cppType (Prim (CP (Vec x))) = "std::vector<" ++ cppType (Prim x) ++ ">"
--cppType (Prim (CP x)) = casadiNs (show x)
--cppType (Ptr x) = cppType (Prim x) ++ "*"
--cppType (Ref x) = cppType (Prim x) ++ "&"
--cppType (ConstRef x) = cppType (Prim x) ++ " const &"
--
--cType :: Type -> String
--cType = toCName . cppType
--
--toCName :: String -> String
--toCName cppName = T.unpack (replaces replacements (T.pack cppName))
--  where
--    replacements = [(":","_"),(" >","_"),("< ","_"),("<","_"),(">","_")]
--
--    replaces :: [(T.Text,T.Text)] -> T.Text -> T.Text
--    replaces ((find',replace'):xs) = replaces xs . T.replace find' replace'
--    replaces [] = id
