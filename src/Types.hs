{-# OPTIONS_GHC -Wall #-}
{-# Language OverloadedStrings #-}

module Types ( Class(..)
             , Const(..)
             , Function(..)
             , Method(..)
             , Name(..)
             , Type(..)
             , RetType(..)
             , Static(..)
             , CasadiPrimitive(..)
             , SimplePrimitive(..)
             , Primitive(..)
             , hsType
             , hsType'
             , ffiType
             , ffiType'
             , cppType
             , cType
             , toCName
             , cWrapperType
             , cppMarshallType
             ) where

import qualified Data.Text as T

data Name = Name String deriving Show
data RetType = SimpleType Type
             | NewRef CasadiPrimitive deriving Show

data CasadiPrimitive = SXMatrix
                     | FX
                     | MX
                     | SXFunction
                     | Vec Primitive
                     deriving Show

data SimplePrimitive = CInt
                     | CDouble
                     | StdString
                     deriving Show

data Primitive = SP SimplePrimitive
               | CP CasadiPrimitive
               deriving Show

data Type = Prim Primitive
          | Ref Primitive
          | Ptr Primitive
          | ConstRef Primitive
          deriving Show

casadiNs :: String -> String
casadiNs = ("CasADi::" ++)

-- haskell type which end-user sees
hsType :: Type -> String
hsType = hsType' False

hsType' :: Bool -> Type -> String
hsType' _ (Prim (SP CInt)) = "Int"
hsType' _ (Prim (SP CDouble)) = "Double"
hsType' _ (Prim (SP StdString)) = "String"
hsType' _ (Prim (CP FX)) = "FX"
hsType' _ (Prim (CP SXFunction)) = "SXFunction"
hsType' _ (Prim (CP MX)) = "MX"
hsType' _ (Prim (CP SXMatrix)) = "SXMatrix"
hsType' p (Prim (CP (Vec x))) = maybeParens p $ "Vector " ++ hsType' True (Prim x)
hsType' p (Ref x) = hsType' p (Prim x)
hsType' p (Ptr x) = hsType' p (Prim x)
hsType' p (ConstRef x) = hsType' p (Prim x)

maybeParens :: Bool -> String -> String
maybeParens False x = x
maybeParens True x = "(" ++ x ++ ")"

-- haskell type that appears in foreign import
ffiType :: Type -> String
ffiType = ffiType' False

ffiType' :: Bool -> Type -> String
ffiType' _ (Prim (SP CInt)) = "CInt"
ffiType' _ (Prim (SP CDouble)) = "CDouble"
ffiType' _ (Prim (SP StdString)) = "CString"
ffiType' p (Prim (CP (Vec x))) = maybeParens p $ "Ptr " ++ ffiType' True (Prim x)
ffiType' p (Prim (CP x)) = maybeParens p $ "Ptr " ++ show x ++ "'"

ffiType' p (Ptr (CP (Vec x)))      = maybeParens p $ "Ptr " ++ ffiType' True (Ptr x)
ffiType' p (Ref (CP (Vec x)))      = maybeParens p $ "Ptr " ++ ffiType' True (Ref x)
ffiType' p (ConstRef (CP (Vec x))) = maybeParens p $ "Ptr " ++ ffiType' True (ConstRef x)

ffiType' p (Ptr (CP x)) = ffiType' p (Prim (CP x))
ffiType' p (Ref (CP x)) = ffiType' p (Prim (CP x))
ffiType' p (ConstRef (CP x)) = ffiType' p (Prim (CP x))

ffiType' p (Ptr (SP StdString)) = ffiType' p (Prim (SP StdString))
ffiType' p (Ref (SP StdString)) = ffiType' p (Prim (SP StdString))
ffiType' p (ConstRef (SP StdString)) = ffiType' p (Prim (SP StdString))

ffiType' p (Ptr x@(SP _)) = maybeParens p $ "Ptr " ++ ffiType' True (Prim x)
ffiType' p (Ref x@(SP _)) = maybeParens p $ "Ptr " ++ ffiType' True (Prim x)
ffiType' p (ConstRef x@(SP _)) = maybeParens p $ "Ptr " ++ ffiType' True (Prim x)

cWrapperType :: Type -> String
cWrapperType (Prim (SP CInt)) = "int"
cWrapperType (Prim (SP CDouble)) = "double"
cWrapperType (Prim (SP StdString)) = "char*"
cWrapperType (Prim (CP SXFunction)) = casadiNs "SXFunction"
cWrapperType (Prim (CP MX)) = casadiNs "MX"
cWrapperType (Prim (CP SXMatrix)) = casadiNs "SXMatrix"
cWrapperType (Prim (CP FX)) = casadiNs "FX"
cWrapperType (Ref (SP StdString)) = "char*"
cWrapperType (ConstRef (SP StdString)) = "char*"
cWrapperType (Prim (CP (Vec x))) = cWrapperType (Prim x) ++ "*"
cWrapperType (Ptr x) = cWrapperType (Prim x) ++ "*"
cWrapperType (Ref x) = cWrapperType (Prim x) ++ "&"
cWrapperType (ConstRef x) = cWrapperType (Prim x) ++ " const &"

cppMarshallType :: Type -> String
cppMarshallType (Ref x) = cppType (Prim x)
cppMarshallType (ConstRef x) = cppType (Prim x)
cppMarshallType x = cppType x

cppType :: Type -> String
cppType (Prim (SP CInt)) = "int"
cppType (Prim (SP CDouble)) = "double"
cppType (Prim (SP StdString)) = "std::string"
cppType (Prim (CP (Vec x))) = "std::vector<" ++ cppType (Prim x) ++ ">"
cppType (Prim (CP x)) = casadiNs (show x)
cppType (Ptr x) = cppType (Prim x) ++ "*"
cppType (Ref x) = cppType (Prim x) ++ "&"
cppType (ConstRef x) = cppType (Prim x) ++ " const &"

cType :: Type -> String
cType = toCName . cppType

toCName :: String -> String
toCName cppName = T.unpack (replaces replacements (T.pack cppName))
  where
    replacements = [(":","_"),(" >","_"),("< ","_"),("<","_"),(">","_")]

    replaces :: [(T.Text,T.Text)] -> T.Text -> T.Text
    replaces ((find',replace'):xs) = replaces xs . T.replace find' replace'
    replaces [] = id

data Method = Method { fName :: Name
                     , fType :: RetType
                     , fArgs :: [Type]
                     , fConst :: Const
                     , fStatic :: Static
                     } deriving Show

newtype Const = Const Bool deriving (Show, Eq)
newtype Static = Static Bool deriving (Show, Eq)

data Class = Class CasadiPrimitive [Method] deriving Show
data Function = Function Name RetType [Type] deriving Show
