{-# OPTIONS_GHC -Wall #-}

module WriteBindings.TypeMaps
       ( hsType
       , ffiType
       , cppType
       , cWrapperType
       , cWrapperRetType
       , cppMarshalType
       , cppClassName
       , cWrapperName
       , cWrapperName'
       , cppMethodName
       , cType
       , writeReturn
       , deleteName
       , hsDataName
       , hsClassName
       , toCName
       , replaces
       ) where

import Data.Char ( toUpper )
import qualified Data.Text as T
import Data.List ( intercalate )
import WriteBindings.ParseJSON

raw :: String
raw = "'"

maybeParens :: Bool -> String -> String
maybeParens False x = x
maybeParens True x = "(" ++ x ++ ")"

uppercase :: String -> String
uppercase (x:xs) = toUpper x : xs
uppercase [] = error "uppercase got empty string"

-- haskell type which end-user sees
hsType :: Bool -> Type -> String
hsType p (CArray t) = maybeParens p $ "Vector " ++ hsType True t
hsType _ (IOInterface t) = "IOInterface" ++ hsType False t
hsType _ (CEnum _ (Name n)) = uppercase n
hsType _ CInt = "Int"
hsType _ CDouble = "Double"
hsType _ StdString = "String"
hsType _ StdOstream = "String"
hsType _ CBool = "Bool"
hsType _ CVoid = "()"
hsType _ CSize = "CSize"
hsType _ CUChar = "CUChar"
hsType _ CChar = "CChar"
hsType _ CLong = "Int"
hsType _ (UserType _ (Name x)) = uppercase x
hsType p (StdVec x) = maybeParens p ("Vector " ++ hsType True x)
hsType p (Ref x) = hsType p x
hsType p (Pointer x) = hsType p x
hsType _ (IOSchemeVec {}) = error "hsType: IOSchemeVec undefined"
hsType _ (StdPair x y) = "(" ++ hsType False x ++ ", " ++ hsType False y ++ ")"
hsType p (Const x) = hsType p x

-- haskell type that appears in foreign import
-- this should never do anything except unwrap and pass the TV to ffiTypeTV
-- or c_deleteName will be wrong
ffiType :: Bool -> Type -> String
ffiType _ CInt = "CInt"
ffiType _ CDouble = "CDouble"
ffiType _ CVoid = "()"
ffiType _ CSize = "CSize"
ffiType _ CLong = "CLong"
ffiType _ CUChar = "CUChar"
ffiType _ CChar = "CChar"
ffiType _ CBool = "CInt"
ffiType _ (CEnum {}) = "CInt"
ffiType p (CArray t) = maybeParens p $ "Ptr " ++ ffiType True t
ffiType p StdString = maybeParens p $ "Ptr StdString"
ffiType p StdOstream = maybeParens p $ "Ptr StdOstream"
ffiType p (StdVec x) = maybeParens p $ "Ptr (StdVec " ++ ffiType True x ++ ")"
ffiType p (Ref x) = ffiType p x
ffiType p (Pointer x) = ffiType p x
--ffiType p (ConstRef x) = ffiType p x
ffiType p (UserType _ (Name x)) = maybeParens p $ "Ptr " ++ uppercase x ++ raw
ffiType _ (IOSchemeVec {}) = error "ffiType: IOSchemeVec undefined"
ffiType p (IOInterface x) = maybeParens p $ "Ptr IOInterface" ++ hsType False x ++ raw
ffiType p (StdPair x y) = maybeParens p $ "Ptr (StdPair " ++ ffiType True x ++ " " ++ ffiType True y ++ ")"
ffiType p (Const x) = ffiType p x

namespace :: Namespace -> Name -> String
namespace (Namespace ns) (Name x) = case intercalate "::" ns of
  [] -> x
  ns' -> ns' ++ "::" ++ x

-- the C++ syntax
cppType :: Type -> String
cppType CInt = "int"
cppType (CArray t) = cppType t ++ "[]"
cppType (CEnum ns t) = namespace ns t
cppType CDouble = "double"
cppType StdString = "std::string"
cppType CBool = "bool"
cppType CVoid = "void"
cppType CSize = "size_t"
cppType CLong = "long"
cppType CUChar = "unsigned char"
cppType CChar = "char"
cppType StdOstream = "std::ostream"
cppType (StdVec x) = "std::vector< " ++ cppType x ++ " >"
cppType (Ref x) = cppType x ++ "&"
cppType (Pointer x) = cppType x ++ "*"
cppType (UserType ns x) = namespace ns x
cppType (StdPair x y) = "std::pair< " ++ cppType x ++ ", " ++ cppType y ++ " >"
cppType (IOInterface x) = "casadi::IOInterface< " ++ cppType x ++ " >"
cppType (IOSchemeVec {}) = error "cppType: IOSchemeVec undefined"
cppType (Const x) = cppType x

-- type C type which the FFI uses
cWrapperType :: Type -> String
cWrapperType CInt = "int"
cWrapperType (CArray t) = cWrapperType t ++ "[]"
cWrapperType CDouble = "double"
cWrapperType CBool = "int"
cWrapperType (CEnum {}) = "int"
cWrapperType CVoid = "void"
cWrapperType CSize = "size_t"
cWrapperType CLong = "long"
cWrapperType CUChar = "unsigned char"
cWrapperType CChar = "char"
cWrapperType x@(UserType {}) = cppType x ++ "*"
cWrapperType (Const x) = cWrapperType x
cWrapperType StdString = "std::string*"
cWrapperType StdOstream = "std::ostream*"
cWrapperType (StdVec x) = "std::vector< " ++ cWrapperType x ++ " >*"
cWrapperType (Ref x)
  | addPtr x = cWrapperType x
  | otherwise = cWrapperType x ++ "*"
cWrapperType (Pointer x)
  | addPtr x = cWrapperType x
  | otherwise = cWrapperType x ++ "*"
cWrapperType (StdPair x y) = "std::pair< " ++ cWrapperType x ++ ", " ++ cWrapperType y ++ " >*"
cWrapperType (IOInterface x) = "casadi::IOInterface< " ++ cWrapperType x ++ " >*"
cWrapperType (IOSchemeVec {}) = error "cWrapperType: IOSchemeVec undefined"

addPtr :: Type -> Bool
addPtr (UserType {}) = True
addPtr (StdString) = True
addPtr (StdOstream) = True
addPtr (StdVec _) = True
addPtr (StdPair {}) = True
addPtr (IOInterface {}) = True
addPtr (Const x) = addPtr x
addPtr _ = False


---- output type of the cpp marshal function, usually same as cppType except for references
cppMarshalType :: Type -> String
cppMarshalType (Const x) = cppMarshalType x
cppMarshalType (Ref x) = cppMarshalType x
cppMarshalType x = cppType x

cppClassName :: Type -> String
cppClassName = cppType

cType :: Type -> String
cType = toCName . cppType

cWrapperName' :: CppFunction -> String
cWrapperName' f = case fOthers f of
  Nothing -> toCName (fName f)
  Just k -> toCName $ fName f ++ "__" ++ show k

cWrapperName :: Type -> Method -> String
cWrapperName t fcn = case mKind fcn of
  Constructor -> toCName (cppClassName t ++ "::" ++ name) ++ number
  _ -> toCName (cppMethodName t fcn) ++ number
  where
    Name name = mName fcn
    number = case mOthers fcn of
      Nothing -> ""
      Just k -> "__" ++ show k


-- the thing to call in casadi, like CasADi::SXFunction::jac
cppMethodName :: Type -> Method -> String
cppMethodName t fcn = case mKind fcn of
  Constructor -> cppClassName t
  _ -> cppClassName t ++ "::" ++ name
  where
    Name name = mName fcn

toCName :: String -> String
toCName cppName = replaces replacements cppName
  where
    replacements = [(":","_"),(" >","_"),("< ","_"),("<","_"),(">","_"),("'","_TIC"),(" ==","_equals"),(" !=","_nequals"),(" +","_plus"),(" *","_mul"),(" -","_minus"),(" ()","_call")]

replaces :: [(String,String)] -> String -> String
replaces ((find',replace'):xs) = replaces xs . T.unpack . T.replace (T.pack find') (T.pack replace') . T.pack
replaces [] = id

cWrapperRetType :: Type -> String
cWrapperRetType = cWrapperType

writeReturn :: Type -> String -> String
writeReturn CVoid x = "    " ++ x ++ ";"
writeReturn t x =
  init $ unlines
  [ "    " ++ cppType t ++ " ret = " ++ x ++ ";"
  , "    return WrapReturn< " ++ cWrapperType t ++ ", " ++ cppType t ++ " >::wrapReturn( ret );"
  ]

deleteName :: Type -> String
deleteName v = "delete_" ++ replaces [("&","")] (toCName (cppType v))

hsDataName :: Type -> String
hsDataName t = hsType False t

hsClassName :: Type -> String
hsClassName t = hsDataName t  ++ "Class"
