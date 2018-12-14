{-# OPTIONS_GHC -Wall -Werror #-}

module WriteBindings.TypeMaps
       ( hsType
       , ffiType
       , cppType
       , cWrapperType
       , cWrapperRetType
       , cppMarshalType
       , cppEmptySwigOutputType
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
hsType ::
  Bool -- ^ parens
  -> Type -- ^ the type, duh
  -> String -- ^ string for the generated haskell type
hsType p (CArray t) = maybeParens p $ "Vector " ++ hsType True t
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
hsType _ CLongLong = "Int"
hsType _ (UserType _ (Name x)) = uppercase x
hsType p (StdVec x) = maybeParens p ("Vector " ++ hsType True x)
hsType p (Ref x) = hsType p x
hsType p (Pointer x) = hsType p x
hsType _ (StdPair x y) = "(" ++ hsType False x ++ ", " ++ hsType False y ++ ")"
hsType p (StdMap x y) = maybeParens p $ "M.Map " ++ hsType True x ++ " " ++ hsType True y
hsType p (Const x) = hsType p x
hsType _ (PrintableObject _) = error "PrintableObject should be internal"

-- haskell type that appears in foreign import
-- this should never do anything except unwrap and pass the TV to ffiTypeTV
-- or c_deleteName will be wrong
ffiType :: Bool -> (Type, SwigOutput) -> String
ffiType p' (t', SwigOutput swigOutput)
  | swigOutput = "Ptr " ++ ffiType' True t'
  | otherwise = ffiType' p' t'
  where
    ffiType' _ CInt = "CInt"
    ffiType' _ CDouble = "CDouble"
    ffiType' _ CVoid = "()"
    ffiType' _ CSize = "CSize"
    ffiType' _ CLong = "CLong"
    ffiType' _ CLongLong = "CLLong"
    ffiType' _ CUChar = "CUChar"
    ffiType' _ CChar = "CChar"
    ffiType' _ CBool = "CInt"
    ffiType' _ (CEnum {}) = "CInt"
    ffiType' p (CArray t) = maybeParens p $ "Ptr " ++ ffiType' True t
    ffiType' p StdString = maybeParens p $ "Ptr StdString"
    ffiType' p StdOstream = maybeParens p $ "Ptr StdOstream"
    ffiType' p (StdVec x) = maybeParens p $ "Ptr (StdVec " ++ ffiType' True x ++ ")"
    ffiType' p (Ref x) = ffiType' p x
    ffiType' p (Pointer x) = ffiType' p x
    --ffiType' p (ConstRef x) = ffiType' p x
    ffiType' p (UserType _ (Name x)) = maybeParens p $ "Ptr " ++ uppercase x ++ raw
    ffiType' p (StdPair x y) = maybeParens p $ "Ptr (StdPair " ++ ffiType' True x ++ " " ++ ffiType' True y ++ ")"
    ffiType' p (StdMap StdString y) = maybeParens p $ "Ptr (StdMap StdString " ++ ffiType' True y ++ ")"
    ffiType' p (StdMap x y) = maybeParens p $ "Ptr (StdMap " ++ ffiType' True x ++ " " ++ ffiType' True y ++ ")"
    ffiType' p (Const x) = ffiType' p x
    ffiType' _ (PrintableObject _) = error "PrintableObject should be internal"

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
cppType CLongLong = "long long"
cppType CUChar = "unsigned char"
cppType CChar = "char"
cppType StdOstream = "std::ostream"
cppType (StdVec x) = "std::vector< " ++ cppType x ++ " >"
cppType (Ref x) = cppType x ++ "&"
cppType (Pointer x) = cppType x ++ "*"
cppType (UserType ns x) = namespace ns x
cppType (StdPair x y) = "std::pair< " ++ cppType x ++ ", " ++ cppType y ++ " >"
cppType (StdMap x y) = "std::map< " ++ cppType x ++ ", " ++ cppType y ++ " >"
cppType (Const x) = cppType x
cppType (PrintableObject _) = error "PrintableObject should be internal"

-- type C type which the FFI uses
cWrapperType :: (Type, SwigOutput) -> String
cWrapperType (t', SwigOutput swigOutput)
  | swigOutput = cWrapperType' (Pointer t')
  | otherwise = cWrapperType' t'
  where
    cWrapperType' CInt = "int"
    cWrapperType' (CArray t) = cWrapperType' t ++ "[]"
    cWrapperType' CDouble = "double"
    cWrapperType' CBool = "int"
    cWrapperType' (CEnum {}) = "int"
    cWrapperType' CVoid = "void"
    cWrapperType' CSize = "size_t"
    cWrapperType' CLong = "long"
    cWrapperType' CLongLong = "long long"
    cWrapperType' CUChar = "unsigned char"
    cWrapperType' CChar = "char"
    cWrapperType' x@(UserType {}) = cppType x ++ "*"
    cWrapperType' (Const x) = cWrapperType' x
    cWrapperType' StdString = "std::string*"
    cWrapperType' StdOstream = "std::ostream*"
    cWrapperType' (StdVec x) = "std::vector< " ++ cWrapperType' x ++ " >*"
    cWrapperType' (Ref x)
      | addPtr x = cWrapperType' x
      | otherwise = cWrapperType' x ++ "*"
    cWrapperType' (Pointer x)
      | addPtr x = cWrapperType' x
      | otherwise = cWrapperType' x ++ "*"
    cWrapperType' (StdPair x y) = "std::pair< " ++ cWrapperType' x ++ ", " ++ cWrapperType' y ++ " >*"
    cWrapperType' (StdMap StdString y) = "std::map< std::string, " ++ cWrapperType' y ++ " >*"
    cWrapperType' (StdMap x y) = "std::map< " ++ cWrapperType' x ++ ", " ++ cWrapperType' y ++ " >*"
    cWrapperType' (PrintableObject _) = error "PrintableObject should be internal"

addPtr :: Type -> Bool
addPtr (UserType {}) = True
addPtr (StdString) = True
addPtr (StdOstream) = True
addPtr (StdVec _) = True
addPtr (StdPair {}) = True
addPtr (StdMap {}) = True
addPtr (Const x) = addPtr x
addPtr _ = False



---- output type of the cpp marshal function, usually same as cppType except for references
cppEmptySwigOutputType :: Type -> String
cppEmptySwigOutputType (Const x) = cppEmptySwigOutputType x
cppEmptySwigOutputType (Ref x@(StdPair {})) = cppEmptySwigOutputType x
cppEmptySwigOutputType (Ref (Const x@(StdPair {}))) = cppEmptySwigOutputType x
cppEmptySwigOutputType (Ref x@(StdMap {})) = cppEmptySwigOutputType x
cppEmptySwigOutputType (Ref (Const x@(StdMap {}))) = cppEmptySwigOutputType x
cppEmptySwigOutputType (Ref x@(StdVec {})) = cppEmptySwigOutputType x
cppEmptySwigOutputType (Ref (Const x@(StdVec {}))) = cppEmptySwigOutputType x
cppEmptySwigOutputType (Ref x@(CEnum {})) = cppEmptySwigOutputType x
cppEmptySwigOutputType (Ref (Const x@(CEnum {}))) = cppEmptySwigOutputType x
cppEmptySwigOutputType (Ref x) = cppEmptySwigOutputType x
cppEmptySwigOutputType x = cppType x



---- output type of the cpp marshal function, usually same as cppType except for references
cppMarshalType :: Type -> String
cppMarshalType (Const x) = cppMarshalType x
cppMarshalType (Ref x@(StdPair {})) = cppMarshalType x
cppMarshalType (Ref (Const x@(StdPair {}))) = cppMarshalType x
cppMarshalType (Ref x@(StdMap {})) = cppMarshalType x
cppMarshalType (Ref (Const x@(StdMap {}))) = cppMarshalType x
cppMarshalType (Ref x@(StdVec {})) = cppMarshalType x
cppMarshalType (Ref (Const x@(StdVec {}))) = cppMarshalType x
cppMarshalType (Ref x@(CEnum {})) = cppMarshalType x
cppMarshalType (Ref (Const x@(CEnum {}))) = cppMarshalType x
--cppMarshalType (Ref x) = cppMarshalType x
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
    replacements =
      [ (":","_"), (" >","_"), ("< ","_"), ("<","_"), (">","_"), ("'","_TIC")
      , ("==","_equals")
      , ("!=","_nequals")
      , ("+","_plus")
      , ("*","_mul")
      , ("-","_minus")
      , ("()","_call")
      , ("operator ","operator_")
      ]

replaces :: [(String,String)] -> String -> String
replaces ((find',replace'):xs) = replaces xs . T.unpack . T.replace (T.pack find') (T.pack replace') . T.pack
replaces [] = id

cWrapperRetType :: Type -> String
cWrapperRetType t = cWrapperType (t, SwigOutput False)

writeReturn :: Type -> String
writeReturn CVoid  =
  "        return;"
writeReturn t =
  "        return WrapReturn< " ++ cWrapperType (t, SwigOutput False) ++ ", " ++ cppType t ++ " >::wrapReturn( ret );"

deleteName :: Type -> String
deleteName v = "delete_" ++ replaces [("&","")] (toCName (cppType v))

hsDataName :: Type -> String
hsDataName t = hsType False t

hsClassName :: Type -> String
hsClassName t = hsDataName t  ++ "Class"
