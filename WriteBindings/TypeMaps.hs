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
       ) where

import qualified Data.Text as T
import WriteBindings.Buildbot.CasadiClasses ( cppTypeCasadiPrimClass, cppTypeCasadiPrimEnum )
import WriteBindings.Types

raw :: String
raw = "'"

maybeParens :: Bool -> String -> String
maybeParens False x = x
maybeParens True x = "(" ++ x ++ ")"

-- haskell type which end-user sees
hsType :: Bool -> Type -> String
hsType _ CInt = "Int"
hsType _ CDouble = "Double"
hsType _ StdString = "String"
hsType _ StdOstream = "String"
hsType _ CBool = "Bool"
hsType _ CVoid = "()"
hsType _ CSize = "CSize"
hsType _ CUChar = "CUChar"
hsType _ CLong = "Int"
hsType _ (CasadiClass x) = show x
hsType _ (CasadiEnum x) = show x
hsType p (StdVec x) = maybeParens p ("Vector " ++ hsType True x)
hsType p (Ref x) = hsType p x
hsType p (ConstRef x) = hsType p x

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
ffiType _ CBool = "CInt"
ffiType p StdString = maybeParens p $ "Ptr StdString'"
ffiType p StdOstream = maybeParens p $ "Ptr StdOstream'"
ffiType p (CasadiClass x) = maybeParens p $ "Ptr " ++ show x ++ raw
ffiType _ (CasadiEnum _) = "CInt"
ffiType p (StdVec x) = maybeParens p $ "Ptr (CppVec " ++ ffiType True x ++ ")"
ffiType p (Ref x) = ffiType p x
ffiType p (ConstRef x) = ffiType p x

-- the C++ syntax
cppType :: Type -> String
cppType CInt = "int"
cppType CDouble = "double"
cppType StdString = "std::string"
cppType CBool = "bool"
cppType CVoid = "void"
cppType CSize = "size_t"
cppType CLong = "long"
cppType CUChar = "unsigned char"
cppType StdOstream = "std::ostream"
cppType (CasadiClass x) = cppTypeCasadiPrimClass x
cppType (CasadiEnum x) = cppTypeCasadiPrimEnum x
cppType (StdVec x) = "std::vector< " ++ cppType x ++ " >"
cppType (Ref x) = cppType x ++ "&"
--cppType (ConstRef x) = cppType x ++ " &"
cppType (ConstRef x) = cppType x ++ " const &"

-- type C type which the FFI uses
cWrapperType :: Type -> String
cWrapperType CInt = "int"
cWrapperType CDouble = "double"
cWrapperType CBool = "int"
cWrapperType CVoid = "void"
cWrapperType CSize = "size_t"
cWrapperType CLong = "long"
cWrapperType CUChar = "unsigned char"
cWrapperType (CasadiEnum x) = cppTypeCasadiPrimEnum x
cWrapperType (CasadiClass x) = cppTypeCasadiPrimClass x ++ "*"
cWrapperType StdString = "std::string*"
cWrapperType StdOstream = "std::ostream*"
cWrapperType (StdVec x) = "std::vector< " ++ cWrapperType x ++ " >*"
--cWrapperType (Ref x) = cWrapperType x ++ "*"
--cWrapperType (ConstRef x) = cWrapperType x ++ " const *"
cWrapperType (Ref x)
  | usedAsPtr x = cWrapperType x
  | otherwise = cWrapperType x ++ "*"
cWrapperType (ConstRef x)
  | usedAsPtr x = cWrapperType x
--  | otherwise = cWrapperType x ++ " *"
  | otherwise = cWrapperType x ++ " const *"

usedAsPtr :: Type -> Bool
usedAsPtr (CasadiClass _) = True
usedAsPtr (StdString) = True
usedAsPtr (StdOstream) = True
usedAsPtr (StdVec _) = True
usedAsPtr _ = False


---- output type of the cpp marshal function, usually same as cppType except for references
cppMarshalType :: Type -> String
cppMarshalType (ConstRef x@(StdVec _)) = cppType x
cppMarshalType (Ref x@(StdVec _)) = cppType x
cppMarshalType x = cppType x

cppClassName :: CasadiClass -> String
cppClassName = cppType . CasadiClass

cType :: Type -> String
cType = toCName . cppType

cWrapperName' :: Function -> String
cWrapperName' (Function (Name functionName) _ _ _) = toCName functionName

cWrapperName :: CasadiClass -> Method -> String
cWrapperName classType fcn = case fMethodType fcn of
  Constructor -> toCName (cppClassName classType ++ "::" ++ methodName)
    where
      Name methodName = fName fcn
  _ -> toCName (cppMethodName classType fcn)


-- the thing to call in casadi, like CasADi::SXFunction::jac
cppMethodName :: CasadiClass -> Method -> String
cppMethodName classType fcn = case fMethodType fcn of
  Constructor -> cppClassName classType
  _ -> cppClassName classType ++ "::" ++ methodName
  where
    Name methodName = fName fcn

toCName :: String -> String
toCName cppName = replaces replacements cppName
  where
    replacements = [(":","_"),(" >","_"),("< ","_"),("<","_"),(">","_"),("'","_TIC"),(" ==","_equals"),(" !=","_nequals"),(" +","_plus"),(" *","_mul"),(" -","_minus")]

replaces :: [(String,String)] -> String -> String
replaces ((find',replace'):xs) = replaces xs . T.unpack . T.replace (T.pack find') (T.pack replace') . T.pack
replaces [] = id

cWrapperRetType :: Type -> String
cWrapperRetType = cWrapperType
--cWrapperRetType (Val (NonVec x)) = cppTypePrim x ++ (if usedAsPtr x then "*" else "")
--cWrapperRetType (Val x) = cppTypeTV x ++ "*"
--cWrapperRetType (Ref x) = cppTypeTV x ++ "*"
--cWrapperRetType (ConstRef x) = cppTypeTV x ++ " const *"

writeReturn :: Type -> String -> String
writeReturn CVoid x = "    " ++ x ++ ";"
writeReturn (Ref _) _ = error "writeReturn: can't return a reference"
writeReturn (ConstRef _) _ = error "writeReturn: can't return a const reference"
writeReturn t x =
  "    return WrapReturn< " ++ cWrapperType t ++ ", " ++ cppType t ++ " >::wrapReturn( " ++ x ++ " );"

deleteName :: Type -> String
deleteName v = "delete_" ++ replaces [("&","")] (toCName (cppType v))

hsDataName :: CasadiClass -> String
hsDataName classType = hsType False (CasadiClass classType)

hsClassName :: CasadiClass -> String
hsClassName classType = hsDataName classType  ++ "Class"
