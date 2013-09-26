{-# OPTIONS_GHC -Wall #-}
{-# Language OverloadedStrings #-}

module WriteCasadiBindings.TypeMaps ( hsType
                                    , hsTypePrim
                                    , ffiType
                                    , ffiTypeTV
                                    , ffiTypePrim
                                    , cppType
                                    , cppTypeTV
                                    , cppTypePrim
                                    , cWrapperType
                                    , cWrapperRetType
                                      -- * bonus stuff, doesn't belong here really
                                    , cppMarshalType
                                    , cppClassName
                                    , cWrapperName
                                    , cWrapperName'
                                    , cppMethodName
                                    , cType
                                    , writeReturn
                                    , deleteName
                                    , makesNewRef
                                    ) where

import qualified Data.Text as T
import WriteCasadiBindings.CasadiClasses ( cppTypeCasadiPrim )
import WriteCasadiBindings.Types

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
hsTypePrim StdOstream = "String"
hsTypePrim CBool = "Bool"
hsTypePrim CVoid = "()"
hsTypePrim CSize = "CSize"
hsTypePrim CUChar = "CUChar"
hsTypePrim CLong = "Int"

hsTypePrim (CasadiClass x) = show x

---- haskell type that appears in foreign import
ffiType :: Bool -> Type -> String
ffiType p (Val x) = ffiTypeTV p x
ffiType p (Ref x) = ffiTypeTV p x
ffiType p (ConstRef x) = ffiTypeTV p x

ffiTypeTV :: Bool -> ThreeVectors -> String
ffiTypeTV p (NonVec x) = ffiTypePrim p x
ffiTypeTV p (Vec (NonVec x)) =
  maybeParens p $ "Ptr (CppVec " ++ ffiTypePrim True x ++ ")"
ffiTypeTV p (Vec (Vec (NonVec x))) =
  maybeParens p $ "Ptr (CppVecVec " ++ ffiTypePrim True x ++ ")"
ffiTypeTV p (Vec (Vec (Vec (NonVec x)))) =
  maybeParens p $ "Ptr (CppVecVecVec " ++ ffiTypePrim True x ++ ")"
ffiTypeTV _ (Vec (Vec (Vec (Vec ())))) = error $ "ffiTypeTV: Vec (Vec (Vec (Vec ())))"

ffiTypePrim :: Bool -> Primitive -> String
ffiTypePrim _ CInt = "CInt"
ffiTypePrim _ CDouble = "CDouble"
ffiTypePrim _ CVoid = "()"
ffiTypePrim _ CSize = "CSize"
ffiTypePrim _ CLong = "CLong"
ffiTypePrim _ CUChar = "CUChar"
ffiTypePrim p CBool = maybeParens p $ "Ptr CppBool'"
ffiTypePrim p StdString = maybeParens p $ "Ptr StdString'"
ffiTypePrim p StdOstream = maybeParens p $ "Ptr StdOstream'"
ffiTypePrim p (CasadiClass x) = maybeParens p $ "Ptr " ++ show x ++ raw

-- type which appears in the casadi library
cppType :: Type -> String
cppType (Val x) = cppTypeTV x
cppType (Ref x) = cppTypeTV x ++ "&"
cppType (ConstRef x) = cppTypeTV x ++ " const &"

cppTypeTV :: ThreeVectors -> String
cppTypeTV (NonVec x) = cppTypePrim x
cppTypeTV (Vec (NonVec x)) =
  "std::vector<"++cppTypePrim x ++ " >"
cppTypeTV (Vec (Vec (NonVec x))) =
  "std::vector<std::vector<"++cppTypePrim x ++ " > >"
cppTypeTV (Vec (Vec (Vec (NonVec x)))) =
  "std::vector<std::vector<std::vector<"++cppTypePrim x ++ " > > >"
cppTypeTV (Vec (Vec (Vec (Vec ())))) = error $ "cppTypeTV: Vec (Vec (Vec (Vec ())))"

cppTypePrim :: Primitive -> String
cppTypePrim CInt = "int"
cppTypePrim CDouble = "double"
cppTypePrim StdString = "std::string"
cppTypePrim CBool = "bool"
cppTypePrim CVoid = "void"
cppTypePrim CSize = "size_t"
cppTypePrim CLong = "long"
cppTypePrim CUChar = "unsigned char"
cppTypePrim StdOstream = "std::ostream"
cppTypePrim (CasadiClass x) = cppTypeCasadiPrim x

-- type which appears in the C++ wrapper
cWrapperType :: Type -> String
cWrapperType (Val x@(NonVec (CasadiClass _))) = cWrapperTypeTV x ++ "*"
cWrapperType (Val x@(NonVec _)) = cWrapperTypeTV x
cWrapperType (Val x) = cWrapperTypeTV x ++ "*"
cWrapperType (Ref x) = cWrapperTypeTV x ++ "&"
cWrapperType (ConstRef x) = cWrapperTypeTV x ++ " const &"

cWrapperTypeTV :: ThreeVectors -> String
cWrapperTypeTV (NonVec x) = cWrapperTypePrim x
cWrapperTypeTV (Vec (NonVec x)) =
  "std::vector<"++cWrapperTypePrim x ++ "*>"
cWrapperTypeTV (Vec (Vec (NonVec x))) =
  "std::vector<std::vector<"++cWrapperTypePrim x ++ "*> >"
cWrapperTypeTV (Vec (Vec (Vec (NonVec x)))) =
  "std::vector<std::vector<std::vector<"++cWrapperTypePrim x ++ "*> > >"
cWrapperTypeTV (Vec (Vec (Vec (Vec ())))) = error $ "cWrapperTypeTV: Vec (Vec (Vec (Vec ())))"

cWrapperTypePrim :: Primitive -> String
cWrapperTypePrim StdString = "char*"
cWrapperTypePrim x = cppTypePrim x

-- output type of the cpp marshal function, usually same as cppType except for references
cppMarshalType :: Type -> String
cppMarshalType (Ref (NonVec x)) = cppTypePrim x ++ "&"
cppMarshalType (Ref x) = cppTypeTV x
cppMarshalType (ConstRef (NonVec x)) = "const " ++ cppTypePrim x
cppMarshalType (ConstRef x) = "const " ++ cppTypeTV x
cppMarshalType (Val x) = cppTypeTV x

cppClassName :: CasadiClass -> String
cppClassName = cppTypePrim . CasadiClass

cType :: Type -> String
cType = toCName . cppType

cWrapperName' :: Function -> String
cWrapperName' (Function (Name functionName) _ _) = toCName functionName

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
toCName cppName = T.unpack (replaces replacements (T.pack cppName))
  where
    replacements = [(":","_"),(" >","_"),("< ","_"),("<","_"),(">","_"),("'","_TIC"),(" ==","_equals"),(" !=","_nequals"),(" +","_plus"),(" *","_mul"),(" -","_minus")]

    replaces :: [(T.Text,T.Text)] -> T.Text -> T.Text
    replaces ((find',replace'):xs) = replaces xs . T.replace find' replace'
    replaces [] = id

cWrapperRetType :: Type -> String
cWrapperRetType (Val (NonVec (CasadiClass cc))) = cppClassName cc ++ "*"
cWrapperRetType (Val (NonVec x@StdString)) = cppTypePrim x ++ "*"
cWrapperRetType (Val (NonVec x@CBool)) = cppTypePrim x ++ "*"
cWrapperRetType (Val (NonVec x)) = cppTypePrim x
cWrapperRetType (Val x) = cppTypeTV x ++ "*"
cWrapperRetType (Ref x) = cppTypeTV x ++ "*"
cWrapperRetType (ConstRef x) = cppTypeTV x ++ " const *"

writeReturn :: Type -> String -> String
writeReturn t x = case makesNewRef t of
  Nothing -> "    return " ++ maybeAddress x ++ ";"
  Just (NonVec (CasadiClass cc)) ->
    "    return new " ++ cppClassName cc ++ "( " ++ x ++ " );"
  Just v ->
    "    return new " ++ cppTypeTV v ++ "( " ++ x ++ " );"
  where
    maybeAddress y = case t of
      Val _ -> y
      Ref _ -> "&( " ++ y ++ " )"
      ConstRef _ -> "&( " ++ y ++ " )"

makesNewRef :: Type -> Maybe ThreeVectors
makesNewRef (Val v@(NonVec (CasadiClass _))) = Just v
makesNewRef (Val v@(NonVec StdString)) = Just v
makesNewRef (Val v@(NonVec CBool)) = Just v
makesNewRef (Val (NonVec _)) = Nothing
makesNewRef (Val v) = Just v
makesNewRef (Ref v) = Just v
makesNewRef (ConstRef v) = Just v

deleteName :: ThreeVectors -> String
deleteName v = "delete_" ++ toCName (cppTypeTV v)
