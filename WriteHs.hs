{-# OPTIONS_GHC -Wall #-}

module WriteHs ( writeHsFunction, writeHsClass ) where

import Data.Char ( toLower )
import Data.List ( intersperse )
import Types
import WriteC ( toCName )
import CasadiTree



--foreign import ccall unsafe "sxm.hpp sxmDelete" c_sxmDelete :: Double -> IO ()
--foreign import ccall unsafe "sxm.hpp newInt"     c_sxmNewInt :: CInt -> IO (Ptr SXMRaw)

writeHsFunction :: Function -> String
writeHsFunction (Function (Name cppName) _ _) =
  "foreign import ccall unsafe \"" ++ cname ++ "\" " -- ++ hsname ++ " :: " ++ proto
  where
--    proto = cname ++ "(" ++ intercalate ", " (map (uncurry paramProto) args) ++ ")"
--    shownType = showType retType
--    hsname = "c_" ++ cname
    cname = toCName cppName
--    proto = concat $ intersperse " -> " (map hsType params ++ [hsType' retType])
    -- cname classname name args retType =

--foreign import ccall unsafe "sxm.hpp sxmDelete" c_sxmDelete :: Double -> IO ()

writeHsClass :: Class -> String
writeHsClass (Class (Name cppName) methods) =
  unlines $
  foreignImports ++
  [ ""
  , "class " ++ classname ++ " a where"
  , "    coerce' :: a -> " ++ cname
  ] ++ classMethods ++ [""] ++ defaultMethods
  ++
  [ ""
  , "newtype " ++ cname ++ " = " ++ cname ++ " (Ptr Raw)"
  , "instance " ++ classname ++ " " ++ cname ++ " where"
  , "    coerce = id"
  ]
  where
    (classMethods, defaultMethods, foreignImports) = unzip3 $ map (writeMethod (Name cppName)) methods
    classname = cname ++ "_Class"
    cname = toCName cppName

toLower' :: String -> String
toLower' [] = []
toLower' (x:xs) = toLower x:xs

writeMethod :: Name -> Method -> (String, String, String)
writeMethod (Name classname) (Method (Name name) retType args _ static) =
 (method, defaultMethod, foreignImport)
 where
   method = init $ unlines
            [ "    " ++ methodName ++ " :: a -> " ++ concat (intersperse " -> " hsargs)
              --  , show (name, retType, args, const', static)
            ]
   methodName = toLower' name 
   foreignImport = 
     "foreign import ccall unsafe \"" ++ cname ++ "\" " ++ hsname ++ " :: " ++ proto
   cname = toCName $ classname ++ "::" ++ name
   hsname = "c_" ++ cname
   proto
     | static == Static False = concat (intersperse " -> " hsargs)
     | otherwise = concat (intersperse " -> " (("Ptr "++classname):hsargs))
   hsargs = map (showHType . hsType) args ++ [showHType (hsType' retType)]

   defaultMethod = "    " ++ methodName ++ " = i'm a default method, wooo"

data HType = HtCInt
           | HtStdString
           | HtSXMat
           | HtSXFun
           | HtStdVec HType
           | HtPtr HType
           | HtIO HType

showHType :: HType -> String
showHType = showHType' False
  where
    showHType' :: Bool -> HType -> String
    showHType' _ HtCInt = "CInt"
    showHType' _ HtSXMat = "SXMatrix"
    showHType' _ HtSXFun = "SXFunction"
    showHType' _ HtStdString = "StdString"
    showHType' p (HtStdVec x) = maybeParens p $ "Vector " ++ showHType' True x
    showHType' p (HtPtr x) = maybeParens p $ "Ptr " ++ showHType' True x
    showHType' p (HtIO x) = maybeParens p $ "IO " ++ showHType' True x

    maybeParens False x = x
    maybeParens True x = "(" ++ x ++ ")"

hsType :: Type -> HType
hsType (Type "int") = HtCInt
hsType (Type "std::string") = HtStdString
hsType (Type "std::vector<CasADi::SXMatrix>") = HtStdVec HtSXMat
hsType (Type "CasADi::SXFunction") = HtSXFun
hsType (Type "CasADi::SXMatrix") = HtSXMat
hsType (Type x) = error $ "hsType: unrecognized primitive: " ++ x
hsType (Ptr x) = HtPtr (hsType x)
hsType (Ref x) = HtPtr (hsType x)

hsType' :: RetType -> HType
hsType' (SimpleType x) = HtIO $ hsType x
hsType' (NewRef x) = HtIO $ hsType (Ptr x)

main :: IO ()
main = do
--  mapM_ putStrLn $ map writeHsFunction tools
  mapM_ putStrLn $ map writeHsClass [sxfun]
