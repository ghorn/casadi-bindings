{-# OPTIONS_GHC -Wall #-}

module WriteBindings.WriteHs
       ( writeClassModules
       , writeDataModule
       , writeToolsModule
       , writeEnumsModule
       ) where

import Data.Char ( toLower, isLower )
import Data.List ( intercalate, partition, sort )
import qualified Data.Foldable as F
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Text as T
import Language.Haskell.Syntax
import Language.Haskell.Pretty

import WriteBindings.ParseJSON
import qualified WriteBindings.TypeMaps as TM

rstrip :: String -> String
rstrip xs = case reverse xs of
  '\n':ys -> reverse ys
  _ -> xs

marshalFun :: Type -> [(Type, SwigOutput)] -> String -> String -> String
marshalFun retType params fun wrappedFun =
  unlines $
  [ fun ++ " " ++ intercalate " " patternMatchArgs ++ " = do"
  , marshals
  , ""
  , "  errStrPtrP <- new nullPtr"
  , "  ret0 <- " ++ wrappedFun ++ " errStrPtrP " ++ intercalate " " appArgs
  , "  errStrPtr <- peek errStrPtrP"
  , "  free errStrPtrP"
  , ""
  , "  " ++ retPattern ++ " <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)"
  , ""
  , unmarshals
  , ""
  , "  return " ++ ret
  , ""
  ]
  where
    retPattern = case retType of
      CVoid -> "()"
      _ -> "ret"

    ret = case (swigOutputNames, retType) of
      ([], CVoid) -> "()"
      ([], _) -> "ret"
      (_, CVoid) -> "(" ++ intercalate ", " (map (++ "'''") swigOutputNames) ++ ")"
      _ -> "(ret, " ++ intercalate ", " (map (++ "'''") swigOutputNames) ++ ")"

    swigOutputNames :: [String]
    swigOutputNames = map fst $ fst $ partition (\(_, (_, SwigOutput swigOutput)) -> swigOutput) args

    marshals = rstrip $ unlines $ map toMarshal args
    unmarshals = rstrip $ unlines $ map toUnmarshal args

    args :: [(String, (Type, SwigOutput))]
    args = zipWith f [0..] params
      where
        f :: Int -> (Type, SwigOutput) -> (String, (Type, SwigOutput))
        f k t@(_, SwigOutput False) = ("x" ++ show k, t)
        f k t@(_, SwigOutput True) = ("o" ++ show k, t)

    -- inputs
    patternMatchArgs = map fst $ filter (\(_, (_, SwigOutput swigOutput)) -> not swigOutput) args

    -- arguments to be applied to the ffi function
    appArgs = map ((++ "'") . fst) args

    toMarshal :: (String, (Type, SwigOutput)) -> String
    toMarshal (x, (_type, SwigOutput False)) = "  " ++ x ++ "' <- marshal " ++ x
    toMarshal (x, (_type, SwigOutput True)) = "  " ++ x ++ "' <- new nullPtr"

    toUnmarshal :: (String, (Type, SwigOutput)) -> String
    toUnmarshal (x, (_type, SwigOutput False)) = "  marshalFree " ++ x ++ " " ++ x ++ "'"
    toUnmarshal (x, (_type, SwigOutput True)) =
      "  " ++ x ++ "'' <- peek " ++ x ++ "'" ++ "\n" ++
      "  free " ++ x ++ "'\n" ++
      "  " ++ x ++ "''' <- if " ++ x ++ "'' == nullPtr then error \"" ++ err ++ "\" else wrapReturn " ++ x ++ "''"
      where
        err = "swig output " ++ x ++ "' was not set in " ++ fun ++ "/" ++ wrappedFun


c_deleteName :: Type -> String
c_deleteName = ("c_" ++) .  TM.deleteName

exportDecl :: [String] -> String
exportDecl names =
  unlines $
  "       (" :
  map (\x -> "         " ++ x ++ ",") names ++
  ["       ) where"]

newtype FFIWrapper = FFIWrapper String
data ClassFunction = ClassFunction String String Doc

rawName :: ClassType -> String
rawName ct = dataName ct ++ "'"

dataName :: ClassType -> String
dataName ct = TM.hsDataName $ unClassType ct

dataDecl :: ClassType -> String
dataDecl ct = "newtype " ++ dataName ct ++ " = " ++ dataName ct ++ " (ForeignPtr " ++ rawName ct ++ ")"

rawDecl :: ClassType -> String
rawDecl ct = "data " ++ rawName ct

betterCamelCase :: String -> String
betterCamelCase input = case break isLower input of
  -- if all letters are upper, make them all lower
  (x,[]) -> map toLower x
  -- if first letter is already lower, do nothing
  ([],x) -> x
  -- Function --> function
  ([x],xs) -> toLower x : xs
  -- XFunction --> xfunction
  ([x,y],xs) -> toLower x : toLower y : xs
  -- IOScheme -> ioScheme
  (xs,ys) -> map toLower (init xs) ++ [last xs] ++ ys

writeClassMethods :: Class -> [(FFIWrapper, ClassFunction)]
writeClassMethods c = methods
  where
    ct = clType c
    ClassType classType' = ct

    methods' = clMethods c
    className' = TM.hsClassName classType'

    methods = concatMap writeMethods methods'

    writeMethods :: Methods -> [(FFIWrapper, ClassFunction)]
    writeMethods (Left fs) = map writeMethod' fs
    writeMethods (Right f) = [writeMethod' f]

    writeMethod' :: Method -> (FFIWrapper, ClassFunction)
    writeMethod' fcn = (FFIWrapper ffiWrapper, ClassFunction hsMethodName method (mDocs fcn))
      where
        method =
          unlines $
          [ hsMethodName ++ " :: " ++ typeDef
          , hsCall
          ]
        hsCall = case mKind fcn of
          Normal -> hsMethodName ++ " x = " ++ wrapperName ++ " (cast" ++ dataName ct ++ " x)"
          _ -> hsMethodName ++ " = " ++ wrapperName
        cWrapperName'' = TM.cWrapperName classType' fcn

        number = case mOthers fcn of
          Nothing -> ""
          Just k -> "__" ++ show k

        hsname = case classType' of
          x@(UserType (Namespace ["casadi"]) (Name _)) -> TM.hsType False x
          y -> error $ "class method name got non-class object: " ++ show y

        hsMethodName = case mKind fcn of
          Constructor -> betterCamelCase hsname ++ number
          _ -> betterCamelCase hsname ++ "_" ++ TM.toCName methodName' ++ number


        (wrapperName, ffiWrapper) = case mKind fcn of
          Normal -> writeFunction
                    CppFunction { fName = cWrapperName''
                                , fOthers = Nothing
                                , fReturn = mReturn fcn
                                , fParams = (Ref classType', SwigOutput False) : mParams fcn
                                , fFriendwrap = False
                                , fDocs = mDocs fcn
                                }
          _ -> writeFunction
               CppFunction { fName = cWrapperName''
                           , fOthers = Nothing
                           , fReturn = mReturn fcn
                           , fParams = mParams fcn
                           , fFriendwrap = False
                           , fDocs = mDocs fcn
                           }

        (outputParams, inputParams) = (map fst ops, map fst ips)
          where
            (ops, ips) = partition (\(_, SwigOutput swigOutput) -> swigOutput) (mParams fcn)

        typeDef = case mKind fcn of
          Normal -> className' ++ " a => " ++ intercalate " -> " ("a":map (TM.hsType False) inputParams ++ [retType'])
          _ -> intercalate " -> " (map (TM.hsType False) inputParams ++ [retType'])

        retType' = case (outputParams, mReturn fcn) of
          ([], _) -> "IO " ++ TM.hsType True (mReturn fcn)
          (_, CVoid) -> "IO (" ++ intercalate ", " (map (TM.hsType False) outputParams) ++ ")"
          _ -> "IO (" ++ intercalate ", " (map (TM.hsType False) (mReturn fcn : outputParams)) ++ ")"

        Name methodName' = mName fcn


lowerCase :: String -> String
lowerCase [] = error "lowerCase: empty string"
lowerCase (x:xs) = toLower x : xs

typeclassName :: ClassType -> String
typeclassName c = TM.hsClassName $ unClassType c

typeclassDecl :: Class -> String
typeclassDecl c =
  unlines
  [ "class " ++ typeclassName ct ++ " a where"
  , "  cast" ++ dataName ct ++ " :: a -> " ++ dataName ct
  , "instance " ++ typeclassName ct ++ " " ++ dataName ct ++ " where"
  , "  cast" ++ dataName ct ++ " = id"
  ]
  where
    ct = clType c

helperInstances :: Class -> String
helperInstances c =
  unlines
  [ "instance Marshal " ++ dn ++ " (Ptr " ++ dn ++ "') where"
  , "  marshal (" ++ dn ++ " x) = return (unsafeForeignPtrToPtr x)"
  , "  marshalFree (" ++ dn ++ " x) _ = touchForeignPtr x"
  , "foreign import ccall unsafe \"&" ++ TM.deleteName cc ++ "\" "
  , "  " ++ c_deleteName cc ++ " :: FunPtr ("++ TM.ffiType False (cc, SwigOutput False) ++ " -> IO ())"
  , "instance WrapReturn (Ptr " ++ dn ++ "') " ++ dn ++ " where"
  , "  wrapReturn = (fmap " ++ dn ++ ") . (newForeignPtr " ++ c_deleteName cc ++ ")"
  ]
  where
    cc = unClassType (clType c)
    dn = dataName (clType c)

baseclassInstances :: Class -> [ClassType] -> String
baseclassInstances c bcs = unlines $ map writeInstance' bcs
  where
    writeInstance' :: ClassType -> String
    writeInstance' bc =
      unlines
      [ "instance " ++ typeclassName bc ++ " " ++ dataName ct ++ " where"
      , "  cast" ++ dataName bc ++ " (" ++ dataName ct ++ " x) = " ++ dataName bc ++ " (castForeignPtr x)"
      ]

    ct = clType c


startsWith :: String -> String -> Bool
startsWith _ [] = True
startsWith (x:xs) (y:ys)
  | x == y = startsWith xs ys
  | otherwise = False
startsWith [] _ = False

writeFunction :: CppFunction -> (String, String)
writeFunction fun = (hsFunctionName, ffiWrapper)
  where
    ffiWrapper =
      unlines $
      foreignImport : maybeDoc hsFunctionName (fDocs fun) ++
      [ ""
      , hsFunctionName
      , "  :: " ++ proto
      , marshalFun retType params hsFunctionName c_hsFunctionName
      ]

    params = fParams fun
    retType = fReturn fun

    hsFunctionName = lowerCase (TM.toCName (TM.cWrapperName' fun))

    cFunctionName = TM.cWrapperName' fun
    c_hsFunctionName = "c_" ++ cFunctionName
    safeunsafe
      | any (cFunctionName `startsWith`)
        [ "casadi__Function__jacobian"
        , "casadi__Function__call"
        , "casadi__Function__callDerivative"
        , "casadi__Function__hessian"
        , "casadi__Function__derivative"
        , "casadi__Function__gradient"
        , "casadi__Function__tangent"
        , "casadi__SharedObject__init"
        ] = "safe"
      | otherwise = "unsafe"
    foreignImport =
      "foreign import ccall " ++ safeunsafe ++ " \"" ++ cFunctionName ++ "\" " ++
      c_hsFunctionName ++ "\n  :: " ++ ffiProto
    ffiProto = intercalate " -> " $
               "Ptr (Ptr StdString)" : map (TM.ffiType False) params ++ [ffiRetType]
    proto = intercalate " -> " $
            map (TM.hsType False) inputParams ++ [retType']

    (outputParams, inputParams) = (map fst ops, map fst ips)
      where
        (ops, ips) = partition (\(_, SwigOutput swigOutput) -> swigOutput) params

    retType' :: String
    retType' = case (outputParams, retType) of
      ([], _) -> "IO " ++ TM.hsType True retType
      (_, CVoid) -> "IO (" ++ intercalate ", " (map (TM.hsType False) outputParams) ++ ")"
      _ -> "IO (" ++ intercalate ", " (map (TM.hsType False) (retType : outputParams)) ++ ")"

    ffiRetType :: String
    ffiRetType = "IO " ++ TM.ffiType True (retType, SwigOutput False)

writeClassModules :: (ClassType -> S.Set ClassType) -> [Class] -> [(String, String)]
writeClassModules _inheritance classes = map (\x -> (dataName (clType x),writeOneModule x)) classes
  where
    writeOneModule :: Class -> String
    writeOneModule c =
      init $ unlines $
      [ "{-# OPTIONS_GHC -Wall #-}"
      , "{-# OPTIONS_GHC -fno-warn-unused-imports #-}"
      , "{-# OPTIONS_GHC -fno-warn-orphans #-}"
      , "{-# Language ForeignFunctionInterface #-}"
      , "{-# Language FlexibleInstances #-}"
      , "{-# Language MultiParamTypeClasses #-}"
      , ""
      , "module Casadi.Core.Classes." ++ dataName ct
      , exportDecl $ [dataName ct, typeclassName ct ++ "(..)"] ++ sort names
      , ""
      , "import Prelude hiding ( Functor )"
      , ""
      , "import Data.Vector ( Vector )"
      , "import qualified Data.Map as M"
      , "import Foreign.C.Types"
      , "import Foreign.Marshal ( new, free )"
      , "import Foreign.Storable ( peek )"
      , "import Foreign.Ptr ( Ptr, nullPtr )"
      , "import Foreign.ForeignPtr ( newForeignPtr )"
      , "import System.IO.Unsafe ( unsafePerformIO ) -- for show instances"
      , ""
      , "import Casadi.Internal.FormatException ( formatException )"
      , "import Casadi.Internal.MarshalTypes ( StdVec, StdString, StdMap, StdPair ) -- StdPair StdOstream'"
      , "import Casadi.Internal.Marshal ( Marshal(..), marshal, marshalFree )"
      , "import Casadi.Internal.WrapReturn ( WrapReturn(..) )"
      , "import Casadi.Core.Data"
      , "import Casadi.Core.Enums"
      ] ++ map f methods
      where
        ct = clType c
        methods = writeClassMethods c
        names = map (\(_, ClassFunction name _ _) -> name) methods
        f (FFIWrapper ffiw, ClassFunction name cf doc) =
          unlines $
          [ "-- direct wrapper"
          , ffiw
          , "-- classy wrapper"
          ] ++ maybeDoc name doc ++ [ cf ]

maybeDoc :: String -> Doc -> [String]
maybeDoc name (Doc doc)
  | stripEmpty (lines doc) == [] = []
  | '\'' `elem` name = []
  | otherwise = "{-|" : map (">"++) (stripEmpty (lines doc)) ++ ["-}"]

stripEmpty :: [String] -> [String]
stripEmpty = reverse . stripLeading . reverse . stripLeading
  where
    stripLeading :: [String] -> [String]
    stripLeading [] = []
    stripLeading ret@(x:xs)
      | all (== ' ') x = stripLeading xs
      | otherwise = ret

writeDataModule :: [Class] -> (ClassType -> S.Set ClassType) -> String
writeDataModule classes baseClasses =
  unlines $
  [ "{-# OPTIONS_GHC -Wall #-}"
  , "{-# Language FlexibleInstances #-}"
  , "{-# Language MultiParamTypeClasses #-}"
  , ""
  , "module Casadi.Core.Data where"
  , ""
  , "import Prelude hiding ( Functor )"
  , ""
  , "import Foreign.Ptr ( Ptr, FunPtr )"
  , "import Foreign.ForeignPtr ( ForeignPtr, castForeignPtr, newForeignPtr, touchForeignPtr )"
  , "import Foreign.ForeignPtr.Unsafe ( unsafeForeignPtrToPtr )"
  , ""
  , "import Casadi.Internal.Marshal (  Marshal(..) )"
  , "import Casadi.Internal.WrapReturn ( WrapReturn(..) )"
  ] ++ map writeData classes
  where
    writeData c =
      unlines $
      [ "-- raw decl"
      , rawDecl ct
      , "-- data decl"
      , "{-|"
      ] ++ map (">" ++) (stripEmpty (lines docs)) ++
      [ "-}"
        , dataDecl ct
      , "-- typeclass decl"
      , typeclassDecl c
      , "-- baseclass instances"
      , baseclassInstances c (F.toList (baseClasses ct))
      , "-- helper instances"
      , helperInstances c
      ]
      where
        ct = clType c
        Doc docs = clDocs c


writeToolsModule :: [CppFunctions] -> String
writeToolsModule functions =
  unlines $
  [ "{-# OPTIONS_GHC -Wall #-}"
  , ""
  , "module Casadi.Core.Tools"
  , exportDecl (sort funNames)
  , ""
  , "import Data.Vector ( Vector )"
  , "import qualified Data.Map as M"
  , "import Foreign.C.Types"
  , "import Foreign.Marshal ( new, free )"
  , "import Foreign.Storable ( peek )"
  , "import Foreign.Ptr ( Ptr, nullPtr )"
  , ""
  , "import Casadi.Core.Data"
  , "import Casadi.Core.Enums"
  , "import Casadi.Internal.FormatException ( formatException )"
  , "import Casadi.Internal.MarshalTypes ( StdMap, StdVec, StdString, StdPair )"
  , "import Casadi.Internal.Marshal ( Marshal(..) )"
  , "import Casadi.Internal.WrapReturn ( WrapReturn(..) )"
  ] ++ funDecls
  where
    (funNames, funDecls) = unzip $ concatMap writeFunctions functions

    writeFunctions :: CppFunctions -> [(String, String)]
    writeFunctions (Left fs) = map writeFunction fs
    writeFunctions (Right f) = [writeFunction f]

--writeIOSchemeHelpersModule :: String -> [CppFunction Type] -> String
--writeIOSchemeHelpersModule modname functions =
--  unlines $
--  [ "{-# OPTIONS_GHC -Wall #-}"
--  , ""
--  , "module Casadi." ++ modname ++ ".IOSchemeHelpers"
--  , exportDecl (sort funNames)
--  , ""
--  , "import Data.Vector ( Vector )"
--  , "import Foreign.Ptr ( Ptr )"
--  , "import Foreign.ForeignPtr ( newForeignPtr )"
--  , ""
--  , "import Casadi." ++ modname ++ ".Data"
--  , "import Casadi.Internal.MarshalTypes ( StdVec, StdString )"
--  , "import Casadi.Internal.Marshal (  Marshal(..), withMarshal )"
--  , "import Casadi.Internal.WrapReturn ( WrapReturn(..) )"
--  , ""
--  ] ++ funDecls
--  where
--    (funNames, funDecls) = unzip $ map (\f -> writeFunction (Just (rename f)) f) functions
--    rename cppFun = beautifulHaskellName (funName cppFun)
--
--    -- haskell functions can't have capital leading letter
--    beautifulHaskellName :: String -> String
--    beautifulHaskellName uglyName = case replaces [("CasADi::",""),(":","_")] uglyName of
--      [] -> error "beautifulHaskellName: empty string will never be beautiful :'("
--      -- fall back on just making just the first letter lowercase
--      "data" -> "data_"
--      x -> x

enumName :: Name -> String
enumName n = TM.hsType False (CEnum (Namespace []) n)

writeEnumsModule :: [(Name,Enum')] -> String
writeEnumsModule enums =
  unlines $
  [ "{-# OPTIONS_GHC -Wall #-}"
  , "{-# OPTIONS_GHC -fno-warn-unused-imports #-}"
  , "{-# LANGUAGE MultiParamTypeClasses #-}"
  , ""
  , "module Casadi.Core.Enums"
  , exportDecl $ sort $ map (\(n,_) -> enumName n ++ "(..)") enums
  , ""
  , "import Foreign.C.Types ( CInt(..) )"
  , "import Casadi.Internal.Marshal ( Marshal(..) )"
  , "import Casadi.Internal.WrapReturn ( WrapReturn(..) )"
  , ""
  ] ++ enumDecls
  where
    enumDecls = map writeEnumDecl enums

    makeEnumDecl :: Name -> [String] -> String
    makeEnumDecl name fields =
      strip $ prettyPrint $
      HsDataDecl src0 [] (HsIdent (enumName name)) [] (map (\f -> HsConDecl src0 (HsIdent f) []) fields)
      [UnQual (HsIdent "Show"),UnQual (HsIdent "Eq")]

    makeMarshalInstance :: Name -> String
    makeMarshalInstance name =
      strip $ prettyPrint $
      HsInstDecl src0 [] (UnQual (HsIdent "Marshal")) [HsTyCon (UnQual (HsIdent (enumName name))), HsTyCon (UnQual (HsIdent "CInt"))]
      [ HsFunBind [HsMatch src0 (HsIdent "marshal") [] (HsUnGuardedRhs (HsInfixApp (HsInfixApp (HsVar (UnQual (HsIdent "return"))) (HsQVarOp (UnQual (HsSymbol "."))) (HsVar (UnQual (HsIdent "fromIntegral")))) (HsQVarOp (UnQual (HsSymbol "."))) (HsVar (UnQual (HsIdent "fromEnum"))))) []]
      ]

    makeWrapReturnInstance :: Name -> String
    makeWrapReturnInstance name =
      strip $ prettyPrint $
      HsInstDecl src0 [] (UnQual (HsIdent "WrapReturn")) [HsTyCon (UnQual (HsIdent "CInt")), HsTyCon (UnQual (HsIdent (enumName name)))] [HsPatBind (SrcLoc {srcFilename = "<unknown>", srcLine = 2, srcColumn = 3}) (HsPVar (HsIdent "wrapReturn")) (HsUnGuardedRhs (HsInfixApp (HsInfixApp (HsVar (UnQual (HsIdent "return"))) (HsQVarOp (UnQual (HsSymbol "."))) (HsVar (UnQual (HsIdent "toEnum")))) (HsQVarOp (UnQual (HsSymbol "."))) (HsVar (UnQual (HsIdent "fromIntegral"))))) []]

    makeEnumInstance :: Name -> [(String, Integer)] -> String
    makeEnumInstance name elems =
      strip $ prettyPrint $
      HsInstDecl src0 [] (UnQual (HsIdent "Enum")) [HsTyCon (UnQual (HsIdent (enumName name)))]
      [HsFunBind (map f elems)
      ,HsFunBind $ map g elems ++ [err]
      ]
      where
        f (fld,k) = HsMatch src0 (HsIdent "fromEnum")
                    [HsPParen (HsPApp (UnQual (HsIdent fld)) [])]
                    (HsUnGuardedRhs (HsLit (HsInt k))) []
        g (fld,k) = HsMatch src0 (HsIdent "toEnum")
                    [HsPParen (HsPLit (HsInt k))]
                    (HsUnGuardedRhs (HsCon (UnQual (HsIdent fld)))) []
        err = HsMatch src0 (HsIdent "toEnum")
              [HsPVar (HsIdent "k")]
              (HsUnGuardedRhs
               (HsInfixApp
                (HsInfixApp (HsVar (UnQual (HsIdent "error")))
                 (HsQVarOp (UnQual (HsSymbol "$")))
                 (HsLit (HsString ((enumName name) ++ ": toEnum: got unhandled number: "))))
                (HsQVarOp (UnQual (HsSymbol "++")))
                (HsApp (HsVar (UnQual (HsIdent "show")))
                 (HsVar (UnQual (HsIdent "k")))))) []


    writeEnumDecl :: (Name, Enum') -> String
    writeEnumDecl (name@(Name n), e) = hssrc
      where
        hssrc = init $ unlines [ "-- EnumDecl: " ++ n
                               , hsEnum
                               , hsEnumInstance
                               , hsMarshalInstance
                               , hsWrapReturnInstance
                               , ""
                               ]
        hsEnum = makeEnumDecl name (sort (M.keys (enumEntries e)))
        hsEnumInstance = makeEnumInstance name
                         (M.toList (fmap (fromIntegral . enumEntryVal) (enumEntries e)))
        hsMarshalInstance = makeMarshalInstance name
        hsWrapReturnInstance = makeWrapReturnInstance name

    strip :: String -> String
    strip = T.unpack . T.strip . T.pack

    src0 :: SrcLoc
    src0 = SrcLoc {srcFilename = "<unknown>", srcLine = 1, srcColumn = 1}
