{-# OPTIONS_GHC -Wall #-}

module WriteCasadiBindings.WriteHs ( writeClassModules
                                   , writeDeleterModule
                                   , writeDataModule
                                   , writeToolsModule
                                   , writeIOSchemeHelpersModule
                                   , writeEnumsModule
                                   ) where

import Data.Char ( toLower, isLower )
import Data.List ( intersperse, sort )
import Data.Maybe ( fromMaybe )
import qualified Data.Text as T
import Language.Haskell.Syntax
import Language.Haskell.Pretty

import WriteCasadiBindings.Types
import WriteCasadiBindings.TypeMaps

replaces :: [(String,String)] -> String -> String
replaces ((find',replace'):xs) = replaces xs . (T.unpack . T.replace (T.pack find') (T.pack replace') . T.pack)
replaces [] = id

prettyTics :: String -> String
prettyTics = replaces [("_TIC","'")]


marshalFun :: [Type] -> String -> String -> (String, String)
marshalFun params fun wrappedFun =
  (fun ++ " " ++ concat (intersperse " " patternMatchArgs) ++ " =\n" ++ marshals,
   wrappedFun ++ " " ++ concat (intersperse " " appArgs))
  where
    marshals = unlines $ blah $ args
    args = zipWith (\k p -> ("x"++show k, p)) [(0::Int)..] params
    patternMatchArgs = map fst args
    appArgs = map (++ "'") patternMatchArgs

    blah :: [(String,Type)] -> [String]
    blah ((x,t):xs) = arg:blah xs
      where
        arg = case hsMarshalNewtypeWrapper t of
          Nothing -> "  withMarshal " ++ x ++ " $ \\" ++ x ++ "' ->"
          Just ntw -> "  withMarshal (" ++ ntw ++ " " ++ x ++ ") $ \\" ++ x ++ "' ->"
    blah [] = []

c_deleteName :: Type -> String
c_deleteName = ("c_" ++) .  deleteName

exportDecl :: [String] -> String
exportDecl names =
  unlines $
  "       (" :
  map (\x -> "         " ++ x ++ ",") names ++
  ["       ) where"]


deleters :: [Class] -> [String]
deleters classes = map deleteForeignImports typesToDelete
  where
    typesToDelete =
      [CInt,CDouble,StdString,CBool] ++
      map (\(Class classType _ _) -> (CasadiClass classType)) classes

    deleteForeignImports :: Primitive -> String
    deleteForeignImports classType = concatMap writeIt types
      where
        primType = if usedAsPtr classType then [Val (NonVec classType)] else []
        types = primType ++
                [ Ref (Vec (NonVec classType))
                , Ref (Vec (Vec (NonVec classType)))
                , Ref (Vec (Vec (Vec (NonVec classType))))
                ]
        writeIt c =
          unlines $
          [ "foreign import ccall unsafe \"&" ++ deleteName c ++ "\" "
          , "  " ++ c_deleteName c ++ " :: FunPtr ("++ ffiType False c ++ " -> IO ())"
          ]


newtype FFIWrapper = FFIWrapper String
data ClassFunction = ClassFunction String String Doc

rawName :: Class -> String
rawName c = dataName c ++ "'"

dataName :: Class -> String
dataName (Class classType _ _) = hsDataName classType

dataDecl :: Class -> String
dataDecl c = "newtype " ++ dataName c ++ " = " ++ dataName c ++ " (ForeignPtr " ++ rawName c ++ ")"

rawDecl :: Class -> String
rawDecl c = "data " ++ rawName c


writeClassMethods :: Class -> [(FFIWrapper, ClassFunction)]
writeClassMethods c@(Class classType methods' _)= methods
  where
    className = hsClassName classType

    methods = map writeMethod' methods'

    writeMethod' :: Method -> (FFIWrapper, ClassFunction)
    writeMethod' fcn = (FFIWrapper ffiWrapper, ClassFunction hsMethodName method (fDoc fcn))
      where
        method =
          unlines $
          [ hsMethodName ++ " :: " ++ typeDef
          , hsCall
          ]
        hsCall = case fMethodType fcn of
          Normal -> hsMethodName ++ " x = " ++ wrapperName ++ " (cast" ++ dataName c ++ " x)"
          _ -> hsMethodName ++ " = " ++ wrapperName
        cWrapperName'' = cWrapperName classType fcn

        betterCamelCase :: String -> String
        betterCamelCase input = case break isLower input of
          (x,[]) -> map toLower x
          ([x],xs) -> toLower x : xs
          ([x,y],xs) -> toLower x : toLower y : xs
          (xs,ys) -> map toLower (init xs) ++ [last xs] ++ ys

        hsMethodName = case fMethodType fcn of
          Constructor -> betterCamelCase $ prettyTics (toCName methodName)
          _ -> betterCamelCase (show classType) ++ "_" ++ prettyTics (toCName methodName)

        (wrapperName, ffiWrapper) = case fMethodType fcn of
          Normal -> writeFunction Nothing $ Function (Name cWrapperName'') (fType fcn) ((Ref (NonVec (CasadiClass classType))):(fArgs fcn)) (Doc "")
          _ -> writeFunction Nothing $ Function (Name cWrapperName'') (fType fcn) (fArgs fcn) (Doc "")

        typeDef = case fMethodType fcn of
          Normal -> className ++ " a => " ++ concat (intersperse " -> " ("a":map (hsType False) (fArgs fcn) ++ [retType']))
          _ -> concat (intersperse " -> " (map (hsType False) (fArgs fcn) ++ [retType']))
        retType' = "IO " ++ hsType True (fType fcn)

        Name methodName = fName fcn


lowerCase :: String -> String
lowerCase [] = error "lowerCase: empty string"
lowerCase (x:xs) = toLower x : xs

typeclassName :: Class -> String
typeclassName (Class classType _ _) = hsClassName classType

typeclassDecl :: Class -> String
typeclassDecl c =
  unlines
  [ "class " ++ typeclassName c ++ " a where"
  , "  cast" ++ dataName c ++ " :: a -> " ++ dataName c
  , "instance " ++ typeclassName c ++ " " ++ dataName c ++ " where"
  , "  cast" ++ dataName c ++ " = id"
  ]

helperInstances :: Class -> String
helperInstances c =
  unlines
  [ "instance Marshal " ++ dataName c ++ " (Ptr " ++ dataName c ++ "') where"
  , "  withMarshal ("++ dataName c ++ " x) f = withForeignPtr x f"
  , "instance ForeignPtrWrapper " ++ dataName c ++ " " ++ dataName c ++ "' where"
  , "  unwrapForeignPtr ("++ dataName c ++ " x) = x"
  , "instance WrapReturn (ForeignPtr " ++ dataName c ++ "') " ++ dataName c ++ " where"
  , "  wrapReturn = return . " ++ dataName c
  ]

baseclassInstances :: Class -> [Class] -> String
baseclassInstances c bcs = unlines $ map writeInstance' bcs
  where
    writeInstance' :: Class -> String
    writeInstance' bc =
      unlines
      [ "instance " ++ typeclassName bc ++ " " ++ dataName c ++ " where"
      , "  cast" ++ dataName bc ++ " (" ++ dataName c ++ " x) = " ++ dataName bc ++ " (castForeignPtr x)"
      ]

writeFunction :: Maybe String -> Function -> (String, String)
writeFunction maybeName fcn@(Function (Name hsFunctionName') retType params doc) = (hsFunctionName, ffiWrapper)
  where
    ffiWrapper =
      unlines $
      foreignImport : maybeDoc hsFunctionName' doc ++
      [ hsFunctionName ++ "\n  :: " ++ proto
      , marshals ++ "  " ++ call
      ]

    hsFunctionName = lowerCase $ prettyTics $ fromMaybe (toCName hsFunctionName') maybeName
    (marshals, call') = marshalFun params hsFunctionName c_hsFunctionName
    call = case makesNewRef retType of
      Nothing -> call' ++ " >>= wrapReturn"
      Just _ -> call' ++ " >>= (newForeignPtr " ++ c_deleteName retType ++ ") >>= wrapReturn"

    cFunctionName = cWrapperName' fcn
    c_hsFunctionName = "c_" ++ cFunctionName
    foreignImport =
      "foreign import ccall unsafe \"" ++ cFunctionName ++ "\" " ++ c_hsFunctionName ++ "\n  :: " ++ ffiProto
    ffiProto = concat $ intersperse " -> " $
               map (ffiType False) params ++ [ffiRetType]
    proto = concat $ intersperse " -> " $
            map (hsType False) params ++ [retType']

    retType' :: String
    retType' = "IO " ++ hsType True retType

    ffiRetType :: String
    ffiRetType = "IO " ++ ffiType True retType

writeDeleterModule :: [Class] -> String
writeDeleterModule classes =
  init $ unlines $
  [ "{-# OPTIONS_GHC -Wall #-}"
  , "{-# Language ForeignFunctionInterface #-}"
  , ""
  , "module Casadi.Wrappers.Deleters where"
  , ""
  , "import Foreign.C.Types"
  , "import Foreign.Ptr ( FunPtr, Ptr )"
  , ""
  , "import Casadi.Wrappers.Data"
  , "import Casadi.MarshalTypes"
  , ""
  ] ++ deleters classes

writeClassModules :: (Class -> [Class]) -> [Class] -> [(String, String)]
writeClassModules inheritance classes = map (\x -> (dataName x,writeOneModule x)) classes
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
      , "module Casadi.Wrappers.Classes." ++ dataName c
      , exportDecl $ [dataName c, typeclassName c ++ "(..)"] ++ sort names
      , ""
      , "import Prelude hiding ( Functor )"
      , ""
      , "import Data.Vector ( Vector )"
      , "import Foreign.C.Types"
      , "import Foreign.Ptr ( Ptr )"
      , "import Foreign.ForeignPtr ( newForeignPtr )"
      , "import System.IO.Unsafe ( unsafePerformIO ) -- for show instances"
      , ""
      ] ++ printableObjectImport ++
      [ "import Casadi.Wrappers.ForeignToolsInstances ( )"
      , "import Casadi.Wrappers.Deleters"
      , "import Casadi.Wrappers.Data"
      , "import Casadi.Wrappers.Enums"
      , "import Casadi.MarshalTypes ( CppVec, CppVecVec, CppVecVecVec,"
      , "                             StdString', CppBool' ) -- StdOstream'"
      , "import Casadi.Marshal ( CornerCase(..), Marshal(..) )"
      , "import Casadi.WrapReturn ( WrapReturn(..) )"
      , ""
      ] ++ showInstance ++ map f methods
      where
        methods = writeClassMethods c
        names = map (\(_, ClassFunction name _ _) -> name) methods
        f (FFIWrapper ffiw, ClassFunction name cf doc) =
          unlines $
          [ "-- direct wrapper"
          , ffiw
          , "-- classy wrapper"
          ] ++ maybeDoc name doc ++ [ cf ]
        showInstance
          | any isPrintableObject (c : inheritance c) =
            [ "instance Show " ++ dataName c ++ " where"
            , "  show = unsafePerformIO . printableObject_getDescription"
            ]
          | otherwise = []
        printableObjectImport
          | any isPrintableObject (inheritance c) =
            ["import Casadi.Wrappers.Classes.PrintableObject"]
          | otherwise = []

        isPrintableObject (Class PrintableObject _ _) = True
        isPrintableObject _ = False

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

writeDataModule :: [Class] -> (Class -> [Class]) -> String
writeDataModule classes baseClasses =
  unlines $
  [ "{-# OPTIONS_GHC -Wall #-}"
  , "{-# Language FlexibleInstances #-}"
  , "{-# Language MultiParamTypeClasses #-}"
  , ""
  , "module Casadi.Wrappers.Data where"
  , ""
  , "import Prelude hiding ( Functor )"
  , ""
  , "import Foreign.Ptr ( Ptr )"
  , "import Foreign.ForeignPtr ( ForeignPtr, castForeignPtr, withForeignPtr )"
  , ""
  , "import Casadi.MarshalTypes ( ForeignPtrWrapper(..) )"
  , "import Casadi.Marshal (  Marshal(..) )"
  , "import Casadi.WrapReturn ( WrapReturn(..) )"
  , ""
  ] ++ map writeData classes
  where

    writeData c@(Class _ _ (Doc classDoc)) =
      unlines $
      [ "-- draw decl"
      , rawDecl c
      , "-- data decl"
      , "{-|"
      ] ++ map (">" ++) (stripEmpty (lines classDoc)) ++
      [ "-}"
        , dataDecl c
      , "-- typeclass decl"
      , typeclassDecl c
      , "-- baseclass instances"
      , baseclassInstances c (baseClasses c)
      , "-- helper instances"
      , helperInstances c
      ]


writeToolsModule :: [Function] -> String
writeToolsModule functions =
  unlines $
  [ "{-# OPTIONS_GHC -Wall #-}"
  , ""
  , "module Casadi.Wrappers.Tools"
  , exportDecl (sort funNames)
  , ""
  , "import Data.Vector ( Vector )"
  , "import Foreign.C.Types"
  , "import Foreign.Ptr ( Ptr )"
  , "import Foreign.ForeignPtr ( newForeignPtr )"
  , ""
  , "import Casadi.Wrappers.Data"
  , "import Casadi.Wrappers.Enums"
  , "import Casadi.Wrappers.Deleters"
  , "import Casadi.Wrappers.ForeignToolsInstances ( )"
  , "import Casadi.MarshalTypes ( CppVec, CppVecVec, CppBool', StdString' )"
  , "import Casadi.Marshal (  CornerCase(..), Marshal(..) )"
  , "import Casadi.WrapReturn ( WrapReturn(..) )"
  , ""
  ] ++ funDecls
  where
    (funNames, funDecls) = unzip $ map (\f -> writeFunction (Just (rename f)) f) functions
    rename (Function (Name uglyName) _ _ _) = beautifulHaskellName uglyName

    -- haskell functions can't have capital leading letter
    beautifulHaskellName :: String -> String
    beautifulHaskellName uglyName = case replaces [("_TIC","'"),("CasADi::",""),(":","_")] uglyName of
      [] -> error "beautifulHaskellName: empty string will never be beautiful :'("
      -- fall back on just making just the first letter lowercase
      "data" -> "data_"
      x -> x


writeIOSchemeHelpersModule :: [Function] -> String
writeIOSchemeHelpersModule functions =
  unlines $
  [ "{-# OPTIONS_GHC -Wall #-}"
  , ""
  , "module Casadi.Wrappers.IOSchemeHelpers"
  , exportDecl (sort funNames)
  , ""
  , "import Data.Vector ( Vector )"
  , "import Foreign.Ptr ( Ptr )"
  , "import Foreign.ForeignPtr ( newForeignPtr )"
  , ""
  , "import Casadi.Wrappers.Data"
  , "import Casadi.Wrappers.Deleters"
  , "import Casadi.Wrappers.ForeignToolsInstances ( )"
  , "import Casadi.MarshalTypes ( CppVec, StdString' )"
  , "import Casadi.Marshal (  Marshal(..) )"
  , "import Casadi.WrapReturn ( WrapReturn(..) )"
  , ""
  ] ++ funDecls
  where
    (funNames, funDecls) = unzip $ map (\f -> writeFunction (Just (rename f)) f) functions
    rename (Function (Name uglyName) _ _ _) = beautifulHaskellName uglyName

    -- haskell functions can't have capital leading letter
    beautifulHaskellName :: String -> String
    beautifulHaskellName uglyName = case replaces [("_TIC","'"),("CasADi::",""),(":","_")] uglyName of
      [] -> error "beautifulHaskellName: empty string will never be beautiful :'("
      -- fall back on just making just the first letter lowercase
      "data" -> "data_"
      x -> x

writeEnumsModule :: [CEnum] -> String
writeEnumsModule enums =
  unlines $
  [ "{-# OPTIONS_GHC -Wall #-}"
  , "{-# LANGUAGE MultiParamTypeClasses #-}"
  , ""
  , "module Casadi.Wrappers.Enums"
  , exportDecl (sort (map (\(CEnum name _ _ _) -> show name ++ "(..)") enums))
  , ""
  , "import Foreign.C.Types ( CInt(..) )"
  , "import Casadi.Marshal ( Marshal(..) )"
  , "import Casadi.WrapReturn ( WrapReturn(..) )"
  , ""
  ] ++ enumDecls
  where
    enumDecls = map writeEnumDecl enums

    makeEnumDecl :: CasadiEnum -> [String] -> String
    makeEnumDecl name fields =
      strip $ prettyPrint $
      HsDataDecl src0 [] (HsIdent (show name)) [] (map (\f -> HsConDecl src0 (HsIdent f) []) fields)
      [UnQual (HsIdent "Show"),UnQual (HsIdent "Eq")]

    makeMarshalInstance :: CasadiEnum -> String
    makeMarshalInstance name =
      strip $ prettyPrint $
      HsInstDecl src0 [] (UnQual (HsIdent "Marshal")) [HsTyCon (UnQual (HsIdent (show name))), HsTyCon (UnQual (HsIdent "CInt"))] [HsFunBind [HsMatch src0 (HsIdent "withMarshal") [HsPVar (HsIdent "x"),HsPVar (HsIdent "f")] (HsUnGuardedRhs (HsApp (HsVar (UnQual (HsIdent "f"))) (HsParen (HsApp (HsVar (UnQual (HsIdent "fromIntegral"))) (HsParen (HsApp (HsVar (UnQual (HsIdent "fromEnum"))) (HsVar (UnQual (HsIdent "x"))))))))) []]]

    makeWrapReturnInstance :: CasadiEnum -> String
    makeWrapReturnInstance name =
      strip $ prettyPrint $
      HsInstDecl src0 [] (UnQual (HsIdent "WrapReturn")) [HsTyCon (UnQual (HsIdent "CInt")), HsTyCon (UnQual (HsIdent (show name)))] [HsPatBind (SrcLoc {srcFilename = "<unknown>", srcLine = 2, srcColumn = 3}) (HsPVar (HsIdent "wrapReturn")) (HsUnGuardedRhs (HsInfixApp (HsInfixApp (HsVar (UnQual (HsIdent "return"))) (HsQVarOp (UnQual (HsSymbol "."))) (HsVar (UnQual (HsIdent "toEnum")))) (HsQVarOp (UnQual (HsSymbol "."))) (HsVar (UnQual (HsIdent "fromIntegral"))))) []]

    makeEnumInstance :: CasadiEnum -> [(String, Integer)] -> String
    makeEnumInstance name elems =
      strip $ prettyPrint $
      HsInstDecl src0 [] (UnQual (HsIdent "Enum")) [HsTyCon (UnQual (HsIdent (show name)))]
      [HsFunBind (map f elems)
      ,HsFunBind $ map g elems ++ [err]
      ]
      where
        f (fld,k) = HsMatch src0 (HsIdent "fromEnum") [HsPParen (HsPApp (UnQual (HsIdent fld)) [])] (HsUnGuardedRhs (HsLit (HsInt k))) []
        g (fld,k) = HsMatch src0 (HsIdent "toEnum") [HsPParen (HsPLit (HsInt k))] (HsUnGuardedRhs (HsCon (UnQual (HsIdent fld)))) []
        err = HsMatch src0 (HsIdent "toEnum") [HsPVar (HsIdent "k")] (HsUnGuardedRhs (HsInfixApp (HsInfixApp (HsVar (UnQual (HsIdent "error"))) (HsQVarOp (UnQual (HsSymbol "$"))) (HsLit (HsString (show name ++ ": toEnum: got unhandled number: ")))) (HsQVarOp (UnQual (HsSymbol "++"))) (HsApp (HsVar (UnQual (HsIdent "show"))) (HsVar (UnQual (HsIdent "k")))))) []


    writeEnumDecl :: CEnum -> String
    writeEnumDecl (CEnum name _ _ xs) = hssrc
      where
        hssrc = init $ unlines [ "-- EnumDecl: " ++ show name
                               , hsEnum
                               , hsEnumInstance
                               , hsMarshalInstance
                               , hsWrapReturnInstance
                               , ""
                               ]
        hsEnum = makeEnumDecl name (map (\(x,_,_) -> x) xs)
        hsEnumInstance = makeEnumInstance name (map (\(x,_,y) -> (x,y)) xs)
        hsMarshalInstance = makeMarshalInstance name
        hsWrapReturnInstance = makeWrapReturnInstance name

    strip :: String -> String
    strip = T.unpack . T.strip . T.pack

    src0 :: SrcLoc
    src0 = SrcLoc {srcFilename = "<unknown>", srcLine = 1, srcColumn = 1}
