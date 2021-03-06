{-# OPTIONS_GHC -Wall -Wextra -Werror -Wno-unused-top-binds -fno-warn-orphans #-}
{-# Language DeriveGeneric #-}
{-# Language DeriveFunctor #-}
{-# Language FlexibleInstances #-}

module WriteBindings.ParseJSON
       ( Class(..)
       , ClassType(..)
       , Namespace(..)
       , Methods
       , Method(..)
       , CppFunctions
       , CppFunction(..)
       , Name(..)
       , SwigOutput(..)
       , Type(..)
       , MethodKind(..)
       , Doc(..)
       , DocLink(..)
       , Tree(..)
       , Enum'(..)
       , EnumEntry(..)
       , Module(..)
       , readModule
       ) where

import GHC.Generics ( Generic )
import Control.Arrow ( first )
import qualified Data.Aeson as Aeson
import Data.Aeson.Types ( typeMismatch )
import qualified Data.ByteString.Lazy as BS
import qualified Data.Text as Text
import qualified Data.Map.Strict as M
import Text.Parsec
--import Debug.Trace
import qualified Data.Set as S

newtype SwigOutput = SwigOutput Bool deriving (Eq, Ord, Show)

toSwigOutput :: String -> SwigOutput
toSwigOutput "Output" = SwigOutput True
toSwigOutput "Normal" = SwigOutput False
toSwigOutput r = error $ "while reading SwigOutput, got unexpected string: " ++ show r

instance Aeson.FromJSON ClassType where
  parseJSON (Aeson.String txt) =
    case parse (typeParser (const False) <* eof) "" str of
      Right x@(UserType {})    -> return (ClassType x)
      Right x@(StdVec _)       -> return (ClassType x)
      Right x@(StdPair {})     -> return (ClassType x)
      Right x@(StdMap {})      -> return (ClassType x)
      Right x@(PrintableObject {}) -> return (ClassType x)
      Right y -> fail $ "\nParsec parser returned wrong class type: " ++ str ++ "\n" ++ show y
      Left x -> fail $ "\nParsec parser failed on " ++ str ++ "\n" ++ show x
    where
      str = Text.unpack txt
  parseJSON v = typeMismatch "Type" v

newtype ClassType = ClassType { unClassType :: Type } deriving (Eq, Ord, Show)
instance GetTypes ClassType where
  getTypes (ClassType t) = S.singleton t

parseType :: IsEnum -> String -> Type
parseType isEnum str = case parse ((typeParser isEnum) <* eof) "" str of
    Right x ->
      --("parsed \"" ++ str ++ "\" -> " ++ show x) `trace`
      x
    Left x -> error $ "\nParsec parser failed on " ++ str ++ "\n" ++ show x

p :: String -> Type -> Parsec String u Type
p name tp = try $ do
  _ <- string name
  notFollowedBy (alphaNum <|> space)
  return tp

ps :: [String] -> Type -> Parsec String u Type
ps names tp = choice $ map (flip p tp) names

templatep :: IsEnum -> String -> (Type -> Type) -> Parsec String u Type
templatep isEnum name f =
  try $ fmap f $
  between (string (name++"<(")) (string ")>") (typeParser isEnum)

classp :: IsEnum -> Parsec String u Type
classp isEnum = try $ do
  ns <- namespace
  blah <- many (alphaNum <|> char '_')
  -- if there is no namespace set it to casadi::
  let ns' = case ns of Namespace [] -> Namespace ["casadi"]
                       ns'' -> ns''
  return $
    if isEnum blah
    then CEnum ns (Name blah)
    else UserType ns' (Name blah)

namespace :: Parsec String u Namespace
namespace = do
  ns <- optionMaybe namespace'
  case ns of Nothing -> return (Namespace [])
             Just xs -> return (Namespace xs)

namespace' :: Parsec String u [String]
namespace' = try $ do
    name <- manyTill (alphaNum <|> char '_') $ (try (string "::"))
    moar <- optionMaybe namespace'
    case moar of Nothing -> return [name]
                 Just xs -> return ([name]++xs)


pairp :: IsEnum -> Parsec String u Type
pairp isEnum =
  try $
  between (string "std::pair<(") (string ")>") $ do
    l <- typeParser isEnum
    _ <- char ','
    r <- typeParser isEnum
    return (StdPair l r)

mapp :: IsEnum -> Parsec String u Type
mapp isEnum =
  try $
  between (string "std::map<(") (string ")>") $ do
    l <- typeParser isEnum
    _ <- char ','
    r <- typeParser isEnum
    return (StdMap l r)

type IsEnum = String -> Bool

constp :: IsEnum -> Parsec String u Type
constp isEnum = try $ do
  _ <- string "q(const)."
  fmap Const (typeParser isEnum)

refp :: IsEnum -> Parsec String u Type
refp isEnum = try $ do
  _ <- string "r."
  fmap Ref (typeParser isEnum)

pointerp :: IsEnum -> Parsec String u Type
pointerp isEnum = try $ do
  _ <- string "p."
  fmap Pointer (typeParser isEnum)

arrayp :: IsEnum -> Parsec String u Type
arrayp isEnum = try $ do
  _ <- string "a()."
  fmap CArray (typeParser isEnum)

typeParser :: IsEnum -> Parsec String u Type
typeParser isEnum = do
  let genericType = UserType (Namespace ["casadi"]) (Name "GenericType")
      sx          = UserType (Namespace ["casadi"]) (Name "SX")
      mx          = UserType (Namespace ["casadi"]) (Name "MX")
      dmatrix     = UserType (Namespace ["casadi"]) (Name "DM")
      im          = UserType (Namespace ["casadi"]) (Name "IM")
      sparsity    = UserType (Namespace ["casadi"]) (Name "Sparsity")
  x <- choice
       [ p "bool" CBool
       , p "int" CInt
       , p "double" CDouble
       , p "casadi_int" CLongLong
       , p "casadi_index" CLongLong -- really casadi_int
       , p "long" CLong
       , p "long long" CLongLong
       , p "void" CVoid
--       , p "unsigned char" CUChar
       , p "char" CChar
       , p "std::string" StdString
       , p "std::size_t" CSize
       , p "size_t" CSize
       , p "std::ostream" StdOstream
       , p "casadi::Function::AuxOut" (StdMap StdString (StdVec StdString))
       , p "casadi::Dict" (StdMap StdString genericType)
       , p "casadi::GenericType::Dict" (StdMap StdString genericType)
       , p "casadi::DMDict" (StdMap StdString dmatrix)
       , p "casadi::SXDict" (StdMap StdString sx)
       , p "casadi::MXDict" (StdMap StdString mx)
       , p "casadi::SpDict" (StdMap StdString sparsity)
       , p "casadi::DMVector" (StdVec dmatrix)
       , p "casadi::DMVectorVector" (StdVec (StdVec dmatrix))
       , p "casadi::SXVector" (StdVec sx)
       , p "casadi::SXVectorVector" (StdVec (StdVec sx))
       , p "casadi::MXVector" (StdVec mx)
       , p "casadi::MXVectorVector" (StdVec (StdVec mx))
         -- some literals
       , ps [ "casadi::Matrix<(double)>"
            ,         "Matrix<(double)>"
            , "casadi::native_DM"
            ]
         dmatrix

       , ps [ "casadi::Matrix<(casadi::SXElem)>"
            ,         "Matrix<(casadi::SXElem)>"
            ]
         sx
       , ps [ "casadi::Matrix<(casadi_int)>"
            ,         "Matrix<(casadi_int)>"
            , "casadi::Matrix<(long long)>"
            ,         "Matrix<(long long)>"
            ]
         im

         
       , constp isEnum
       , refp isEnum
       , pointerp isEnum
       , arrayp isEnum
       , templatep isEnum "std::vector" StdVec
       , templatep isEnum "casadi::GenericExpression" id
       , templatep isEnum "casadi::GenericMatrix" id
       , templatep isEnum "casadi::SparsityInterface" id
       , templatep isEnum "casadi::PrintableObject" PrintableObject
       , pairp isEnum
       , mapp isEnum
       , classp isEnum
       ]
  return x

newtype Doc = Doc String deriving (Generic, Eq, Ord, Show)
instance Aeson.FromJSON Doc

newtype DocLink = DocLink String deriving (Generic, Eq, Ord, Show)
instance Aeson.FromJSON DocLink

data MethodKind = Constructor | Static | Normal deriving (Generic, Eq, Ord, Show)
instance Aeson.FromJSON MethodKind

newtype Name = Name String deriving (Generic, Show, Eq, Ord)
instance Aeson.FromJSON Name
unName :: Name -> String
unName (Name x) = x

newtype Namespace = Namespace [String] deriving (Show, Eq, Ord)

data Type = CInt
          | CDouble
          | CBool
          | CVoid
          | CSize
          | CLong
          | CLongLong
          | CUChar
          | CChar
          | CArray Type
          | StdOstream
          | StdString
          | UserType Namespace Name
          | CEnum Namespace Name
          | StdPair Type Type
          | StdMap Type Type
          | Ref Type
          | Pointer Type
          | Const Type
          | StdVec Type
          | PrintableObject Type
          deriving (Eq, Ord, Show)

class GetTypes a where
  getTypes :: a -> S.Set Type

data Class' a =
  Class'
  { classType :: ClassType
  , classMethods :: [Method' a]
  , classDocs :: Doc
  } deriving (Generic, Eq, Ord, Functor, Show)

data Class =
  Class
  { clType :: ClassType
  , clMethods :: [Methods]
  , clDocs :: Doc
  } deriving (Generic, Eq, Ord, Show)

classUnion :: Class' Type -> Class' Type -> Class' Type
classUnion x y
  | ctx /= cty = error $ "classUnion: got different types\n" ++ show (ctx, cty)
  | otherwise = ret
  where
    ret = x { classMethods = classMethods x ++ classMethods y
            , classDocs = docs
            }
    -- lets go with no docs for now
    docs
--      | classDocs x == classDocs y = classDocs x
      | otherwise = Doc ""

    ctx = classType x
    cty = classType y

instance GetTypes (Class' Type) where
  getTypes c = S.unions $ getTypes (classType c) : map getTypes (classMethods c)
instance GetTypes Class where
  getTypes c = S.unions $ getTypes (clType c) : map getTypes (clMethods c)

data Method' a =
  Method'
  { methodName :: Name
  , methodReturn :: a
  , methodParams :: [(a, String)]
  , methodKind :: MethodKind
  , methodDocs :: Doc
  } deriving (Generic, Eq, Ord, Functor, Show)
instance GetTypes (Method' Type) where
  getTypes m = S.fromList (methodReturn m : map fst (methodParams m))

data Method =
  Method
  { mName :: Name
  , mOthers :: Maybe Int
  , mReturn :: Type
  , mParams :: [(Type, SwigOutput)]
  , mKind :: MethodKind
  , mDocs :: Doc
  } deriving (Generic, Eq, Ord, Show)
instance GetTypes Method where
  getTypes m = S.fromList (mReturn m : map fst (mParams m))
type Methods = Either [Method] Method
instance GetTypes (Either [Method] Method) where
  getTypes (Left xs) = S.unions $ map getTypes xs
  getTypes (Right x) = getTypes x

data CppFunction' a =
  CppFunction'
  { funName :: String
  , funReturn :: a
  , funFriendwrap :: Bool
  , funParams :: [(a, String)]
  , funDocs :: Doc
  } deriving (Generic, Eq, Ord, Functor, Show)
instance GetTypes (CppFunction' Type) where
  getTypes f = S.fromList (funReturn f : map fst (funParams f))

data CppFunction =
  CppFunction
  { fName :: String
  , fOthers :: Maybe Int
  , fReturn :: Type
  , fFriendwrap :: Bool
  , fParams :: [(Type, SwigOutput)]
  , fDocs :: Doc
  } deriving (Generic, Eq, Ord, Show)
instance GetTypes CppFunction where
  getTypes f = S.fromList (fReturn f : map fst (fParams f))

data Enum' =
  Enum'
  { enumDocs :: Doc
  , enumEntries :: M.Map String EnumEntry
  } deriving (Generic, Eq, Ord, Show)

data EnumEntry =
  EnumEntry
  { enumEntryDocs :: Doc
  , enumEntryVal :: Int
  } deriving (Generic, Eq, Ord, Show)

data Tree a =
  Tree
  { treeFunctions :: [CppFunction' a]
  , treeClasses :: [Class' a]
  , treeEnums :: M.Map String Enum'
  , treeInheritance :: M.Map String [a]
  , treeIncludes :: [String]
  } deriving (Generic, Functor, Show)

data Module =
  Module
  { moduleFunctions :: [CppFunctions]
  , moduleClasses :: M.Map ClassType Class
  , moduleEnums :: M.Map Name Enum'
  , moduleInheritance :: M.Map ClassType (S.Set ClassType)
  , moduleIncludes :: [String]
  }


instance GetTypes Module where
  getTypes m = S.unions
               [ S.unions $ map getTypes (moduleFunctions m)
               , S.unions $ map getTypes $ M.keys (moduleClasses m)
               , S.unions $ map getTypes $ M.elems (moduleClasses m)
               ]


instance Aeson.FromJSON a => Aeson.FromJSON (CppFunction' a)
instance Aeson.FromJSON Enum'
instance Aeson.FromJSON EnumEntry
instance Aeson.FromJSON a => Aeson.FromJSON (Tree a)
instance Aeson.FromJSON a => Aeson.FromJSON (Method' a)
instance Aeson.FromJSON a => Aeson.FromJSON (Class' a)

getAllTypes :: Type -> S.Set Type
getAllTypes x@(StdPair tx ty) = S.insert x $ S.union (getAllTypes tx) (getAllTypes ty)
getAllTypes x@(StdMap tx ty)  = S.insert x $ S.union (getAllTypes tx) (getAllTypes ty)
getAllTypes x@(CArray t)          = S.insert x (getAllTypes t)
getAllTypes x@(Ref t)             = S.insert x (getAllTypes t)
getAllTypes x@(Pointer t)         = S.insert x (getAllTypes t)
getAllTypes x@(Const t)           = S.insert x (getAllTypes t)
getAllTypes x@(StdVec t)          = S.insert x (getAllTypes t)
getAllTypes x@CInt                 = S.singleton x
getAllTypes x@CDouble              = S.singleton x
getAllTypes x@CBool                = S.singleton x
getAllTypes x@CVoid                = S.singleton x
getAllTypes x@CSize                = S.singleton x
getAllTypes x@CLong                = S.singleton x
getAllTypes x@CLongLong            = S.singleton x
getAllTypes x@CUChar               = S.singleton x
getAllTypes x@CChar                = S.singleton x
getAllTypes x@StdOstream           = S.singleton x
getAllTypes x@StdString            = S.singleton x
getAllTypes x@(UserType {})        = S.singleton x
getAllTypes x@(CEnum {})           = S.singleton x
getAllTypes x@(PrintableObject {}) = S.singleton x


hasBadType :: GetTypes a => a -> Bool
hasBadType x = any badType $ S.unions $ map getAllTypes (S.toList (getTypes x))
  where
    badType :: Type -> Bool
    badType StdOstream = True
    badType (StdPair {}) = False
    badType (Pointer {}) = True
    badType (CArray {}) = True
    badType (UserType _ (Name name))
      | name `elem` ["Dictionary", "DpleStructure", "LrDpleStructure"] = True
    badType _ = False

startsWith :: String -> String -> Bool
startsWith x y = take n x == y
  where
    n = length y

hasBadMethodName :: String -> Bool
--hasBadMethodName xs = any (xs `startsWith`) ["operator "]
hasBadMethodName _ = False

filterMethods :: Class' Type -> Class' Type
filterMethods c = c { classMethods =
                         filter (not . hasBadMethodName . unName . methodName)
                         $ filter (not . hasBadType) (classMethods c)
                    }

overloadMethods :: Class' Type -> Class
overloadMethods c =
  Class
  { clType = classType c
  , clMethods = map m2ms (M.toList ms)
  , clDocs = Doc "" -- classDocs c
  }
  where
    ms :: M.Map Name [Method' Type]
    ms = M.fromListWith (++) $ map (\m -> (methodName m, [m])) (classMethods c)

    m2ms :: (Name, [Method' Type]) -> Methods
    m2ms (Name name, []) = error $ "method '" ++ name ++ "' has zero instances"
    m2ms (_, [f]) = Right (m2m f Nothing)
    m2ms (_, fs) = Left $ zipWith (\f k -> m2m f (Just k)) fs [0..]

    m2m :: Method' Type -> Maybe Int -> Method
    m2m f others =
      Method
      { mName = methodName f
      , mOthers = others
      , mKind = methodKind f
      , mReturn = methodReturn f
      , mParams = map (\(x,y) -> (x, toSwigOutput y)) (methodParams f)
      , mDocs = Doc "" -- methodDocs f
      }

type CppFunctions = Either [CppFunction] CppFunction
instance GetTypes (Either [CppFunction] CppFunction) where
  getTypes (Left xs) = S.unions $ map getTypes xs
  getTypes (Right x) = getTypes x

readModule :: FilePath -> IO Module
readModule jsonpath = do
  putStrLn $ "parsing " ++ jsonpath
  eitherTree <- fmap Aeson.eitherDecode' (BS.readFile jsonpath) :: IO (Either String (Tree String))
  tree0 <- case eitherTree of
    Left err -> error $ "module parsing failure: " ++ err
    Right ret -> return ret

  let isEnum :: String -> Bool
      isEnum = (`S.member` (S.fromList (M.keys (treeEnums (tree0)))))

      tree' :: Tree Type
      tree' = fmap (parseType isEnum) tree0

      treeToModule :: Tree Type -> Module
      treeToModule tree =
        Module
        { moduleFunctions = map f2fs (M.toList functions0) :: [CppFunctions]
        , moduleClasses = M.mapWithKey addPrint
                          $ M.map (overloadMethods . filterMethods)
                          $ M.fromListWith classUnion
--                          $ filter lool
                          $ filter (not . isPrintableObject . fst)
                          $ filter (not . hasBadType . fst)
                          $ map (\c -> (classType c, c)) $ treeClasses tree
        , moduleEnums        = M.fromList $ map (first Name) $ M.toList $ treeEnums tree
        , moduleIncludes     = treeIncludes tree
        , moduleInheritance  = M.filterWithKey (\k _ -> not (isPrintableObject k)) newInheritance
        }
        where
--          lool :: (ClassType, Class' Type) -> Bool
--          lool (ClassType (StdMap {}), _) = False
--          lool _ = True

          printableObjects :: S.Set ClassType
          printableObjects = S.fromList $
                             map (\(ClassType (PrintableObject x)) -> ClassType x) $
                             filter isPrintableObject $
                             map classType (treeClasses tree)

          addPrint :: ClassType -> Class -> Class
          addPrint ct c
            | S.member ct printableObjects = c { clMethods = clMethods c ++ printMethods }
            | otherwise = c
            where
              printMethods :: [Methods]
              printMethods = [ ms "getRepresentation"
                             , ms "getDescription"
                             ]
                where
                  ms :: String -> Methods
                  ms n = Right $
                         Method
                         { mName = Name n
                         , mOthers = Nothing
                         , mReturn = StdString
                         , mParams = []
                         , mKind = Normal
                         , mDocs = Doc ""
                         }

          isPrintableObject (ClassType (PrintableObject {})) = True
          isPrintableObject _ = False

          newInheritance :: M.Map ClassType (S.Set ClassType)
          newInheritance =
            M.map (S.filter (not . isPrintableObject))
            $ M.map (S.filter (not . hasBadType) . S.fromList)
            $ M.fromListWith (++)
            $ filter (not . hasBadType . fst)
            $ map (\(k,v) -> (ClassType (parseType isEnum k), map ClassType v))
            $ M.toList (treeInheritance tree)

          functions0 :: M.Map String [CppFunction' Type]
          functions0 =
            M.map (filter (not . hasBadType) . filter (not . hasBadMethodName . funName))
            $ M.fromListWith (++) $ map (\x -> (funName x, [x])) $ treeFunctions tree

          f2fs :: (String, [CppFunction' Type]) -> CppFunctions
          f2fs (_name, []) = Left [] -- error $ "function '" ++ name ++ "' has zero instances"
          f2fs (_, [f]) = Right (f2f f Nothing)
          f2fs (_, fs) = Left $ zipWith (\f k -> f2f f (Just k)) fs [0..]

          f2f :: CppFunction' Type -> Maybe Int -> CppFunction
          f2f f others =
            CppFunction
            { fName = funName f
            , fOthers = others
            , fReturn = funReturn f
            , fFriendwrap = funFriendwrap f
            , fParams = map (\(x,y) -> (x, toSwigOutput y)) (funParams f)
            , fDocs = Doc "" -- funDocs f
            }

      module0 = treeToModule tree'

--      pmi :: M.Map ClassType (S.Set ClassType) -> IO ()
--      pmi xs = do
--        putStrLn "=============================================================================="
--        let pmi' (ct, inh) = do
--              putStrLn ""
--              print ct
--              mapM_ (putStrLn . ("      " ++) . show) (S.toList inh)
--        mapM_ pmi' (M.toList xs)
--  mapM_ (pmi . moduleInheritance) modules

  return module0


main :: IO ()
main = do
  let jsonpath = "/home/greghorn/casadi/build/swig/json/casadi.json"
  _ <- readModule jsonpath
  return ()
