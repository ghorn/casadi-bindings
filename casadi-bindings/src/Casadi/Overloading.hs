{-# OPTIONS_GHC -Wall #-}

module Casadi.Overloading
       ( ArcTan2(..)
       , Erf(..)
       , Fmod(..)
       , SymOrd(..)
       , lt, gt
       , ifLeqThen, ifGeqThen, ifEqThen, ifLtThen, ifGtThen
       ) where

import Data.Fixed ( mod' )
import SpatialMath ( ArcTan2(..) )

-- | doesn't require Real, used for overloading symbolics
class Fmod a where
  fmod :: a -> a -> a

instance Fmod Double where fmod = mod'
instance Fmod Float where fmod = mod'

-- | error function
class Erf a where
  erf :: a -> a
  erfinv :: a -> a


-- | Ord, but returns a 1 or a 0 instead of True or False.
-- If you optimize functions which include these, you are responsible for
-- keeping things smooth enough.
--
-- >>> 41 `leq` 42 :: Double
-- 1.0
--
-- >>> 42 `leq` 42 :: Double
-- 1.0
--
-- >>> 43 `leq` 42 :: Double
-- 0.0
--
-- >>> 41 `geq` 42 :: Double
-- 0.0
--
-- >>> 42 `geq` 42 :: Double
-- 1.0
--
-- >>> 43 `geq` 42 :: Double
-- 1.0
--
-- >>> 41 `eq` 42 :: Double
-- 0.0
--
-- >>> 42 `eq` 42 :: Double
-- 1.0
--
-- >>> 43 `eq` 42 :: Double
-- 0.0
class Num a => SymOrd a where
  -- | @<=@
  leq :: a -> a -> a
  -- | @>=@
  geq :: a -> a -> a
  -- | @==@
  eq :: a -> a -> a

instance SymOrd Double where
  x `leq` y = if x <= y then 1 else 0
  x `geq` y = if x >= y then 1 else 0
  x  `eq` y = if x == y then 1 else 0
instance SymOrd Float where
  x `leq` y = if x <= y then 1 else 0
  x `geq` y = if x >= y then 1 else 0
  x  `eq` y = if x == y then 1 else 0

-- | @<@
-- >>> 41 `lt` 42 :: Double
-- 1.0
--
-- >>> 42 `lt` 42 :: Double
-- 0.0
--
-- >>> 43 `lt` 42 :: Double
-- 0.0
lt :: SymOrd a => a -> a -> a
lt x y = 1 - geq x y

-- | @>@
-- >>> 41 `gt` 42 :: Double
-- 0.0
--
-- >>> 42 `gt` 42 :: Double
-- 0.0
--
-- >>> 43 `gt` 42 :: Double
-- 1.0
gt :: SymOrd a => a -> a -> a
gt x y = 1 - leq x y

-- | @ifLeqThen x y ifX ifY == if x <= y then ifX else ifY@
-- >>> ifLeqThen 41 42 100 200 :: Double
-- 100.0
--
-- >>> ifLeqThen 42 42 100 200 :: Double
-- 100.0
--
-- >>> ifLeqThen 43 42 100 200 :: Double
-- 200.0
--
ifLeqThen :: SymOrd a => a -> a -> a -> a -> a
ifLeqThen = ifCondThen leq


-- | @ifGeqThen x y ifX ifY == if x >= y then ifX else ifY@
-- >>> ifGeqThen 41 42 100 200 :: Double
-- 200.0
--
-- >>> ifGeqThen 42 42 100 200 :: Double
-- 100.0
--
-- >>> ifGeqThen 43 42 100 200 :: Double
-- 100.0
--
ifGeqThen :: SymOrd a => a -> a -> a -> a -> a
ifGeqThen = ifCondThen geq


-- | @ifEqThen x y ifX ifY == if x == y then ifX else ifY@
-- >>> ifEqThen 41 42 100 200 :: Double
-- 200.0
--
-- >>> ifEqThen 42 42 100 200 :: Double
-- 100.0
--
-- >>> ifEqThen 43 42 100 200 :: Double
-- 200.0
--
ifEqThen :: SymOrd a => a -> a -> a -> a -> a
ifEqThen = ifCondThen eq


-- | @ifLtThen x y ifX ifY == if x < y then ifX else ifY@
-- >>> ifLtThen 41 42 100 200 :: Double
-- 100.0
--
-- >>> ifLtThen 42 42 100 200 :: Double
-- 200.0
--
-- >>> ifLtThen 43 42 100 200 :: Double
-- 200.0
--
ifLtThen :: SymOrd a => a -> a -> a -> a -> a
ifLtThen = ifCondThen lt


-- | @ifGtThen x y ifX ifY == if x > y then ifX else ifY@
-- >>> ifGtThen 41 42 100 200 :: Double
-- 200.0
--
-- >>> ifGtThen 42 42 100 200 :: Double
-- 200.0
--
-- >>> ifGtThen 43 42 100 200 :: Double
-- 100.0
--
ifGtThen :: SymOrd a => a -> a -> a -> a -> a
ifGtThen = ifCondThen gt


ifCondThen :: Num a => (a -> a -> a) -> a -> a -> a -> a -> a
ifCondThen cond x y if' then' = true*if' + (1 - true)*then'
  where
    true = cond x y
