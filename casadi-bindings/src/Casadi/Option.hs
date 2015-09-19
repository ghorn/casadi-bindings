{-# OPTIONS_GHC -Wall #-}
{-# Language GADTs #-}

-- todo(greg): merge this with Casadi.GenericC
module Casadi.Option
       ( Opt(..)
       , GenericC(..)
       , GenericType
       , getOption
       ) where

import Casadi.Core.Classes.OptionsFunctionality
  ( OptionsFunctionalityClass
  , optionsFunctionality_hasOption, optionsFunctionality_getOption
  )
import Casadi.GenericC

getOption :: (OptionsFunctionalityClass a, GenericC b) => a -> String -> IO (Maybe b)
getOption f name = do
  has <- optionsFunctionality_hasOption f name
  if has
    then optionsFunctionality_getOption f name >>= fromGeneric
    else return Nothing
