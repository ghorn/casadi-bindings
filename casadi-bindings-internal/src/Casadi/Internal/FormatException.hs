{-# OPTIONS_GHC -Wall #-}

module Casadi.Internal.FormatException
       ( formatException
       ) where

formatException :: String -> String
formatException = rstrip . lstrip

rstrip :: String -> String
rstrip xs = reverse (lstrip (reverse xs))

lstrip :: String -> String
lstrip ('\n':xs) = lstrip xs
lstrip xs = xs
