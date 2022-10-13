module Data.PrettyShow where

class PrettyShow a where
  prettyShow :: a -> String
