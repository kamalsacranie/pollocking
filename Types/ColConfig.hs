module Types.ColConfig (ColConfig) where

data ColConfig = CC {}
  deriving (Show, Eq)

instance Semigroup ColConfig where
  _ <> _ = CC {}

instance Monoid ColConfig where
  mempty = CC {}
