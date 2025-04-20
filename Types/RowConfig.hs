module Types.RowConfig (RowConfig (RC)) where

data RowConfig = RC {}
  deriving (Show, Eq)

instance Semigroup RowConfig where
  _ <> _ = RC {}

instance Monoid RowConfig where
  mempty = RC {}
