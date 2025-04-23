module Types.Variant (Variant (Row, Col), defaultRowVariant, defaultColVariant, isRow, isCol) where

import Types.ColConfig (ColConfig)
import Types.RowConfig (RowConfig)

data Variant = Row RowConfig | Col ColConfig
  deriving (Show, Eq)

instance Semigroup Variant where
  Row l <> Row r = Row (l <> r)
  Col l <> Col r = Col (l <> r)
  _ <> _ = error "Cannot combine different variants"

defaultRowVariant = Row mempty

defaultColVariant = Col mempty

isRow Row {} = True
isRow _ = False

isCol Col {} = True
isCol _ = False
