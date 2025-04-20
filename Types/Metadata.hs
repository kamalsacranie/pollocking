{-# LANGUAGE OverloadedRecordDot #-}

module Types.Metadata (Metadata (MD, size, position)) where

import Types.Position (Position)
import Types.Size (Size)

data Metadata = MD
  { size :: Size,
    position :: Position
  }
  deriving (Show, Eq)

instance Semigroup Metadata where
  l <> r = MD {size = l.size <> r.size, position = l.position <> r.position}

instance Monoid Metadata where
  mempty = MD {size = mempty, position = mempty}
