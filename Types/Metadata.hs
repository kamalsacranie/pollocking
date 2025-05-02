{-# LANGUAGE OverloadedRecordDot #-}

module Types.Metadata (Metadata (MD, size, position, style)) where

import Types.Position (Position)
import Types.Size (Size)
import Types.Style (Style)

data Metadata = MD
  { size :: Size,
    position :: Position,
    style :: Style
  }
  deriving (Show, Eq)

instance Semigroup Metadata where
  l <> r = MD {size = l.size <> r.size, position = l.position <> r.position, style = l.style <> r.style}

instance Monoid Metadata where
  mempty = MD {size = mempty, position = mempty, style = mempty}
