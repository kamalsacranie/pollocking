{-# LANGUAGE OverloadedRecordDot #-}

module Types.Metadata (Metadata (MD, size, position, style)) where

import Types.Position (Position)
import Types.Size (Size)
import Types.Style (Style)

-- Style doesn't really fit here but I have no better spot for it without
-- rewriting the types.
-- Ideally, metadata would only include stuff that is important for the drawing
-- and rendering of the tree. Style is used when rendering to the terminal
-- buffer but I would say semantically, it is better put in a config type. I
-- think this will be possible when we introduce the leaf config type that we
-- will need for wrappable text.
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
