{-# LANGUAGE OverloadedRecordDot #-}

module Types.Size (Size(S, width, height)) where

import Data.Word (Word16)

data Size = S {width :: Word16, height :: Word16}
  deriving (Show, Eq)

instance Semigroup Size where
  l <> r = S {width = max l.width r.width, height = max l.height r.height}

instance Monoid Size where
  mempty = S {width = 0, height = 0}
