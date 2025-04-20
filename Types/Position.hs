{-# LANGUAGE OverloadedRecordDot #-}

module Types.Position (Position(P, x, y)) where

data Position = P {x :: Int, y :: Int}
  deriving (Show, Eq)

instance Semigroup Position where
  l <> r = P {x = max l.x r.x, y = max l.y r.y}

instance Monoid Position where
  mempty = P 0 0
