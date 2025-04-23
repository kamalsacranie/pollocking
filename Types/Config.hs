{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

module Types.Config
  ( Config (NC, fill, flexWidth, flexHeight, heightBound, widthBound),
    upper,
    getUpper,
    lower,
    getLower,
    bounded,
  )
where

import Control.Applicative (Applicative (liftA2))
import Data.Word (Word16)
import GHC.Base (Alternative ((<|>)))

data Config = NC
  { heightBound :: Bound,
    widthBound :: Bound,
    flexWidth :: Maybe Float,
    flexHeight :: Maybe Float,
    fill :: Char
  }
  deriving (Show, Eq)

instance Semigroup Config where
  l <> r =
    NC
      { heightBound = l.heightBound <> r.heightBound,
        widthBound = l.widthBound <> r.widthBound,
        flexWidth = max <$> l.flexWidth <*> r.flexWidth <|> l.flexWidth <|> r.flexWidth,
        flexHeight = max <$> l.flexHeight <*> r.flexHeight <|> l.flexHeight <|> r.flexHeight,
        fill = if fill l == ' ' then fill r else fill l
      }

instance Monoid Config where
  mempty = NC {heightBound = mempty, widthBound = mempty, fill = ' ', flexWidth = Nothing, flexHeight = Nothing}

------------------------------------------------------

data Bound = B {_lower :: Word16, _upper :: Word16}
  deriving (Show, Eq)

upper :: Bound -> Word16 -> Bound
upper b u =
  if b._upper < b._lower
    then error "Upper bound is less than lower bound"
    else b {_upper = u}

getUpper :: Bound -> Word16
getUpper b = b._upper

lower :: Bound -> Word16 -> Bound
lower b l =
  if b._upper < b._lower
    then error "Lower bound is greater than upper bound"
    else b {_lower = l}

getLower :: Bound -> Word16
getLower b = b._lower

bounded :: Bound -> Word16 -> Word16
bounded B {..} = min _upper . max _lower

instance Semigroup Bound where
  B l1 u1 <> B l2 u2 =
    let b =
          B
            { _lower = min l1 l2,
              _upper = max u1 u2
            }
     in if b._upper < b._lower
          then error "Upper bound is less than lower bound"
          else b

instance Monoid Bound where
  mempty =
    B
      { _lower = 0,
        _upper = maxBound
      }
