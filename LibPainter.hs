{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}

module LibPainter ( fillVertical,
    fillHorizontal,
    bgColor,
    col,
    horizontalRule,
    horizontalSpacer,
    row,
    text,
    textBold,
    textColor,
    textItalic,
    textUnderline,
    verticalRule,
    verticalSpacer,
  ) where

import Data.Set (insert)
import Types.ColConfig (ColConfig)
import Types.Config
    ( Config(widthBound, flexWidth, heightBound, fill, flexHeight),
      lower,
      upper )
import Types.Element ( Element(..), setMetadata )
import Types.Metadata (Metadata (style))
import Types.RowConfig (RowConfig)
import Types.Style (Color, Style (textStyles), TextStyle (Bold, Italic, Underline))
import qualified Types.Style as TSty (Style (textColor), bgColor)
import Types.Variant (Variant (Col, Row))

nodeWith :: Config -> Variant -> [Element] -> Element
nodeWith gc v children = Node {md = mempty, variant = v, config = gc, children = children}

rowWith :: Config -> RowConfig -> [Element] -> Element
rowWith gc = nodeWith gc . Row

defaultRowWith :: Config -> [Element] -> Element
defaultRowWith gc = rowWith gc mempty

row :: [Element] -> Element
row = defaultRowWith mempty

colWith :: Config -> ColConfig -> [Element] -> Element
colWith gc = nodeWith gc . Col

defaultColWith :: Config -> [Element] -> Element
defaultColWith gc = colWith gc mempty

col :: [Element] -> Element
col = defaultColWith mempty

text :: [Char] -> Element
text =
  ( \case
      [] -> row []
      [s] -> Leaf mempty False $ unwords . words $ handleEscapeCode s
      textLines -> col $ map text textLines
  )
    . lines
  where
    handleEscapeCode = \case
      [] -> ""
      [c] -> [c]
      '\x1b' : tl -> "\\x1b" ++ handleEscapeCode tl
      hd : tl -> hd : handleEscapeCode tl

fillHorizontal :: Float -> Element -> Element
fillHorizontal f node@Node {config} = node {config = config {flexWidth = Just f}}
fillHorizontal f leaf = fillHorizontal f $ rowWith mempty mempty [leaf]

fillVertical :: Float -> Element -> Element
fillVertical f node@Node {config} = node {config = config {flexHeight = Just f}}
fillVertical _ _ = undefined

-- TODO: Make versions that are sized with actual Ints
horizontalSpacer :: Float -> Element
horizontalSpacer f = fillVertical 1 $ defaultRowWith (mempty {fill = Nothing, flexWidth = Just f, heightBound = upper mempty 0, widthBound = upper mempty 0}) []

verticalSpacer :: Float -> Element
verticalSpacer f = fillHorizontal 1 $ defaultColWith (mempty {fill = Nothing, flexHeight = Just f, heightBound = upper mempty 0, widthBound = upper mempty 0}) []

horizontalRule :: Float -> Element
horizontalRule f = defaultRowWith (mempty {fill = Just '─', flexWidth = Just f, heightBound = lower mempty 1}) []

verticalRule :: Float -> Element
verticalRule f = defaultColWith (mempty {fill = Just '│', flexHeight = Just f, widthBound = lower mempty 1}) []

bgColor :: Color -> Element -> Element
bgColor c = flip setMetadata (\md -> md {style = md.style {TSty.bgColor = Just c}})

textColor :: Color -> Element -> Element
textColor c = flip setMetadata (\md -> md {style = md.style {TSty.textColor = Just c}})

textItalic :: Element -> Element
textItalic = flip setMetadata (\md -> md {style = md.style {textStyles = insert Italic md.style.textStyles}})

textUnderline :: Element -> Element
textUnderline = flip setMetadata (\md -> md {style = md.style {textStyles = insert Underline md.style.textStyles}})

textBold :: Element -> Element
textBold = flip setMetadata (\md -> md {style = md.style {textStyles = insert Bold md.style.textStyles}})
