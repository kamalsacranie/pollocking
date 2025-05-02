{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}

module LibPainter where

import Data.Set (Set, insert)
import Types.ColConfig (ColConfig)
import Types.Config
import Types.Element
import Types.Metadata (Metadata (style))
import Types.RowConfig (RowConfig)
import Types.Style (Color, Style (textColor, textStyles), TextStyle (Bold, Italic, Underline), bgColor)
import Types.Variant (Variant (Col, Row), defaultColVariant, defaultRowVariant)

nodeWith :: Config -> Variant -> [Element] -> Element
nodeWith gc v children = (defaultNode v) {config = gc, children}

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
col children = (defaultNode defaultColVariant) {children}

char :: Char -> Element
char c = Leaf {md = mempty, s = [c]}

text :: [Char] -> Element
text =
  ( \case
      [] -> row []
      [s] -> Leaf mempty $ unwords . words $ handleEscapeCode s
      lines -> col $ map text lines
  )
    . lines
  where
    handleEscapeCode s = case s of
      [] -> ""
      [c] -> [c]
      '\x1b' : tail -> "\\x1b" ++ handleEscapeCode tail
      head : tail -> head : handleEscapeCode tail

horizontalSpacer f = defaultRowWith (mempty {fill = ' ', flexWidth = Just f, heightBound = upper mempty 0, widthBound = upper mempty 0}) []

verticalSpacer f = defaultColWith (mempty {fill = ' ', flexHeight = Just f, heightBound = upper mempty 0, widthBound = upper mempty 0}) []

horizontalRule f = defaultRowWith (mempty {fill = '─', flexWidth = Just f, heightBound = lower mempty 1}) []

verticalRule f = defaultColWith (mempty {fill = '│', flexHeight = Just f, widthBound = lower mempty 1}) []

bgColor :: Color -> Element -> Element
bgColor c = flip setMetadata (\md -> md {style = md.style {bgColor = Just c}})

textColor :: Color -> Element -> Element
textColor c = flip setMetadata (\md -> md {style = md.style {textColor = Just c}})

textItalic :: Element -> Element
textItalic = flip setMetadata (\md -> md {style = md.style {textStyles = insert Italic md.style.textStyles}})

textUnderline :: Element -> Element
textUnderline = flip setMetadata (\md -> md {style = md.style {textStyles = insert Underline md.style.textStyles}})

textBold :: Element -> Element
textBold = flip setMetadata (\md -> md {style = md.style {textStyles = insert Bold md.style.textStyles}})
