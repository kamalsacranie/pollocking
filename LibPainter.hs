{-# LANGUAGE DuplicateRecordFields #-}

module LibPainter where

import Types.ColConfig (ColConfig)
import Types.Config
import Types.Element
import Types.Metadata (Metadata)
import Types.RowConfig (RowConfig)
import Types.Variant (Variant (Col, Row), defaultColVariant, defaultRowVariant)

rowWith :: Config -> RowConfig -> [Element] -> Element
rowWith gc rc children = (defaultNode (Row rc)) {config = gc, children}

defaultRowWith :: Config -> [Element] -> Element
defaultRowWith gc = rowWith gc mempty

row :: [Element] -> Element
row = defaultRowWith mempty

colWith :: Config -> ColConfig -> [Element] -> Element
colWith gc cc children = (defaultNode (Col cc)) {config = gc, children}

defaultColWith :: Config -> [Element] -> Element
defaultColWith gc = colWith gc mempty

col :: [Element] -> Element
col children = (defaultNode defaultColVariant) {children}

char :: Char -> Element
char c = Leaf {md = mempty, c}

text :: [Char] -> Element
text = row . map char

horizontalSpacer f = defaultRowWith (mempty {fill = ' ', flexWidth = Just f, heightBound = upper mempty 0, widthBound = upper mempty 0}) []

verticalSpacer f = defaultColWith (mempty {fill = ' ', flexHeight = Just f, heightBound = upper mempty 0, widthBound = upper mempty 0}) []

horizontalRule f = defaultRowWith (mempty {fill = '─', flexWidth = Just f, heightBound = lower mempty 1}) []

verticalRule f = defaultColWith (mempty {fill = '│', flexHeight = Just f, widthBound = lower mempty 1}) []
