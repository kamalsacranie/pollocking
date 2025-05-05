{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad.State (runState)
import Data.List (intercalate)
import Data.Maybe (fromMaybe, mapMaybe)
import LibPainter (bgColor, col, horizontalRule, horizontalSpacer, row, text, textBold, textColor, textItalic, textUnderline, verticalRule, verticalSpacer)
import System.Process (readProcess)
import Text.Read (readMaybe)
import Types.Config (Config (fill, flexHeight, flexWidth))
import Types.Element
  ( CanvasSequece (In, Out),
    Element (Leaf, Node, children, config, md, s),
    normaliseFlexorFloats,
    positionElements,
    render,
    setMetadata,
    sizeFixedHorizontally,
    sizeFixedVertically,
    sizeFlexHorizontally,
    sizeFlexVertically,
  )
import Types.Metadata (size)
import Types.Size (Size (height, width))
import Types.Style (Color (..))

fillSize :: Float -> Element -> Element
fillSize f = fillVertical f . fillHorizontal f

border :: Element -> Element
border e =
  col
    [ (fillHorizontal 1 . row) [text "╭", horizontalRule 1, text "╮"],
      (fillSize 1 . row) [verticalRule 1, fillSize 1 e, verticalRule 1],
      (fillHorizontal 1 . row) [text "╰", horizontalRule 1, text "╯"]
    ]

fillHorizontal :: Float -> Element -> Element
fillHorizontal f node@(Node {config}) = node {config = config {flexWidth = Just f}}
fillHorizontal _ _ = undefined

fillVertical :: Float -> Element -> Element
fillVertical f node@(Node {config}) = node {config = config {flexHeight = Just f}}
fillVertical _ _ = undefined

fillBackground :: Char -> Element -> Element
fillBackground x node@(Node {..}) = node {config = config {fill = x}}
fillBackground x leaf@(Leaf {}) = leaf {s = [x]}

t :: Element
t =
 border $
    (fillBackground '.' . bgColor White . textColor Black . row)
      [ (fillVertical 1 . col)
          [ verticalSpacer 0.5,
            (textBold . textItalic . text) "This is the first column",
            fillHorizontal 1 . padCenter $ text "World",
            verticalSpacer 0.5
          ],
        horizontalSpacer 0.5,
        verticalRule 1,
        horizontalSpacer 0.5,
        col
          [ (textUnderline . textBold . text) "This is the second column",
            (fillHorizontal 1 . row) [horizontalSpacer 1, text "World"]
          ]
      ]

padCenter :: Element -> Element
padCenter e = row [horizontalSpacer 0.5, e, horizontalSpacer 0.5]

tree0 :: Element
tree0 =
  col
    [ col
        [ text "This is kinda crazy bro?? isn't it cool that I have this thingy??",
          text "this might get a bit annoying"
        ],
      (fillVertical 1 . fillHorizontal 1) (padCenter (fillVertical 1 t))
    ]

getTerminalSize :: IO (Maybe (Int, Int))
getTerminalSize =
  ( \case
      [cols, rows] -> Just (cols, rows)
      _ -> Nothing
  )
    . (mapMaybe readMaybe :: [String] -> [Int])
    -- Maybe use a function that doesn't throw here? or handle the throw I guess
    <$> mapM (flip (readProcess "tput") "") [["lines"], ["cols"]]

main :: IO ()
main = do
  putStr "\ESC[?1049h\ESC[H" -- enter fullscreen terminal mode; go to the top left
  (screenHeight, screenWidth) <- getTerminalSize >>= (\(termLines, termCols) -> return (termLines, termCols)) . fromMaybe (error "Could not obtain terminal size. Are you running in a tty?")
  let processedTree =
        ( positionElements
            . (\tree -> sizeFlexVertically (fromIntegral (case tree of Node {md} -> md; Leaf {md} -> md).size.height) tree)
            . (\tree -> sizeFlexHorizontally (fromIntegral (case tree of Node {md} -> md; Leaf {md} -> md).size.width) tree)
            . (`setMetadata` (\md -> md {size = md.size {width = fromIntegral screenWidth, height = fromIntegral screenHeight - 1}}))
            . sizeFixedVertically
            . sizeFixedHorizontally
            . normaliseFlexorFloats
        )
          tree0
   in putStrLn
        $ intercalate "\n"
        $ map
          ( intercalate ""
              . map
                ( \case
                    Out chars -> chars
                    In chars -> chars
                )
          )
          . (\e -> fst $ runState (render e) mempty)
        $ processedTree

-- putStr "\x1b[?1049l" -- exit fullscreen mode

-- putStr "\ESC[H" -- put cursor in top left
-- putStr "\r\ESC[KNew 1\n" -- Overwrite line 1
-- putStr "\r\ESC[KNew 2\n" -- Overwrite line 2
-- putStr "\r\ESC[KNew 3\n" -- Overwrite line 3
