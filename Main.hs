{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Data.List (intercalate)
import Data.Maybe (fromMaybe, mapMaybe)
import LibPainter (col, horizontalRule, horizontalSpacer, row, text, verticalRule, verticalSpacer)
import System.Process (readProcess)
import Text.Read (readMaybe)
import Types.Config (Config (fill, flexHeight, flexWidth))
import Types.Element
  ( Element (Leaf, Node, c, children, config, md),
    positionElements,
    render,
    sizeFixedHorizontally,
    sizeFixedVertically,
    sizeFlexHorizontally,
    sizeFlexVertically,
  )
import Types.Metadata (size)
import Types.Size (Size (height, width))

border :: Element -> Element
border e =
  col
    [ (fillHorizontal 1 . row) [text "╭", horizontalRule 0.5, text "444", horizontalRule 0.5, text "╮"],
      (fillVertical 1 . fillHorizontal 1 . row) [verticalRule 1, fillVertical 1 . fillHorizontal 1 $ e, verticalRule 1],
      (fillHorizontal 1 . row) [text "╰", horizontalRule 1, text "╯"]
    ]

fillHorizontal f node@(Node {config}) = node {config = config {flexWidth = Just f}}

fillVertical f node@(Node {config}) = node {config = config {flexHeight = Just f}}

fillBackground :: Char -> Element -> Element
fillBackground x node@(Node {..}) = node {config = config {fill = x}}
fillBackground x leaf@(Leaf {}) = leaf {c = x}

t =
  border $
    row
      [ (fillVertical 1 . col)
          [ verticalSpacer 0.5,
            text "This is the first column",
            fillHorizontal 1 . padCenter $ text "World",
            verticalSpacer 0.5
          ],
        horizontalSpacer 0.5,
        verticalRule 1,
        horizontalSpacer 0.5,
        col
          [ text "This is the second column",
            (fillHorizontal 1 . row) [horizontalSpacer 1, text "World"]
          ]
      ]

padCenter e = row [horizontalSpacer 0.5, e, horizontalSpacer 0.5]

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
  (screenHeight, screenWidth) <- getTerminalSize >>= (\(lines, cols) -> return (lines, cols)) . fromMaybe (error "Could not obtain terminal size. Are you running in a tty?")
  let tree =
        ( positionElements
            . (\tree -> sizeFlexVertically (fromIntegral (case tree of Node {md} -> md; Leaf {md} -> md).size.height) tree)
            . (\tree -> sizeFlexHorizontally (fromIntegral (case tree of Node {md} -> md; Leaf {md} -> md).size.width) tree)
            . (\case node@(Node {md}) -> node {md = md {size = md.size {width = fromIntegral screenWidth, height = 20}}})
            . sizeFixedVertically
            . sizeFixedHorizontally
        )
          tree0
   in (putStrLn . intercalate "\n" . render $ tree)

-- print $ tree.md

-- print $ map (\x -> x.md) $ drop 1 tree.children
