{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import Data.List (intercalate)
import Data.Maybe (fromMaybe, mapMaybe)
import LibPainter (col, horizontalRule, horizontalSpacer, row, text, verticalRule, verticalSpacer)
import System.Process (readProcess)
import Text.Read (readMaybe)
import Types.Config (Config (flexHeight, flexWidth))
import Types.Element
  ( Element (Leaf, Node, children, config, md),
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
    [ row [text "╭", horizontalRule 1, text "╮"],
      row [verticalRule 1, e, verticalRule 1],
      row [text "╰", horizontalRule 1, text "╯"]
    ]

fillHorizontal f node@(Node {config}) = node {config = config {flexWidth = Just f}}

fillVertical f node@(Node {config}) = node {config = config {flexHeight = Just f}}

t =
  fillHorizontal 1 . border $
    fillHorizontal 1 $
      row
        [ col
            [ text "This is the first column",
              text "World"
            ],
          horizontalSpacer 0.5,
          verticalRule 1,
          horizontalSpacer 0.5,
          col
            [ text "This is the second column",
              row [horizontalSpacer 1, text "World"]
            ]
        ]

tree0 =
  col
    [ col
        [ text "This is kinda crazy bro?? isn't it cool that I have this thingy??",
          text "this might get a bit annoying"
        ],
      t,
      t,
      t,
      fillVertical 0.89 t
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
            . (\case node@(Node {md}) -> node {md = md {size = md.size {width = fromIntegral screenWidth, height = fromIntegral screenHeight - 1}}})
            . sizeFixedVertically
            . sizeFixedHorizontally
        )
          tree0
   in (putStrLn . intercalate "\n" . render $ tree)

-- print $ tree.md

-- print $ map (\x -> x.md) $ (drop 2 . take 3) tree.children
