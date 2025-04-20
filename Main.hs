{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import Data.List (intercalate)
import Data.Maybe (fromMaybe, mapMaybe)
import LibPainter (col, horizontalRule, horizontalSpacer, row, text, verticalRule, verticalSpacer)
import System.Process (readProcess)
import Text.Read (readMaybe)
import Types.Element
  ( Element (Node, md),
    positionElements,
    render,
    sizeFixedHorizontally,
    sizeFixedVertically,
    sizeFlexHorizontally,
    sizeFlexVertically,
  )
import Types.Metadata (size)
import Types.Size (Size (height, width))

tree0 =
  col
    [ col
        [ text "This is kinda crazy bro?? isn't it cool that I have this thingy??",
          text "this might get a bit annoying"
        ],
      row [text "╭", horizontalRule 1, text "╮"],
      row
        [ verticalRule 1,
          col
            [ text "This is the first column",
              text "World"
            ],
          verticalRule 1,
          col
            [ text "This is the second column",
              text "World"
            ],
          horizontalSpacer 1,
          verticalRule 1
        ],
      row [text "╰", horizontalRule 1, text "╯"],
      verticalSpacer 1,
      horizontalSpacer 1
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
            . sizeFlexVertically (fromIntegral screenHeight)
            . sizeFlexHorizontally (fromIntegral screenWidth)
            . sizeFixedVertically
            . sizeFixedHorizontally
        )
          tree0
   in putStrLn $ intercalate "\n" $ reverse $ drop 2 $ reverse $ render tree
