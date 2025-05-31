{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import System.IO (hFlush, stdout)
import Data.Maybe (fromMaybe, mapMaybe)
import Control.Arrow ((>>>))
import LibPainter
  ( fillVertical,
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
  )
import System.Process (readProcess)
import Text.Read (readMaybe)
import Types.Config (Config (fill))
import Types.Element
  ( Element (Leaf, Node, wrap, children, config, s),
    normaliseFlexorFloats,
    positionElements,
    clipHorizontally,
    cascadeStyles,
    cascadeFillCharacters,
    render,
    showCanvas,
    wrapText,
    setMetadata,
    sizeFixedHorizontally,
    sizeFixedVertically,
    sizeFlexHorizontally,
    sizeFlexVertically,
  )
import Types.Style (Color (..))
import Types.Metadata
import Types.Size

-- Lowkey I kinda want to change the whole architecure to revolve the idea that
-- a tree is a function that takes a width and then returns an actual tree with
-- everything sized.
-- For example, the `text` funciton would have the signature: `text :: Word16 ->
-- [Char] -> Element` because the true number of children will only be known at
-- render time where we know the screen size. So it would be cool to have
-- instead of what we have now, we would have a state monad acutally where the
-- state would be the size of the parent node.
-- The monad would end up being kind like the parse monad.

-- Also leaves and nodes should basically be the same thing. That is to
-- say,leaves should be lik enodes with a parameterised type of string instead
-- of a list of children

fillSize :: Float -> Element -> Element
fillSize f = fillVertical f . fillHorizontal f

border :: Element -> Element
border e =
  col
    [ (fillHorizontal 1 . row) [text "╭", horizontalRule 1, text "╮"],
      (fillSize 1 . row) [verticalRule 1, fillSize 1 e, verticalRule 1],
      (fillHorizontal 1 . row) [text "╰", horizontalRule 1, text "╯"]
    ]

fillBackground :: Char -> Element -> Element
fillBackground x node@(Node {config}) = node {config = config {fill = Just x}}
fillBackground x leaf@(Leaf {}) = leaf {s = [x]}

t :: Element
t =
 border $
    row >>>
      fillBackground '•'
      . bgColor Darkblue
      . textColor Orange1 $
      [ (fillVertical 1 . col)
          [ verticalSpacer 1,
            (textBold . textItalic . text) "This is the first column",
            fillHorizontal 1 . padCenter $ text "World",
            verticalSpacer 1
          ],
        horizontalSpacer 0.5,
        verticalRule 1,
        horizontalSpacer 0.5,
        col >>> fillVertical 1 $
          [ textUnderline . textBold . text $ "This is the second column",
            row >>> fillHorizontal 1 $ [horizontalSpacer 1, text "World"]
          ]
      ]

padCenter :: Element -> Element
padCenter e = row [horizontalSpacer 0.5, e, horizontalSpacer 0.5]

wrapT :: Element -> Element
wrapT leaf@Leaf {} = leaf { wrap = True }
wrapT node@Node {children} = node {children = map wrapT children}

tree0 :: Element
tree0 =
  fillSize 1 . col $
    [ col
        [ wrapT . text $ "This is kinda crazy bro?? isn't it cool that I have this thingy?? This text shoud go off screen at somepoint",
          text "this might get a bit annoying"
        ],
      fillSize 1 (padCenter (fillVertical 1 t))
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

tempLoop :: IO ()
tempLoop = hFlush stdout *> putStr "\ESC[2J" *> putStr "\ESC[H" *> (do
  (screenHeight, screenWidth) <- getTerminalSize >>= (\(termLines, termCols) -> return (termLines, termCols)) . fromMaybe (error "Could not obtain terminal size. Are you running in a tty?")
  let processedTree = (cascadeFillCharacters Nothing . cascadeStyles mempty) tree0
      frameSpecificTree =
        ( positionElements
            . sizeFlexVertically (fromIntegral screenHeight - 5)
            . sizeFixedVertically
            . wrapText (fromIntegral screenWidth)
            . sizeFlexHorizontally (fromIntegral screenWidth)
            . (`setMetadata` (\md -> md {size = md.size {width = fromIntegral screenWidth, height = fromIntegral screenHeight - 5}}))
            . sizeFixedHorizontally
            . normaliseFlexorFloats
        )
          processedTree
   in putStrLn . showCanvas . clipHorizontally (fromIntegral screenWidth) . render $ frameSpecificTree) *> tempLoop

main :: IO ()
main = do
  putStr "\ESC[?1049h\ESC[H" -- enter fullscreen terminal mode; go to the top left
  tempLoop
