{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

import Control.Exception (throwIO)
import Control.Monad (when)
import Control.Monad.Trans.Maybe (MaybeT (runMaybeT))
import Data.Bifunctor (Bifunctor (second))
import Data.List (intercalate, partition)
import Data.Maybe (fromJust, fromMaybe, isNothing, mapMaybe)
import Data.Sequence (mapWithIndex)
import Data.Text.Internal.Fusion.Size (Size)
import Distribution.Utils.String (trim)
import System.Process (readProcess)
import Text.Read (readMaybe)

{-
Takeaways from sizing like this:

- We can have just one metadata with no maybe values, just ints. Metadata simply needs to be size and relative position
- It is okay for metadata to default to 0; it just means all the elements are 0 w,h and all on top of each other
- Then our functions can just modify the position and size
- Maybe having spacers in our tree is more explicit? can we find a way of having an orientation independent spacer? maybe...
- I feel like instead of having () for calculating the size of the tree for the first time, we could almost auto populate the size creation of the tree when we're building it?
  Like in the functions row, col, text etc
- use width and height instead of rows and cols
- I also prefer referring to width first and then height, so we have to swap i suppose
- Instead of having size this size that, we should have a width of, horizontal widht of, etc.
- Common conf should be put in node and not node type
-}

-- feels like node-variant should be replaced with some record with common config and the node idk.
data Element a
  = Node a NodeVariant [Element a]
  | Leaf a Char
  deriving (Show)

data SizeMD where
  SizeMD :: {size :: (Int, Int)} -> SizeMD
  deriving (Show)

data RPosMD where
  RPosMD :: {relativePosition :: (Int, Int)} -> RPosMD
  deriving (Show)

data SizeRPosMD = SizeRPosMD
  { size :: SizeMD,
    relativePosition :: RPosMD
  }
  deriving (Show)

elementMD node = case node of
  Node md _ _ -> md
  Leaf md _ -> md

elementReplaceMD node md = case node of
  Node _ t es -> Node md t es
  Leaf _ c -> Leaf md c

printConfig EConf {fillWidth, fillHeight} = print fillWidth

class Default a where
  def :: a

data Measure = Precise Int | Fit
  deriving (Show, Eq)

data CommonConf where
  EConf :: {fillWidth :: Bool, fillHeight :: Bool, containerFill :: Char, minHeight :: Measure, minWidth :: Measure} -> CommonConf
  deriving (Show, Eq)

instance Default CommonConf where
  def = EConf {fillWidth = False, fillHeight = False, containerFill = ' ', minHeight = Precise 0, minWidth = Precise 0}

data RowConf where
  RowConf :: {} -> RowConf
  deriving (Show, Eq)

instance Default RowConf where
  def = RowConf {}

data ColConf where
  ColConf :: {} -> ColConf
  deriving (Show, Eq)

instance Default ColConf where
  def = ColConf {}

data NodeVariant
  = Row CommonConf RowConf
  | Col CommonConf ColConf
  deriving (Show, Eq)

freshNode = Node ()

-- Figure out how to generate econf using optional rguments field etc idk.
row = freshNode (Row def RowConf {})

rowWith :: CommonConf -> RowConf -> [Element ()] -> Element ()
rowWith conf rconf = freshNode (Row conf rconf)

col = freshNode (Col def ColConf {})

colWith :: CommonConf -> ColConf -> [Element ()] -> Element ()
colWith conf cconf = freshNode (Col conf cconf)

leaf = Leaf ()

text = row . map leaf

nodeVariantCommonConf = \case
  Col c _ -> c
  Row c _ -> c

calcFixedSizes :: Element () -> Element SizeMD
calcFixedSizes (Node _ ty es) =
  let es' = map calcFixedSizes es
      eConf = nodeVariantCommonConf ty
      size =
        foldl
          ( \acc e ->
              case e of
                Node md _ _ -> resize ty acc md.size
                Leaf md _ -> resize ty acc md.size
          )
          (measureSizeOr 0 eConf.minHeight, measureSizeOr 0 eConf.minWidth)
          es'
   in Node (SizeMD {size = size}) ty (map calcFixedSizes es)
  where
    resize ty (rows, cols) (rows', cols') = case ty of
      Row _ _ -> (max rows rows', cols + cols')
      Col _ _ -> (rows + rows', max cols cols')
calcFixedSizes (Leaf () c) = Leaf (SizeMD {size = (1, 1)}) c

calcPosition :: Element SizeMD -> Element SizeRPosMD
calcPosition (Node (SizeMD {size}) row@(Row _ _) es) =
  let (_, es') = foldl (\(col, result) -> second (: result) . updateMetadataAndCurrentCol col) (0, []) (map calcPosition es)
   in Node (SizeRPosMD {size = SizeMD size, relativePosition = RPosMD (0, 0)}) row (reverse es')
  where
    updateMetadataAndCurrentCol col e =
      let SizeRPosMD {size = SizeMD {size}, relativePosition = RPosMD {relativePosition}} = elementMD e
          col' = snd size + col
       in (col', elementReplaceMD e (SizeRPosMD {size = SizeMD {size}, relativePosition = RPosMD (fst relativePosition, col)}))
calcPosition (Node (SizeMD {size}) col@(Col _ _) es) =
  let (_, es') = foldl (\(row, result) -> second (: result) . updateMetadataAndCurrentRow row) (0, []) (map calcPosition es)
   in Node (SizeRPosMD {size = SizeMD size, relativePosition = RPosMD (0, 0)}) col (reverse es')
  where
    updateMetadataAndCurrentRow row e =
      let SizeRPosMD {size = SizeMD {size}, relativePosition = RPosMD {relativePosition}} = elementMD e
          row' = fst size + row
       in (row', elementReplaceMD e (SizeRPosMD {size = SizeMD {size}, relativePosition = RPosMD (row, snd relativePosition)}))
calcPosition (Leaf (SizeMD {size}) c) = Leaf (SizeRPosMD {size = SizeMD size, relativePosition = RPosMD (0, 0)}) c

createCanvas :: Char -> (Int, Int) -> [String]
createCanvas c (row, col) = replicate row (replicate col c)

-- why can i not figure out how to make this n-dimentional
splice :: Int -> [Char] -> [Char] -> [Char]
splice start original replacement =
  let (pre, rest) = splitAt start original
   in let (original', rest') = splitAt (length replacement) rest
       in pre ++ zipWith (\o r -> if r == ' ' then o else r) original' replacement ++ rest'

drawOnCanvas :: [[Char]] -> [[Char]] -> (Int, Int) -> [[Char]]
drawOnCanvas baseCanvas canvas (topLeftX, topLeftY) =
  let (pre, rest) = splitAt topLeftX baseCanvas
   in let (rows, rest') = splitAt (length canvas) rest
       in pre ++ zipWith (splice topLeftY) rows canvas ++ rest'

mapContainerCommonConf ty f = case ty of
  Row conf rconf -> Row (f conf) rconf
  Col conf cconf -> Col (f conf) cconf

containerCommonConf ty f = case ty of
  Row conf rconf -> f conf
  Col conf cconf -> f conf

render :: Element SizeRPosMD -> [[Char]]
render (Node (SizeRPosMD {size = (SizeMD {size}), relativePosition = (RPosMD {relativePosition})}) ty es) =
  let renderedChildrenAndMD = zip (map elementMD es) (map render es)
   in foldl
        (\acc (SizeRPosMD {relativePosition = (RPosMD {relativePosition})}, child) -> drawOnCanvas acc child relativePosition)
        (createCanvas (containerCommonConf ty containerFill) size)
        renderedChildrenAndMD
render (Leaf _ c) = [[c]]

fill :: Char -> Element a -> Element a
fill c (Node md ty es) =
  let recordUpdate conf = conf {containerFill = c}
   in Node
        md
        (mapContainerCommonConf ty recordUpdate)
        es
fill c (Leaf md _) = Leaf md c

-- it's feeling like I will have to have some Box type as much as i don't want it??
hSizable c = rowWith (def {containerFill = c, minHeight = Precise 1, minWidth = Fit}) def []

vSizable c = colWith (def {containerFill = c, minHeight = Fit, minWidth = Precise 1}) def []

myTree = myPara

measureIsPrecise m = case m of Precise _ -> True; _ -> False

measureSize m = case m of Fit -> Nothing; Precise x -> Just x

measureSizeOr d m = fromMaybe d (measureSize m)

-- this is a retarded function
rowNodeIsProportional = \case
  (Node _ (Row EConf {minWidth = Fit} _) _) -> True
  _ -> False

colNodeIsProportional = \case
  (Node _ (Col EConf {minHeight = Fit} _) _) -> True
  _ -> False

calcHorizontalVariableSizes :: Element SizeMD -> Int -> Element SizeMD
calcHorizontalVariableSizes (Node SizeMD {size = (h, w)} ty@(Row EConf {minWidth, minHeight} _) es) freeWidth =
  let (proportionalNodes, fixedNodes) = partition rowNodeIsProportional es
   in let newWidth = if (not . null) proportionalNodes || (not . measureIsPrecise) minWidth then freeWidth else max (measureSizeOr w minWidth) w
          remainingVariableWidth =
            foldl
              ( \acc SizeMD {size = (_, w)} ->
                  max 0 (acc - w)
              )
              newWidth
              (map elementMD fixedNodes)
          temp (Node SizeMD {size = (h, _)} ty@(Row EConf {minWidth = Fit} _) es) =
            calcHorizontalVariableSizes (Node SizeMD {size = (h, remainingVariableWidth)} ty es) remainingVariableWidth
          temp x =
            calcHorizontalVariableSizes x remainingVariableWidth
       in Node
            SizeMD
              { size =
                  ( max h (measureSizeOr 1 minHeight),
                    max newWidth (measureSizeOr 0 minWidth)
                  )
              }
            ty
            (map temp es)
calcHorizontalVariableSizes (Node SizeMD {size = (h, w)} col@(Col EConf {minWidth, minHeight} _) es) freeWidth =
  -- maybe this min height should should be done in the first sizing step too hmmm?
  Node
    SizeMD
      { size =
          ( max h (measureSizeOr 0 minHeight),
            max w (max freeWidth (measureSizeOr 0 minWidth))
          )
      }
    col
    $ map (`calcHorizontalVariableSizes` freeWidth) es
calcHorizontalVariableSizes leaf@(Leaf _ _) _ = leaf

calcVerticalVariableSizes (Node SizeMD {size = (h, w)} ty@(Col EConf {minWidth, minHeight} _) es) freeHeight =
  let (proportionalNodes, fixedNodes) = partition colNodeIsProportional es
   in let newHeight = if (not . null) proportionalNodes || (not . measureIsPrecise) minHeight then freeHeight else max (measureSizeOr h minHeight) h
          remainingVariableHeight =
            foldl
              ( \acc SizeMD {size = (h, _)} ->
                  max 0 (acc - h)
              )
              newHeight
              (map elementMD fixedNodes)
          temp (Node SizeMD {size = (_, w)} ty@(Col EConf {minHeight = Fit} _) es) =
            calcVerticalVariableSizes (Node SizeMD {size = (remainingVariableHeight, w)} ty es) remainingVariableHeight
          temp x = calcVerticalVariableSizes x remainingVariableHeight
       in Node
            SizeMD
              { size =
                  ( max newHeight (measureSizeOr 0 minHeight),
                    max w (measureSizeOr 0 minWidth)
                  )
              }
            ty
            (map temp es)
calcVerticalVariableSizes (Node SizeMD {size = (h, w)} row@(Row EConf {minWidth, minHeight} _) es) freeHeight =
  Node
    SizeMD
      { size =
          ( max h (max 0 (measureSizeOr freeHeight minHeight)),
            max w (measureSizeOr 0 minWidth)
          )
      }
    row
    $ map (`calcVerticalVariableSizes` freeHeight) es
calcVerticalVariableSizes leaf@(Leaf _ _) _ = leaf

getTerminalSize =
  ( \case
      [cols, rows] -> Just (cols, rows)
      _ -> Nothing
  )
    . (mapMaybe readMaybe :: [String] -> [Int])
    . take 2
    -- Maybe use a function that doesn't throw here? or handle the throw I guess
    <$> mapM (flip (readProcess "tput") "") [["lines"], ["cols"]]

myPara =
  col
    [ row [text "╭", hSizable '─', text "╮"],
      row
        [ vSizable '│',
          col
            [ text "This is the first column",
              text "Hello"
            ],
          vSizable '│',
          col
            [ text "This is the second column",
              text "World"
            ],
          vSizable '│'
        ],
      row [text "╰", hSizable '─', text "╯"]
    ]

setRootSize :: Element SizeMD -> (Int, Int) -> Element SizeMD
setRootSize (Node (SizeMD {size = (h, w)}) ty es) (rows, cols) =
  Node
    SizeMD
      { size =
          ( max h rows,
            max w cols
          )
      }
    ty
    es
setRootSize node _ = node

main = do
  (rows, cols) <- getTerminalSize >>= (\(r, c) -> return (r, c - 2)) . fromMaybe (error "Could not obtain terminal size. Are you running in a tty?")
  let myTree = calcPosition . (`calcVerticalVariableSizes` 20) . (`calcHorizontalVariableSizes` cols) . (`setRootSize` (20, cols)) . calcFixedSizes $ myPara
   in putStrLn $ intercalate "\n" (render myTree)
