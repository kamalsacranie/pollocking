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

{- How to do sizeable things
- We can have some new leaf type
- We can give a flag to node variants? -}

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

data Measure = Precise Int | Proportion
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
colWith conf cconf = freshNode (Col def cconf)

leaf = Leaf ()

text = row . map leaf

calcFixedSizes :: Element () -> Element SizeMD
calcFixedSizes (Node _ ty es) =
  let es' = map calcFixedSizes es
      size =
        foldl
          ( \acc e ->
              case e of
                Node md _ _ -> resize ty acc md.size
                Leaf md _ -> resize ty acc md.size
          )
          (0, 0)
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

myPara =
  (fill '•' . row)
    [ col
        [ text "This is the first column",
          -- sizable '-',
          text "Hello"
        ],
      -- vSizable '|',
      hSizable '-',
      col
        [ text "This is the second column",
          -- sizable '-',
          text "World"
        ]
    ]

fill :: Char -> Element a -> Element a
fill c (Node md ty es) =
  let recordUpdate conf = conf {containerFill = c}
   in Node
        md
        (mapContainerCommonConf ty recordUpdate)
        es
fill c (Leaf md _) = Leaf md c

-- it's feeling like I will have to have some Box type as much as i don't want it??
hSizable c = rowWith (def {containerFill = c, minHeight = Precise 1, minWidth = Proportion}) def []

vSizable c = colWith (def {containerFill = c, minHeight = Proportion, minWidth = Precise 1}) def []

myTree = myPara

measureSize m = case m of Proportion -> Nothing; Precise x -> Just x

measureSizeOr d m = fromMaybe d (measureSize m)

nodeIsProportional = \case (Node _ (Row EConf {minWidth = Proportion} _) _) -> True; _ -> False

calcHorizontalVariableSizes :: Element SizeMD -> Int -> Element SizeMD
calcHorizontalVariableSizes (Node SizeMD {size = (rowSize, colSize)} ty@(Row EConf {minWidth, minHeight} _) es) freeWidth =
  let (proportionalNodes, fixedNodes) = partition nodeIsProportional es
   in let newWidth = if (not . null) proportionalNodes then freeWidth else max (measureSizeOr colSize minWidth) colSize
          remainingVariableWidth =
            foldl
              ( \acc SizeMD {size = (_, colSize)} ->
                  max 0 (acc - colSize)
              )
              newWidth
              (map elementMD fixedNodes)
          temp (Node SizeMD {size = (rowSize, colSize)} ty@(Row EConf {minWidth = Proportion} _) es) =
            calcHorizontalVariableSizes (Node SizeMD {size = (rowSize, remainingVariableWidth)} ty es) remainingVariableWidth
          temp x =
            calcHorizontalVariableSizes x remainingVariableWidth
       in Node SizeMD {size = (max rowSize (measureSizeOr 1 minHeight), max newWidth (measureSizeOr 0 minWidth))} ty (map temp es)
calcHorizontalVariableSizes (Node SizeMD {size = (r, c)} col@(Col EConf {minWidth, minHeight} _) es) width =
  Node SizeMD {size = (max r (measureSizeOr 1 minHeight), max c (max width (measureSizeOr width minWidth)))} col $
    map (`calcHorizontalVariableSizes` width) es
calcHorizontalVariableSizes leaf@(Leaf _ _) _ = leaf

getTerminalSize =
  ( \case
      [cols, rows] -> Just (cols, rows)
      _ -> Nothing
  )
    . (mapMaybe readMaybe :: [String] -> [Int])
    . take 2
    -- Maybe use a function that doesn't throw here? or handle the throw I guess
    <$> mapM (flip (readProcess "tput") "") [["lines"], ["cols"]]

main = do
  (rows, cols) <- fromMaybe (error "Could not obtain terminal size. Are you running in a tty?") <$> getTerminalSize
  let myTree1 = calcFixedSizes myPara
  let myTree2 = calcHorizontalVariableSizes myTree1 cols
  let myTree3 = calcPosition myTree2
  putStrLn $ intercalate "\n" (render myTree3)
