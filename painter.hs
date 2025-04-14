{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

import Data.Bifunctor (Bifunctor (second))
import Data.List (intercalate)
import Data.Maybe (fromJust, fromMaybe)
import Data.Sequence (mapWithIndex)
import Data.Text.Internal.Fusion.Size (Size)

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
  { sizeTemp :: SizeMD,
    relativePositionTemp :: RPosMD
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

data CommonConf where
  EConf :: {fillWidth :: Bool, fillHeight :: Bool, containerFill :: Char, minHeight :: Word, minWidth :: Word} -> CommonConf
  deriving (Show, Eq)

instance Default CommonConf where
  def = EConf {fillWidth = False, fillHeight = False, containerFill = ' ', minHeight = 0, minWidth = 0}

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
                Node (SizeMD {size = s}) _ _ -> resize ty acc s
                Leaf (SizeMD {size = s}) _ -> resize ty acc s
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
   in Node (SizeRPosMD {sizeTemp = SizeMD size, relativePositionTemp = RPosMD (0, 0)}) row (reverse es')
  where
    updateMetadataAndCurrentCol col e =
      let SizeRPosMD {sizeTemp = SizeMD {size}, relativePositionTemp = RPosMD {relativePosition}} = elementMD e
          col' = snd size + col
       in (col', elementReplaceMD e (SizeRPosMD {sizeTemp = SizeMD {size}, relativePositionTemp = RPosMD (fst relativePosition, col)}))
calcPosition (Node (SizeMD {size}) col@(Col _ _) es) =
  let (_, es') = foldl (\(row, result) -> second (: result) . updateMetadataAndCurrentRow row) (0, []) (map calcPosition es)
   in Node (SizeRPosMD {sizeTemp = SizeMD size, relativePositionTemp = RPosMD (0, 0)}) col (reverse es')
  where
    updateMetadataAndCurrentRow row e =
      let SizeRPosMD {sizeTemp = SizeMD {size}, relativePositionTemp = RPosMD {relativePosition}} = elementMD e
          row' = fst size + row
       in (row', elementReplaceMD e (SizeRPosMD {sizeTemp = SizeMD {size}, relativePositionTemp = RPosMD (row, snd relativePosition)}))
calcPosition (Leaf (SizeMD {size}) c) = Leaf (SizeRPosMD {sizeTemp = SizeMD size, relativePositionTemp = RPosMD (0, 0)}) c

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
render (Node (SizeRPosMD {sizeTemp = (SizeMD {size}), relativePositionTemp = (RPosMD {relativePosition})}) ty es) =
  let renderedChildrenAndMD = zip (map elementMD es) (map render es)
   in foldl
        (\acc (SizeRPosMD {relativePositionTemp = (RPosMD {relativePosition})}, child) -> drawOnCanvas acc child relativePosition)
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
      vSizable '|',
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
hSizable c = rowWith (def {containerFill = c, minHeight = 1}) def []

vSizable c = colWith (def {containerFill = c, minWidth = 1}) def []

myTree = myPara

main = do
  let myTree' = calcFixedSizes myPara
  let myTree'' = calcPosition myTree'
  -- let temp = tempDraw myTree''
  let SizeRPosMD {sizeTemp = SizeMD {size}, relativePositionTemp} = elementMD myTree''
  putStrLn $ intercalate "\n" (render myTree'')
