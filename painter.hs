import Data.Bifunctor (Bifunctor (second))
import Data.List (intercalate)
import Data.Maybe (fromJust, fromMaybe)
import Data.Sequence (mapWithIndex)
import Data.Text.Internal.Fusion.Size (Size)

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

data NodeVariant
  = Row
  | Col
  deriving (Show, Eq)

freshNode = Node ()

row = freshNode Row

col = freshNode Col

leaf = Leaf ()

text = row . map leaf

calcSizes :: Element () -> Element SizeMD
calcSizes (Node _ ty es) =
  let es' = map calcSizes es
      size =
        foldl
          ( \acc e ->
              case e of
                Node (SizeMD {size = s}) _ _ -> resize ty acc s
                Leaf (SizeMD {size = s}) _ -> resize ty acc s
          )
          (0, 0)
          es'
   in Node (SizeMD {size = size}) ty (map calcSizes es)
  where
    resize ty (rows, cols) (rows', cols') = case ty of
      Row -> (max rows rows', cols + cols')
      Col -> (rows + rows', max cols cols')
calcSizes (Leaf () c) = Leaf (SizeMD {size = (1, 1)}) c

calcPosition :: Element SizeMD -> Element SizeRPosMD
calcPosition (Node (SizeMD {size}) Row es) =
  let (_, es') = foldl (\(col, result) -> second (: result) . updateMetadataAndCurrentCol col) (0, []) (map calcPosition es)
   in Node (SizeRPosMD {sizeTemp = SizeMD size, relativePositionTemp = RPosMD (0, 0)}) Row (reverse es')
  where
    updateMetadataAndCurrentCol col e =
      let SizeRPosMD {sizeTemp = SizeMD {size}, relativePositionTemp = RPosMD {relativePosition}} = elementMD e
          col' = snd size + col
       in (col', elementReplaceMD e (SizeRPosMD {sizeTemp = SizeMD {size}, relativePositionTemp = RPosMD (fst relativePosition, col)}))
calcPosition (Node (SizeMD {size}) Col es) =
  let (_, es') = foldl (\(row, result) -> second (: result) . updateMetadataAndCurrentRow row) (0, []) (map calcPosition es)
   in Node (SizeRPosMD {sizeTemp = SizeMD size, relativePositionTemp = RPosMD (0, 0)}) Col (reverse es')
  where
    updateMetadataAndCurrentRow row e =
      let SizeRPosMD {sizeTemp = SizeMD {size}, relativePositionTemp = RPosMD {relativePosition}} = elementMD e
          row' = fst size + row
       in (row', elementReplaceMD e (SizeRPosMD {sizeTemp = SizeMD {size}, relativePositionTemp = RPosMD (row, snd relativePosition)}))
calcPosition (Leaf (SizeMD {size}) c) = Leaf (SizeRPosMD {sizeTemp = SizeMD size, relativePositionTemp = RPosMD (0, 0)}) c

createBlankCanvas :: (Int, Int) -> [String]
createBlankCanvas (row, col) = replicate row (replicate col ' ')

splice :: Int -> [a] -> [a] -> [a]
splice start original replacement =
  let (pre, rest) = splitAt start original
   in let (_, rest') = splitAt (length replacement) rest
       in pre ++ replacement ++ rest'

drawOnCanvas :: [[a]] -> [[a]] -> (Int, Int) -> [[a]]
drawOnCanvas baseCanvas canvas (topLeftX, topLeftY) =
  let (pre, rest) = splitAt topLeftX baseCanvas
   in let (rows, rest') = splitAt (length canvas) rest
       in pre ++ zipWith (splice topLeftY) rows canvas ++ rest'

render :: Element SizeRPosMD -> [[Char]]
render (Node (SizeRPosMD {sizeTemp = (SizeMD {size}), relativePositionTemp = (RPosMD {relativePosition})}) _ es) =
  let renderedChildrenAndMD = zip (map elementMD es) (map render es)
   in foldl
        (\acc (SizeRPosMD {relativePositionTemp = (RPosMD {relativePosition})}, child) -> drawOnCanvas acc child relativePosition)
        (createBlankCanvas size)
        renderedChildrenAndMD
render (Leaf _ c) = [[c]]

myPara =
  row
    [ col
        [ text "This is the first column",
          -- sizable '-',
          text "Hello"
        ],
      -- sizable '|',
      col
        [ text "This is the second column",
          -- sizable '-',
          text "World"
        ]
    ]

myTree = myPara

main = do
  let myTree' = calcSizes myTree
  let myTree'' = calcPosition myTree'
  -- let temp = tempDraw myTree''
  let SizeRPosMD {sizeTemp = SizeMD {size}, relativePositionTemp} = elementMD myTree''
  putStrLn $ intercalate "\n" (render myTree'')
