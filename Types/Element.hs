{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

module Types.Element
  ( Element (Node, md, variant, config, children, Leaf, c),
    defaultNode,
    sizeFixedHorizontally,
    sizeFixedVertically,
    positionElements,
    sizeFlexHorizontally,
    sizeFlexVertically,
    render,
  )
where

import Data.Bifunctor (Bifunctor (second))
import Data.Maybe (isJust)
import Data.Word (Word16)
import Types.Config
  ( Config
      ( fill,
        flexHeight,
        flexWidth,
        heightBound,
        widthBound
      ),
    bounded,
    getLower,
    getUpper,
  )
import Types.Metadata (Metadata (MD, position, size))
import Types.Position (Position (x, y))
import Types.Size (Size (S, height, width))
import Types.Variant (Variant (Col, Row))

data Element
  = Node
      { md :: Metadata,
        variant :: Variant,
        config :: Config,
        children :: [Element]
      }
  | Leaf
      { md :: Metadata,
        c :: Char
      }
  deriving (Show, Eq)

elementWidths :: [Element] -> [Word16]
elementWidths = map (\x -> x.md.size.width)

elementFixedWidths :: [Element] -> [Word16]
elementFixedWidths =
  map
    ( \case
        Node {md, config} ->
          if isJust config.flexWidth
            then
              getLower config.widthBound
            else md.size.width
        Leaf {md} -> md.size.width
    )

elementFixedHeights :: [Element] -> [Word16]
elementFixedHeights =
  map
    ( \case
        Node {md, config} ->
          if isJust config.flexHeight
            then
              getLower config.heightBound
            else md.size.height
        Leaf {md} -> md.size.height
    )

elementHeights :: [Element] -> [Word16]
elementHeights = map (\x -> x.md.size.height)

defaultNode :: Variant -> Element
defaultNode v = Node {md = mempty, variant = v, config = mempty, children = []}

-- We need to go back and think about how we can make this work at any stage of our process
-- Right now it doesn't check if nodes are flexible. I feel like we can make
-- this stage independent if we use the minW/H for this flex thing
sizeFixedHorizontally :: Element -> Element
sizeFixedHorizontally node@(Node {variant, children, md, config}) =
  let children' = map sizeFixedHorizontally children
   in let width =
            ( case variant of
                Row {} -> sum
                Col {} -> foldl max node.md.size.width
            )
              . elementFixedWidths
              $ children'
       in node {md = md {size = md.size {width = bounded config.widthBound width}}, children = children'}
sizeFixedHorizontally leaf@Leaf {md} =
  leaf {md = md {size = md.size {width = 1}}}

sizeFixedVertically :: Element -> Element
sizeFixedVertically node@(Node {variant, children, md, config}) =
  let children' = map sizeFixedVertically children
   in let height =
            ( case variant of
                Col {} -> sum
                Row {} -> foldl max md.size.height
            )
              . elementFixedHeights
              $ children'
       in node {md = md {size = md.size {height = bounded config.heightBound height}}, children = children'}
sizeFixedVertically leaf@Leaf {md} =
  leaf {md = md {size = md.size {height = 1}}}

sizeFlexHorizontally :: Word16 -> Element -> Element
sizeFlexHorizontally parentFreeWidth node@(Node {..}) =
  let newWidth = case (config.flexWidth, any (\case Node {config} -> isJust config.flexWidth; Leaf {} -> False) children) of
        (Just x, _) -> fromIntegral (ceiling (fromIntegral parentFreeWidth * x)) -- need to figure out how to normalize the floats somewhere
        (Nothing, True) -> parentFreeWidth
        (Nothing, False) -> md.size.width
   in let nodeFreeWidth =
            newWidth
              - ( case variant of
                    Col {} -> 0
                    Row {} -> sum (elementFixedWidths children)
                )
       in -- TODO: Overflow checking
          node
            { md = md {size = md.size {width = newWidth}},
              children = map (sizeFlexHorizontally nodeFreeWidth) children
            }
sizeFlexHorizontally _ leaf@(Leaf {}) = leaf

sizeFlexVertically :: Word16 -> Element -> Element
sizeFlexVertically parentFreeHeight node@(Node {..}) =
  let newHeight = case (config.flexHeight, any (\case Node {config} -> isJust config.flexHeight; Leaf {} -> False) children) of
        (Just x, _) -> fromIntegral (ceiling (fromIntegral parentFreeHeight * x)) -- need to figure out how to normalize the floats somewhere
        (Nothing, True) -> parentFreeHeight
        (Nothing, False) -> md.size.height
   in let nodeFreeHeight =
            newHeight
              - ( case variant of
                    Col {} -> sum (elementFixedHeights children)
                    Row {} -> 0
                )
       in node
            { md = md {size = md.size {height = newHeight}},
              children = map (sizeFlexVertically nodeFreeHeight) children
            }
sizeFlexVertically _ leaf@(Leaf {}) = leaf

positionElements :: Element -> Element
positionElements node@(Node {variant = Row {}, ..}) =
  let (_, children') = foldl (\(x, result) -> second (: result) . updateMDAndCurrentX x) (0, []) (map positionElements children)
   in node {md = md {position = md.position {x = 0, y = 0}}, children = reverse children'}
  where
    updateMDAndCurrentX x e =
      let md = case e of Node {md} -> md; Leaf {md} -> md
       in ( fromIntegral md.size.width + x,
            case e of
              node@(Node {..}) -> node {md = md {position = md.position {x}}}
              leaf@(Leaf {..}) -> leaf {md = md {position = md.position {x}}}
          )
positionElements node@(Node {variant = Col {}, ..}) =
  let (_, children') = foldl (\(y, result) -> second (: result) . updateMDAndCurrentY y) (0, []) (map positionElements children)
   in node {md = md {position = md.position {x = 0, y = 0}}, children = reverse children'}
  where
    updateMDAndCurrentY y e =
      let md = case e of Node {md} -> md; Leaf {md} -> md
       in ( fromIntegral md.size.height + y,
            case e of
              node@(Node {..}) -> node {md = md {position = md.position {y}}}
              leaf@(Leaf {..}) -> leaf {md = md {position = md.position {y}}}
          )
positionElements leaf@(Leaf {md, c}) = leaf {md = md {position = md.position {x = 0, y = 0}}}

--- TODO: CLEANUP

createCanvas :: Char -> Size -> [String]
createCanvas c s = replicate (fromIntegral s.height) (replicate (fromIntegral s.width) c)

splice :: Int -> [Char] -> [Char] -> [Char]
splice start original replacement =
  let (pre, rest) = splitAt start original
   in let (original', rest') = splitAt (length replacement) rest
       in pre ++ zipWith (\o r -> if r == ' ' then o else r) original' replacement ++ rest'

drawOnCanvas :: [[Char]] -> [[Char]] -> Position -> [[Char]]
drawOnCanvas baseCanvas canvas pos =
  let (pre, rest) = splitAt pos.y baseCanvas
   in let (rows, rest') = splitAt (length canvas) rest
       in pre ++ zipWith (splice pos.x) rows canvas ++ rest'

render :: Element -> [[Char]]
render Node {md, children, config} =
  let renderedChildrenAndMD = zip (map (\case Node {md} -> md; Leaf {md} -> md) children) (map render children)
   in foldl
        (\acc (md, child) -> drawOnCanvas acc child md.position)
        (createCanvas config.fill md.size)
        renderedChildrenAndMD
render (Leaf _ c) = [[c]]
