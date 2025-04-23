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

import Control.Applicative ((<|>))
import Data.Bifunctor (Bifunctor (second))
import Data.Maybe (fromMaybe, isJust, maybeToList)
import Data.Word (Word16)
import GHC.Float (int2Float)
import GHC.IO.Unsafe (unsafePerformIO)
import Types.Config
  ( Config
      ( NC,
        fill,
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
import Types.Variant qualified as Variant

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
              bounded config.widthBound md.size.width
            else md.size.width
        Leaf {md} -> md.size.width
    )

elementFixedMinimumWidths :: [Element] -> [Word16]
elementFixedMinimumWidths =
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
              bounded config.heightBound md.size.height
            else md.size.height
        Leaf {md} -> md.size.height
    )

elementFixedMinimumHeights :: [Element] -> [Word16]
elementFixedMinimumHeights =
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
      width =
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
      height =
        ( case variant of
            Col {} -> sum
            Row {} -> foldl max md.size.height
        )
          . elementFixedHeights
          $ children'
   in node {md = md {size = md.size {height = bounded config.heightBound height}}, children = children'}
sizeFixedVertically leaf@Leaf {md} =
  leaf {md = md {size = md.size {height = 1}}}

primaryAxisFlexFloats Node {children, variant = Row {}} = children >>= (maybeToList . flexWidth . config)
primaryAxisFlexFloats Node {children, variant = Col {}} = children >>= (maybeToList . flexHeight . config)
primaryAxisFlexFloats Leaf {} = []

sumPrimaryAxisFLexFloats = sum . primaryAxisFlexFloats

calculateElementSlop :: Element -> Word16 -> Word16
calculateElementSlop node@(Node {}) availableSize =
  if sumPrimaryAxisFLexFloats node == 1.0
    then
      availableSize
        - sum
          ( map
              (floor . ((int2Float . fromIntegral) availableSize *))
              (primaryAxisFlexFloats node)
          )
    else
      0
calculateElementSlop Leaf {} _ = 0

updateLastFlexibleChild fx =
  snd
    . foldr
      ( \x (done, res) -> case (done, x) of
          (False, x@(Node {config = NC {flexWidth = Just f}, ..})) -> (True, fx x : res)
          _ -> (done, x : res)
      )
      (False, [])

sizeFlexHorizontally :: Word16 -> Element -> Element
sizeFlexHorizontally parentFreeWidth node@(Node {..}) =
  let newWidth = case config.flexWidth of
        Just x -> max md.size.width $ fromIntegral (floor (fromIntegral parentFreeWidth * x)) -- need to figure out how to normalize the floats somewhere
        Nothing -> md.size.width
      nodeFreeWidth =
        newWidth
          - ( case variant of
                Col {} -> 0
                Row {} -> sum (elementFixedMinimumWidths children)
            )
      slop = calculateElementSlop node nodeFreeWidth
   in node
        { md = md {size = md.size {width = newWidth}}, -- TODO: should this be bounded???
          children =
            updateLastFlexibleChild
              (\x@(Node {..}) -> x {md = md {size = md.size {width = md.size.width + slop}}})
              (map (sizeFlexHorizontally nodeFreeWidth) children)
        }
sizeFlexHorizontally _ leaf@(Leaf {}) = leaf

sizeFlexVertically :: Word16 -> Element -> Element
sizeFlexVertically parentFreeHeight node@(Node {..}) =
  let newHeight = case config.flexHeight of
        Just x -> fromIntegral (floor (fromIntegral parentFreeHeight * x)) -- need to figure out how to normalize the floats somewhere
        Nothing -> md.size.height
      nodeFreeHeight =
        newHeight
          - ( case variant of
                Col {} -> sum (elementFixedMinimumHeights children)
                Row {} -> 0
            )
      slop = calculateElementSlop node nodeFreeHeight
   in node
        { md = md {size = md.size {height = newHeight}},
          children =
            updateLastFlexibleChild
              (\x@(Node {..}) -> x {md = md {size = md.size {height = md.size.height + slop}}})
              (map (sizeFlexVertically nodeFreeHeight) children)
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
      (original', rest') = splitAt (length replacement) rest
   in pre ++ zipWith (\o r -> if r == ' ' then o else r) original' replacement ++ rest'

drawOnCanvas :: [[Char]] -> [[Char]] -> Position -> [[Char]]
drawOnCanvas baseCanvas canvas pos =
  let (pre, rest) = splitAt pos.y baseCanvas
      (rows, rest') = splitAt (length canvas) rest
   in pre ++ zipWith (splice pos.x) rows canvas ++ rest'

render :: Element -> [[Char]]
render Node {md, children, config} =
  let renderedChildrenAndMD = zip (map (\case Node {md} -> md; Leaf {md} -> md) children) (map render children)
   in foldl
        (\acc (md, child) -> drawOnCanvas acc child md.position)
        (createCanvas config.fill md.size)
        renderedChildrenAndMD
render (Leaf _ c) = [[c]]
