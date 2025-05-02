{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

module Types.Element
  ( Element (Node, md, variant, config, children, Leaf, s),
    defaultNode,
    sizeFixedHorizontally,
    sizeFixedVertically,
    positionElements,
    sizeFlexHorizontally,
    sizeFlexVertically,
    render,
    getMetadata,
    setMetadata,
    normaliseFlexorFloats,
    CanvasSequece (In, Out),
  )
where

import Control.Applicative ((<|>))
import Data.Bifunctor (Bifunctor (second))
import Data.List (intercalate, mapAccumL, mapAccumR)
import Data.List qualified as List.Data
import Data.Maybe (fromMaybe, isJust, maybeToList)
import Data.Set (Set)
import Data.Text (replace)
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
import Types.Metadata (Metadata (MD, position, size, style))
import Types.Position (Position (x, y))
import Types.Size (Size (S, height, width))
import Types.Style (Style (ST, bgColor, fillCharColor, textColor, textStyles), TextStyle (Bold, Italic, Underline), color)
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
        s :: [Char]
      }
  deriving (Show, Eq)

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

defaultNode :: Variant -> Element
defaultNode v = Node {md = mempty, variant = v, config = mempty, children = []}

-- Potentially a case for template haskell??
isRow = Variant.isRow . variant

isCol = Variant.isCol . variant

isNode Node {} = True
isNode Leaf {} = False

getMetadata Node {md} = md
getMetadata Leaf {md} = md

setMetadata x@(Node {md}) f = x {md = f md}
setMetadata x@(Leaf {md}) f = x {md = f md}

horizontalFlexFloats Node {children} = filter isNode children >>= (maybeToList . flexWidth . config)
horizontalFlexFloats Leaf {} = []

verticalFlexFloats Node {children} = filter isNode children >>= (maybeToList . flexHeight . config)
verticalFlexFloats Leaf {} = []

primaryAxisFlexFloats node@(Node {children, variant = Row {}}) = horizontalFlexFloats node
primaryAxisFlexFloats node@(Node {children, variant = Col {}}) = verticalFlexFloats node
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

normaliseFloatList = undefined

normaliseFlexorFloats :: Element -> Element
normaliseFlexorFloats node@(Node {variant = Row {}, ..}) =
  let horizontalFlexorSum = sum $ horizontalFlexFloats node
      transform = if horizontalFlexorSum > 1 then (/ horizontalFlexorSum) else id
   in node
        { children =
            map
              ( normaliseFlexorFloats
                  . ( \case
                        node@Node {..} -> node {config = config {flexWidth = transform <$> config.flexWidth, flexHeight = max 1 <$> config.flexHeight}}
                        x -> x
                    )
              )
              children
        }
normaliseFlexorFloats node@(Node {variant = Col {}, ..}) =
  let verticalFlexorSum = sum $ verticalFlexFloats node
      transform = if verticalFlexorSum > 1 then (/ verticalFlexorSum) else id
   in node
        { children =
            map
              ( normaliseFlexorFloats
                  . ( \case
                        node@Node {..} -> node {config = config {flexWidth = max 1 <$> config.flexWidth, flexHeight = transform <$> config.flexHeight}}
                        x -> x
                    )
              )
              children
        }
normaliseFlexorFloats leaf@(Leaf {}) = leaf

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
sizeFixedHorizontally leaf@Leaf {md, s} =
  leaf {md = md {size = md.size {width = fromIntegral $ length s}}}

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
  leaf {md = md {size = md.size {height = 1}}} -- TODO: Height is number of new lines + 1

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
              (\x@(Node {..}) -> x {md = md {size = md.size {width = md.size.width + slop}}}) -- This is a gross API, i know i will be getting a node and not an element but i can't have the type checker know
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
positionElements node@(Node {..}) =
  let children' = snd $ mapAccumL updateMDAndCurrentOffset 0 (map positionElements children)
   in node {md = md {position = md.position {x = 0, y = 0}}, children = children'}
  where
    updateMDAndCurrentOffset = case variant of
      Col {} -> updateMDAndCurrentY
      Row {} -> updateMDAndCurrentX
    updateMDAndCurrentY y e =
      ( fromIntegral (getMetadata e).size.height + y,
        setMetadata e (\md -> md {position = md.position {y}})
      )
    updateMDAndCurrentX x e =
      ( fromIntegral (getMetadata e).size.width + x,
        setMetadata e (\md -> md {position = md.position {x}})
      )
positionElements leaf@(Leaf {md, s}) = leaf {md = md {position = md.position {x = 0, y = 0}}}

------------------- TODO: CLEANUP
type Canvas = [[CanvasSequece]]

data CanvasSequece = In [Char] | Out [Char]
  deriving (Show, Eq)

createCanvas :: Char -> Size -> Canvas
createCanvas s size = replicate (fromIntegral size.height) [In $ replicate (fromIntegral size.width) s]

splice :: Int -> [CanvasSequece] -> [CanvasSequece] -> [CanvasSequece]
splice start original replacement =
  let (_, _, result) =
        foldl
          ( \(tempPointer, done, result) cs -> case (cs, done) of
              (_, True) -> (tempPointer, True, result ++ [cs])
              (In chars, False) ->
                let tempOffset = tempPointer + length chars
                 in if tempOffset > start
                      then
                        let start' = start - tempPointer
                            (pre, rest) = splitAt start' chars
                            (original', rest') = splitAt (length $ replacement >>= \case In chars -> chars; _ -> []) rest
                         in (tempOffset, True, result ++ [In pre] ++ replacement ++ [In rest']) -- TODO: Figure out bgFill transparency
                      else (tempOffset, False, result ++ [cs])
              (Out chars, False) -> (tempPointer, False, result ++ [cs]) -- TODO: Use a better fold direction
          )
          (0, False, [])
          original
   in result

drawOnCanvas :: Canvas -> Canvas -> Position -> Canvas
drawOnCanvas baseCanvas canvas pos =
  let (pre, rest) = splitAt pos.y baseCanvas
      (rows, rest') = splitAt (length canvas) rest
   in pre ++ zipWith (splice pos.x) rows canvas ++ rest'

temp :: Set TextStyle -> Style -> ([Char] -> [CanvasSequece])
temp textStyles ST {textColor, fillCharColor, bgColor} =
  let applyBgColor =
        ( \(r, g, b) s ->
            Out ("\x1b[48;2;" ++ (intercalate ";" . map show) [r, g, b] ++ "m") : s ++ [Out "\x1b[49m"]
        )
          . color
          <$> bgColor
      applyTextColor =
        ( \(r, g, b) s ->
            Out ("\x1b[38;2;" ++ (intercalate ";" . map show) [r, g, b] ++ "m") : s ++ [Out "\x1b[39m"]
        )
          . color
          <$> textColor
      applyTextStyles =
        foldr
          ( \x ->
              (.) $ case x of
                Bold -> (\s -> Out "\x1b[1m" : s ++ [Out "\x1b[22m"])
                Italic -> (\s -> Out "\x1b[3m" : s ++ [Out "\x1b[23m"])
                Underline -> (\s -> Out "\x1b[4m" : s ++ [Out "\x1b[24m"])
          )
          id
          textStyles
   in applyTextStyles . foldr ((.) . fromMaybe id) id [applyTextColor, applyBgColor] . (: []) . In

render :: Element -> Canvas
render Node {md, children, config} =
  let renderedChildrenAndMD = zip (map getMetadata children) (map render children)
   in foldl
        (\acc (md, child) -> drawOnCanvas acc child md.position)
        (createCanvas config.fill md.size)
        renderedChildrenAndMD
-- TODO: Figure out how to make it explicit that leaves are always 1. Right now
-- it is implicit...
render (Leaf md s) = [temp md.style.textStyles md.style s]
