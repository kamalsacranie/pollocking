{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

module Types.Element
  ( Element (Node, md, variant, config, children, Leaf, s),
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

import Data.List (intercalate, mapAccumL, mapAccumR)
import Data.Maybe (fromMaybe, isJust, maybeToList)
import Data.Word (Word16)
import GHC.Float (int2Float)
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
  )
import Types.Metadata (Metadata (position, size, style))
import Types.Position (Position (x, y))
import Types.Size (Size (height, width))
import Types.Style (Style (ST, bgColor, textColor, textStyles), TextStyle (Bold, Italic, Underline), color)
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

isNode :: Element -> Bool
isNode Node {} = True
isNode Leaf {} = False

getMetadata :: Element -> Metadata
getMetadata Node {md} = md
getMetadata Leaf {md} = md

setMetadata :: Element -> (Metadata -> Metadata) -> Element
setMetadata x@(Node {md}) f = x {md = f md}
setMetadata x@(Leaf {md}) f = x {md = f md}

horizontalFlexFloats :: Element -> [Float]
horizontalFlexFloats Node {children} = filter isNode children >>= (maybeToList . flexWidth . config)
horizontalFlexFloats Leaf {} = []

verticalFlexFloats :: Element -> [Float]
verticalFlexFloats Node {children} = filter isNode children >>= (maybeToList . flexHeight . config)
verticalFlexFloats Leaf {} = []

primaryAxisFlexFloats :: Element -> [Float]
primaryAxisFlexFloats node@(Node {variant = Row {}}) = horizontalFlexFloats node
primaryAxisFlexFloats node@(Node {variant = Col {}}) = verticalFlexFloats node
primaryAxisFlexFloats Leaf {} = []

sumPrimaryAxisFLexFloats :: Element -> Float
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

updateLastFlexibleChild :: (Traversable t) => (Element -> Element) -> t Element -> t Element
updateLastFlexibleChild fx =
  snd
    . mapAccumR
      ( \done x -> case (done, x) of
          (False, Node {config = NC {flexWidth = Just _}}) -> (True, fx x)
          _ -> (done, x)
      )
      False

normaliseFlexorFloats :: Element -> Element
normaliseFlexorFloats node@(Node {variant = Row {}, children}) =
  let horizontalFlexorSum = sum $ horizontalFlexFloats node
      transform = if horizontalFlexorSum > 1 then (/ horizontalFlexorSum) else id
   in node
        { children =
            map
              ( normaliseFlexorFloats
                  . ( \case
                        child@Node {config} -> child {config = config {flexWidth = transform <$> config.flexWidth, flexHeight = max 1 <$> config.flexHeight}}
                        x -> x
                    )
              )
              children
        }
normaliseFlexorFloats node@(Node {variant = Col {}, children}) =
  let verticalFlexorSum = sum $ verticalFlexFloats node
      transform = if verticalFlexorSum > 1 then (/ verticalFlexorSum) else id
   in node
        { children =
            map
              ( normaliseFlexorFloats
                  . ( \case
                        child@Node {config} -> child {config = config {flexWidth = max 1 <$> config.flexWidth, flexHeight = transform <$> config.flexHeight}}
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
sizeFlexHorizontally parentFreeWidth node@(Node {children, md, variant, config}) =
  let newWidth = case config.flexWidth of
        Just x -> max md.size.width $ floor (fromIntegral parentFreeWidth * x) -- TODO: figure out why we have max width here and not max height below
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
              ( \x ->
                  let childMd = getMetadata x
                   in x {md = childMd {size = childMd.size {width = childMd.size.width + slop}}} -- TODO: Refactor this ugly api
              )
              (map (sizeFlexHorizontally nodeFreeWidth) children)
        }
sizeFlexHorizontally _ leaf@(Leaf {}) = leaf

sizeFlexVertically :: Word16 -> Element -> Element
sizeFlexVertically parentFreeHeight node@(Node {..}) =
  let newHeight = case config.flexHeight of
        Just x -> floor (fromIntegral parentFreeHeight * x)
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
              ( \x ->
                  let childMd = getMetadata x
                   in x {md = childMd {size = childMd.size {height = childMd.size.height + slop}}}
              )
              (map (sizeFlexVertically nodeFreeHeight) children)
        }
sizeFlexVertically _ leaf@(Leaf {}) = leaf

positionElements :: Element -> Element
positionElements node@(Node {children, variant, md}) =
  let children' = snd $ mapAccumL updateMDAndCurrentOffset 0 (map positionElements children)
   in node {md = md {position = md.position {x = 0, y = 0}}, children = children'}
  where
    updateMDAndCurrentOffset = case variant of
      Col {} -> updateMDAndCurrentY
      Row {} -> updateMDAndCurrentX
    updateMDAndCurrentY y e =
      ( fromIntegral (getMetadata e).size.height + y,
        setMetadata e (\nodeMd -> nodeMd {position = nodeMd.position {y}})
      )
    updateMDAndCurrentX x e =
      ( fromIntegral (getMetadata e).size.width + x,
        setMetadata e (\nodeMd -> nodeMd {position = nodeMd.position {x}})
      )
positionElements leaf@(Leaf {md}) = leaf {md = md {position = md.position {x = 0, y = 0}}}

------------------- TODO: CLEANUP
type Canvas = [[CanvasSequece]]

data CanvasSequece = In [Char] | Out [Char]
  deriving (Show, Eq)

createCanvas :: Char -> Size -> Canvas
createCanvas s size = replicate (fromIntegral size.height) [In $ replicate (fromIntegral size.width) s]

splice :: Int -> Word16 -> [CanvasSequece] -> [CanvasSequece] -> [CanvasSequece]
splice start size original replacement =
  snd $
    foldl
      ( \(acc@(tempPointer, done), result) cs -> case (cs, done) of
          (In chars, False) ->
            let tempOffset = tempPointer + length chars
             in if tempOffset > start
                  then
                    let start' = start - tempPointer
                        (pre, rest) = splitAt start' chars
                        (_original', rest') = splitAt (fromIntegral size) rest
                     in ((tempOffset, True), result ++ [In pre] ++ replacement ++ [In rest']) -- TODO: Figure out bgFill transparency
                  else ((tempOffset, False), result ++ [cs])
          (_, _) -> (acc, result ++ [cs])
      )
      ((0, False), [])
      original

drawOnCanvas :: Canvas -> Canvas -> Metadata -> Canvas
drawOnCanvas baseCanvas canvas md =
  let (pre, rest) = splitAt md.position.y baseCanvas
      (rows, rest') = splitAt (length canvas) rest
   in pre ++ zipWith (splice md.position.x md.size.width) rows canvas ++ rest'

styleToFormatter :: Style -> ([Char] -> [CanvasSequece])
styleToFormatter ST {textColor, bgColor, textStyles} =
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
  foldl
    (\acc child -> drawOnCanvas acc (render child) (getMetadata child))
    (createCanvas config.fill md.size)
    children
-- TODO: Figure out how to make it explicit that leaves are always 1. Right now
-- it is implicit...
render (Leaf md s) = [styleToFormatter md.style s]
