{-# LANGUAGE OverloadedRecordDot #-}

module Types.Style (Style (ST, bgColor, fillCharColor, textColor, textStyles), Color (..), TextStyle (..), color) where

import Data.Binary (Word8)
import Data.Set (Set)

data Style = ST
  { bgColor :: Maybe Color,
    fillCharColor :: Maybe Color,
    textColor :: Maybe Color,
    textStyles :: Set TextStyle,
    fillCharStyles :: Set TextStyle
  }
  deriving (Show, Eq)

instance Semigroup Style where
  l <> r =
    ST
      { bgColor = l.bgColor <> r.bgColor,
        fillCharColor = l.fillCharColor <> r.fillCharColor,
        textColor = l.textColor <> r.textColor,
        textStyles = l.textStyles <> r.textStyles,
        fillCharStyles = l.fillCharStyles <> r.fillCharStyles
      }

instance Monoid Style where
  mempty =
    ST
      { bgColor = Nothing,
        fillCharColor = Nothing,
        textColor = Nothing,
        textStyles = mempty,
        fillCharStyles = mempty
      }

data TextStyle = Bold | Italic | Underline
  deriving (Show, Eq, Ord)

data Color
  = Black
  | Maroon
  | Green
  | Olive
  | Navy
  | Purple1
  | Teal
  | Silver
  | Grey
  | Red
  | Lime
  | Yellow
  | Blue
  | Fuchsia
  | Aqua
  | White
  | Grey0
  | Navyblue
  | Darkblue
  | Blue3
  | Blue4
  | Blue1
  | Darkgreen
  | Deepskyblue4
  | Deepskyblue5
  | Deepskyblue6
  | Dodgerblue3
  | Dodgerblue2
  | Green4
  | Springgreen4
  | Turquoise4
  | Deepskyblue3
  | Deepskyblue7
  | Dodgerblue1
  | Green3
  | Springgreen3
  | Darkcyan
  | Lightseagreen
  | Deepskyblue2
  | Deepskyblue1
  | Green5
  | Springgreen5
  | Springgreen2
  | Cyan3
  | Darkturquoise
  | Turquoise2
  | Green1
  | Springgreen6
  | Springgreen1
  | Mediumspringgreen
  | Cyan2
  | Cyan1
  | Darkred
  | Deeppink4
  | Purple4
  | Purple5
  | Purple3
  | Blueviolet
  | Orange4
  | Grey37
  | Mediumpurple4
  | Slateblue3
  | Slateblue4
  | Royalblue1
  | Chartreuse4
  | Darkseagreen4
  | Paleturquoise4
  | Steelblue
  | Steelblue3
  | Cornflowerblue
  | Chartreuse3
  | Darkseagreen5
  | Cadetblue1
  | Cadetblue2
  | Skyblue3
  | Steelblue1
  | Chartreuse5
  | Palegreen3
  | Seagreen3
  | Aquamarine3
  | Mediumturquoise
  | Steelblue2
  | Chartreuse2
  | Seagreen2
  | Seagreen1
  | Seagreen4
  | Aquamarine1
  | Darkslategray2
  | Darkred1
  | Deeppink5
  | Darkmagenta1
  | Darkmagenta2
  | Darkviolet1
  | Purple2
  | Orange5
  | Lightpink4
  | Plum4
  | Mediumpurple3
  | Mediumpurple5
  | Slateblue1
  | Yellow4
  | Wheat4
  | Grey53
  | Lightslategrey
  | Mediumpurple
  | Lightslateblue
  | Yellow5
  | Darkolivegreen3
  | Darkseagreen
  | Lightskyblue3
  | Lightskyblue4
  | Skyblue2
  | Chartreuse6
  | Darkolivegreen4
  | Palegreen4
  | Darkseagreen3
  | Darkslategray3
  | Skyblue1
  | Chartreuse1
  | Lightgreen1
  | Lightgreen2
  | Palegreen1
  | Aquamarine2
  | Darkslategray1
  | Red3
  | Deeppink6
  | Mediumvioletred
  | Magenta3
  | Darkviolet2
  | Purple6
  | Darkorange3
  | Indianred1
  | Hotpink3
  | Mediumorchid3
  | Mediumorchid
  | Mediumpurple2
  | Darkgoldenrod
  | Lightsalmon3
  | Rosybrown
  | Grey63
  | Mediumpurple6
  | Mediumpurple1
  | Gold3
  | Darkkhaki
  | Navajowhite3
  | Grey69
  | Lightsteelblue3
  | Lightsteelblue
  | Yellow3
  | Darkolivegreen5
  | Darkseagreen6
  | Darkseagreen2
  | Lightcyan3
  | Lightskyblue1
  | Greenyellow
  | Darkolivegreen2
  | Palegreen2
  | Darkseagreen7
  | Darkseagreen1
  | Paleturquoise1
  | Red4
  | Deeppink3
  | Deeppink7
  | Magenta4
  | Magenta5
  | Magenta2
  | Darkorange4
  | Indianred2
  | Hotpink4
  | Hotpink2
  | Orchid
  | Mediumorchid1
  | Orange3
  | Lightsalmon4
  | Lightpink3
  | Pink3
  | Plum3
  | Violet
  | Gold4
  | Lightgoldenrod3
  | Tan
  | Mistyrose3
  | Thistle3
  | Plum2
  | Yellow6
  | Khaki3
  | Lightgoldenrod2
  | Lightyellow3
  | Grey84
  | Lightsteelblue1
  | Yellow2
  | Darkolivegreen1
  | Darkolivegreen6
  | Darkseagreen8
  | Honeydew2
  | Lightcyan1
  | Red1
  | Deeppink2
  | Deeppink1
  | Deeppink8
  | Magenta6
  | Magenta1
  | Orangered1
  | Indianred3
  | Indianred4
  | Hotpink1
  | Hotpink5
  | Mediumorchid2
  | Darkorange
  | Salmon1
  | Lightcoral
  | Palevioletred1
  | Orchid2
  | Orchid1
  | Orange1
  | Sandybrown
  | Lightsalmon1
  | Lightpink1
  | Pink1
  | Plum1
  | Gold1
  | Lightgoldenrod4
  | Lightgoldenrod5
  | Navajowhite1
  | Mistyrose1
  | Thistle1
  | Yellow1
  | Lightgoldenrod1
  | Khaki1
  | Wheat1
  | Cornsilk1
  | Grey100
  | Grey3
  | Grey7
  | Grey11
  | Grey15
  | Grey19
  | Grey23
  | Grey27
  | Grey30
  | Grey35
  | Grey39
  | Grey42
  | Grey46
  | Grey50
  | Grey54
  | Grey58
  | Grey62
  | Grey66
  | Grey70
  | Grey74
  | Grey78
  | Grey82
  | Grey85
  | Grey89
  | Grey93
  | Custom {r :: Word8, g :: Word8, b :: Word8}
  deriving (Show, Eq)

instance Semigroup Color where
  l <> r =
    let (rl, bl, gl) = color l
        (rr, br, gr) = color r
     in Custom
          (rl `div` 2 + rr `div` 2)
          (bl `div` 2 + br `div` 2)
          (gl `div` 2 + gr `div` 2)

color :: Color -> (Word8, Word8, Word8)
color Black = (0, 0, 0)
color Maroon = (128, 0, 0)
color Green = (0, 128, 0)
color Olive = (128, 128, 0)
color Navy = (0, 0, 128)
color Purple1 = (128, 0, 128)
color Teal = (0, 128, 128)
color Silver = (192, 192, 192)
color Grey = (128, 128, 128)
color Red = (255, 0, 0)
color Lime = (0, 255, 0)
color Yellow = (255, 255, 0)
color Blue = (0, 0, 255)
color Fuchsia = (255, 0, 255)
color Aqua = (0, 255, 255)
color White = (255, 255, 255)
color Grey0 = (0, 0, 0)
color Navyblue = (0, 0, 95)
color Darkblue = (0, 0, 135)
color Blue3 = (0, 0, 175)
color Blue4 = (0, 0, 215)
color Blue1 = (0, 0, 255)
color Darkgreen = (0, 95, 0)
color Deepskyblue4 = (0, 95, 95)
color Deepskyblue5 = (0, 95, 135)
color Deepskyblue6 = (0, 95, 175)
color Dodgerblue3 = (0, 95, 215)
color Dodgerblue2 = (0, 95, 255)
color Green4 = (0, 135, 0)
color Springgreen4 = (0, 135, 95)
color Turquoise4 = (0, 135, 135)
color Deepskyblue3 = (0, 135, 175)
color Deepskyblue7 = (0, 135, 215)
color Dodgerblue1 = (0, 135, 255)
color Green3 = (0, 175, 0)
color Springgreen3 = (0, 175, 95)
color Darkcyan = (0, 175, 135)
color Lightseagreen = (0, 175, 175)
color Deepskyblue2 = (0, 175, 215)
color Deepskyblue1 = (0, 175, 255)
color Green5 = (0, 215, 0)
color Springgreen5 = (0, 215, 95)
color Springgreen2 = (0, 215, 135)
color Cyan3 = (0, 215, 175)
color Darkturquoise = (0, 215, 215)
color Turquoise2 = (0, 215, 255)
color Green1 = (0, 255, 0)
color Springgreen6 = (0, 255, 95)
color Springgreen1 = (0, 255, 135)
color Mediumspringgreen = (0, 255, 175)
color Cyan2 = (0, 255, 215)
color Cyan1 = (0, 255, 255)
color Darkred = (95, 0, 0)
color Deeppink4 = (95, 0, 95)
color Purple4 = (95, 0, 135)
color Purple5 = (95, 0, 175)
color Purple3 = (95, 0, 215)
color Blueviolet = (95, 0, 255)
color Orange4 = (95, 95, 0)
color Grey37 = (95, 95, 95)
color Mediumpurple4 = (95, 95, 135)
color Slateblue3 = (95, 95, 175)
color Slateblue4 = (95, 95, 215)
color Royalblue1 = (95, 95, 255)
color Chartreuse4 = (95, 135, 0)
color Darkseagreen4 = (95, 135, 95)
color Paleturquoise4 = (95, 135, 135)
color Steelblue = (95, 135, 175)
color Steelblue3 = (95, 135, 215)
color Cornflowerblue = (95, 135, 255)
color Chartreuse3 = (95, 175, 0)
color Darkseagreen5 = (95, 175, 95)
color Cadetblue1 = (95, 175, 135)
color Cadetblue2 = (95, 175, 175)
color Skyblue3 = (95, 175, 215)
color Steelblue1 = (95, 175, 255)
color Chartreuse5 = (95, 215, 0)
color Palegreen3 = (95, 215, 95)
color Seagreen3 = (95, 215, 135)
color Aquamarine3 = (95, 215, 175)
color Mediumturquoise = (95, 215, 215)
color Steelblue2 = (95, 215, 255)
color Chartreuse2 = (95, 255, 0)
color Seagreen2 = (95, 255, 95)
color Seagreen1 = (95, 255, 135)
color Seagreen4 = (95, 255, 175)
color Aquamarine1 = (95, 255, 215)
color Darkslategray2 = (95, 255, 255)
color Darkred1 = (135, 0, 0)
color Deeppink5 = (135, 0, 95)
color Darkmagenta1 = (135, 0, 135)
color Darkmagenta2 = (135, 0, 175)
color Darkviolet1 = (135, 0, 215)
color Purple2 = (135, 0, 255)
color Orange5 = (135, 95, 0)
color Lightpink4 = (135, 95, 95)
color Plum4 = (135, 95, 135)
color Mediumpurple3 = (135, 95, 175)
color Mediumpurple5 = (135, 95, 215)
color Slateblue1 = (135, 95, 255)
color Yellow4 = (135, 135, 0)
color Wheat4 = (135, 135, 95)
color Grey53 = (135, 135, 135)
color Lightslategrey = (135, 135, 175)
color Mediumpurple = (135, 135, 215)
color Lightslateblue = (135, 135, 255)
color Yellow5 = (135, 175, 0)
color Darkolivegreen3 = (135, 175, 95)
color Darkseagreen = (135, 175, 135)
color Lightskyblue3 = (135, 175, 175)
color Lightskyblue4 = (135, 175, 215)
color Skyblue2 = (135, 175, 255)
color Chartreuse6 = (135, 215, 0)
color Darkolivegreen4 = (135, 215, 95)
color Palegreen4 = (135, 215, 135)
color Darkseagreen3 = (135, 215, 175)
color Darkslategray3 = (135, 215, 215)
color Skyblue1 = (135, 215, 255)
color Chartreuse1 = (135, 255, 0)
color Lightgreen1 = (135, 255, 95)
color Lightgreen2 = (135, 255, 135)
color Palegreen1 = (135, 255, 175)
color Aquamarine2 = (135, 255, 215)
color Darkslategray1 = (135, 255, 255)
color Red3 = (175, 0, 0)
color Deeppink6 = (175, 0, 95)
color Mediumvioletred = (175, 0, 135)
color Magenta3 = (175, 0, 175)
color Darkviolet2 = (175, 0, 215)
color Purple6 = (175, 0, 255)
color Darkorange3 = (175, 95, 0)
color Indianred1 = (175, 95, 95)
color Hotpink3 = (175, 95, 135)
color Mediumorchid3 = (175, 95, 175)
color Mediumorchid = (175, 95, 215)
color Mediumpurple2 = (175, 95, 255)
color Darkgoldenrod = (175, 135, 0)
color Lightsalmon3 = (175, 135, 95)
color Rosybrown = (175, 135, 135)
color Grey63 = (175, 135, 175)
color Mediumpurple6 = (175, 135, 215)
color Mediumpurple1 = (175, 135, 255)
color Gold3 = (175, 175, 0)
color Darkkhaki = (175, 175, 95)
color Navajowhite3 = (175, 175, 135)
color Grey69 = (175, 175, 175)
color Lightsteelblue3 = (175, 175, 215)
color Lightsteelblue = (175, 175, 255)
color Yellow3 = (175, 215, 0)
color Darkolivegreen5 = (175, 215, 95)
color Darkseagreen6 = (175, 215, 135)
color Darkseagreen2 = (175, 215, 175)
color Lightcyan3 = (175, 215, 215)
color Lightskyblue1 = (175, 215, 255)
color Greenyellow = (175, 255, 0)
color Darkolivegreen2 = (175, 255, 95)
color Palegreen2 = (175, 255, 135)
color Darkseagreen7 = (175, 255, 175)
color Darkseagreen1 = (175, 255, 215)
color Paleturquoise1 = (175, 255, 255)
color Red4 = (215, 0, 0)
color Deeppink3 = (215, 0, 95)
color Deeppink7 = (215, 0, 135)
color Magenta4 = (215, 0, 175)
color Magenta5 = (215, 0, 215)
color Magenta2 = (215, 0, 255)
color Darkorange4 = (215, 95, 0)
color Indianred2 = (215, 95, 95)
color Hotpink4 = (215, 95, 135)
color Hotpink2 = (215, 95, 175)
color Orchid = (215, 95, 215)
color Mediumorchid1 = (215, 95, 255)
color Orange3 = (215, 135, 0)
color Lightsalmon4 = (215, 135, 95)
color Lightpink3 = (215, 135, 135)
color Pink3 = (215, 135, 175)
color Plum3 = (215, 135, 215)
color Violet = (215, 135, 255)
color Gold4 = (215, 175, 0)
color Lightgoldenrod3 = (215, 175, 95)
color Tan = (215, 175, 135)
color Mistyrose3 = (215, 175, 175)
color Thistle3 = (215, 175, 215)
color Plum2 = (215, 175, 255)
color Yellow6 = (215, 215, 0)
color Khaki3 = (215, 215, 95)
color Lightgoldenrod2 = (215, 215, 135)
color Lightyellow3 = (215, 215, 175)
color Grey84 = (215, 215, 215)
color Lightsteelblue1 = (215, 215, 255)
color Yellow2 = (215, 255, 0)
color Darkolivegreen1 = (215, 255, 95)
color Darkolivegreen6 = (215, 255, 135)
color Darkseagreen8 = (215, 255, 175)
color Honeydew2 = (215, 255, 215)
color Lightcyan1 = (215, 255, 255)
color Red1 = (255, 0, 0)
color Deeppink2 = (255, 0, 95)
color Deeppink1 = (255, 0, 135)
color Deeppink8 = (255, 0, 175)
color Magenta6 = (255, 0, 215)
color Magenta1 = (255, 0, 255)
color Orangered1 = (255, 95, 0)
color Indianred3 = (255, 95, 95)
color Indianred4 = (255, 95, 135)
color Hotpink1 = (255, 95, 175)
color Hotpink5 = (255, 95, 215)
color Mediumorchid2 = (255, 95, 255)
color Darkorange = (255, 135, 0)
color Salmon1 = (255, 135, 95)
color Lightcoral = (255, 135, 135)
color Palevioletred1 = (255, 135, 175)
color Orchid2 = (255, 135, 215)
color Orchid1 = (255, 135, 255)
color Orange1 = (255, 175, 0)
color Sandybrown = (255, 175, 95)
color Lightsalmon1 = (255, 175, 135)
color Lightpink1 = (255, 175, 175)
color Pink1 = (255, 175, 215)
color Plum1 = (255, 175, 255)
color Gold1 = (255, 215, 0)
color Lightgoldenrod4 = (255, 215, 95)
color Lightgoldenrod5 = (255, 215, 135)
color Navajowhite1 = (255, 215, 175)
color Mistyrose1 = (255, 215, 215)
color Thistle1 = (255, 215, 255)
color Yellow1 = (255, 255, 0)
color Lightgoldenrod1 = (255, 255, 95)
color Khaki1 = (255, 255, 135)
color Wheat1 = (255, 255, 175)
color Cornsilk1 = (255, 255, 215)
color Grey100 = (255, 255, 255)
color Grey3 = (8, 8, 8)
color Grey7 = (18, 18, 18)
color Grey11 = (28, 28, 28)
color Grey15 = (38, 38, 38)
color Grey19 = (48, 48, 48)
color Grey23 = (58, 58, 58)
color Grey27 = (68, 68, 68)
color Grey30 = (78, 78, 78)
color Grey35 = (88, 88, 88)
color Grey39 = (98, 98, 98)
color Grey42 = (108, 108, 108)
color Grey46 = (118, 118, 118)
color Grey50 = (128, 128, 128)
color Grey54 = (138, 138, 138)
color Grey58 = (148, 148, 148)
color Grey62 = (158, 158, 158)
color Grey66 = (168, 168, 168)
color Grey70 = (178, 178, 178)
color Grey74 = (188, 188, 188)
color Grey78 = (198, 198, 198)
color Grey82 = (208, 208, 208)
color Grey85 = (218, 218, 218)
color Grey89 = (228, 228, 228)
color Grey93 = (238, 238, 238)
color Custom {r, g, b} = (r, g, b)
