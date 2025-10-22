-- Game/Assets.hs
-- Assets for rendering: sizes and color palettes.
module Game.Assets
  ( Assets(..)
  , loadAssets
  ) where

import Graphics.Gloss (Color, Picture, red, green, blue, yellow, cyan, magenta
                      , makeColorI)
import Game.Types (BColor(..))

-- | Values the renderer depends on: tile size and color mapping.
data Assets = Assets
  { cellSize     :: !Float            -- ^ Size of one board cell.
  , glossColor   :: BColor -> Color   -- ^ Map logical color -> Gloss.Color.
  , background   :: Maybe Picture     -- ^ Background Picture
  }

-- | Build default asset pack.
loadAssets :: IO Assets
loadAssets = pure Assets { cellSize = 24, glossColor = toGloss
                         , background = Nothing }

-- Logical color -> Gloss color.
toGloss :: BColor -> Color
toGloss CRed = red
toGloss CGrn = green
toGloss CBlu = blue
toGloss CYel = yellow
toGloss CCyn = cyan
toGloss CMag = magenta
toGloss COrg = makeColorI 255 165 0 255  -- orange

