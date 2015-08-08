module Game.Glow.Render (
  render
) where

import           Graphics.Gloss.Data.Color (Color, makeColor)
import           Graphics.Gloss.Data.Picture (
  Picture, color, scale, text, translate
  )

import           Game.Glow.World (World)

-- | Render the world to a picture we can draw.
render :: World -> Picture
render = color devColor . scale 0.2 0.2 . translate (-1024) 0 . text . show

-- | This is the color for use by dev overlays. It is yellow with an alpha of
-- 0.8.
devColor :: Color
devColor = makeColor 1 0.67 0 0.8

