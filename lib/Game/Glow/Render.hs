module Game.Glow.Render (
  render, drawText
) where

import           Graphics.Gloss.Data.Color (Color, makeColor)
import           Graphics.Gloss.Data.Picture (
  Picture (Pictures), color, scale, text, translate
  )

import           Game.Glow.World (World)

-- | Render the world to a picture we can draw.
render :: World -> Picture
render = devColor . scale 0.2 0.2 . translate (- 512) 0 . drawText 150 . show

-- | This is the color for use by dev overlays. It is yellow with an alpha of
-- 0.8.
devColor :: Picture -> Picture
devColor = color $ makeColor 1 0.67 0 0.8

-- | Properly draw multiline text. Needs the correct line offset to work
-- properly. Line height is relative, with 100 being the character size from
-- the baseline. 150 gives a nice spacing without the possibility of lines
-- colliding.
drawText :: Float -> String -> Picture
drawText oss s = let l = zip (lines s) [0..]
                  in Pictures $ map applyOffset l
  where
    applyOffset :: (String, Float) -> Picture
    applyOffset (l,os) = translate 0 (- os * oss) $ text l

