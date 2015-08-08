module Game.Glow.Render (
  render
) where

import           Graphics.Gloss.Data.Picture (Picture, text)

import           Game.Glow.World (World)

-- | Render the world to a picture we can draw.
render :: World -> Picture
render = text . show

