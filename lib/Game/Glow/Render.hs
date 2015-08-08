module Game.Glow.Render (
  render
) where

import           Graphics.Gloss.Data.Picture (Picture, text)

render :: (Show a) => a -> Picture
render = text . show

