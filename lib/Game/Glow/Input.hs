module Game.Glow.Input (
  handleInput
) where

import           Graphics.Gloss.Interface.Pure.Game (Event)

-- | Global input event handler.
handleInput :: Event -> a -> a
handleInput ev w = w

