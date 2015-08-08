{-# OPTIONS_HADDOCK ignore-exports #-}

-- | This module contains the main input event handler and its subhandlers. The
-- actual manipulation of the game state is done in "Game.Glow.World".

module Game.Glow.Input (
  handleInput
) where

import           Graphics.Gloss.Interface.Pure.Game (Event)

import           Game.Glow.World (World)

-- | Global input event handler.
handleInput :: Event -> World -> World
handleInput ev w = w

