{-# OPTIONS_HADDOCK ignore-exports #-}

-- | This module contains the main event handler and its subhandlers. The
-- actual manipulation of the game state is done in "Game.Glow.World".

module Game.Glow.Event (
  handleEvent
) where

import           Graphics.Gloss.Interface.Pure.Game (Event (..))

import           Game.Glow.World (World, movePlatforms)

-- | Global event handler.
handleEvent :: Event -> World -> World
handleEvent ev w0 = case ev of
                      _ -> w0

