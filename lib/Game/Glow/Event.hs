{-# OPTIONS_HADDOCK ignore-exports #-}

-- | This module contains the main event handler and its subhandlers. The
-- actual manipulation of the game state is done in "Game.Glow.World".

module Game.Glow.Event (
  handleEvent
) where

import           Graphics.Gloss.Interface.Pure.Game (Event (..))

import           Game.Glow.World (World, movePlatforms, resizeWorld)

-- | Global event handler.
handleEvent :: Event -> World -> World
handleEvent ev w0 = case ev of
                      EventKey key state mod (x,y) -> w0
                      EventMotion (x,y)            -> movePlatforms (x,y) w0
                      EventResize (w,h)            -> resizeWorld (w,h) w0

