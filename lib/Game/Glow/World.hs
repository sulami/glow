{-# OPTIONS_HADDOCK ignore-exports #-}

-- | This module contains everything specifig to this game's world, like the
-- global world state, the entities and such.

module Game.Glow.World (
  World,
  initalWorld,
  step
) where

import           Graphics.Gloss.Data.Bitmap (loadBMP)
import           Graphics.Gloss.Data.Picture (Picture)

-- | The global world state. This holds all the state the game has.
data World = World {
  sprites :: [Sprite] -- ^ All currently present sprites
}

instance Show World where
  show (World s) = show s

-- | Create an inital world state.
initalWorld :: World
initalWorld = World []

-- | Advance the world for the next frame, using the time passed since the last
-- one.
step :: Float -> World -> World
step delta w0 = w0

-- | Any entity, like enemies. Comparable to classic sprites.
data Sprite = Sprite {
  pic :: Picture, -- ^ The picture representation, most likely a BMP
  position :: (Float, Float), -- ^ The position of the sprite
  size :: (Float, Float) -- ^ The size of the sprite
}

instance Show Sprite where
  show (Sprite pic pos siz) = unlines [ "Sprite: " ++ show pic,
                                        "Pos: " ++ show pos,
                                        "Size: " ++ show siz ]

