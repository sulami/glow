{-# OPTIONS_HADDOCK ignore-exports #-}

-- | This module contains everything specifig to this game's world, like the
-- global world state, the entities and such.

module Game.Glow.World (
  World,
  initalWorld, drawWorld,
  step,
  drawSprite
) where

import           Graphics.Gloss.Data.Bitmap (loadBMP)
import           Graphics.Gloss.Data.Picture (Picture (Pictures), circle)

-- | The global world state. This holds all the state the game has.
data World = World {
  sprites :: [Sprite] -- ^ All currently present sprites
} deriving (Show)

-- | Create an inital world state.
initalWorld :: World
initalWorld = World [ Sprite (circle 12.5) (0,0) (25,25) -- The ball
                       ]

-- | Create a single picture from a world.
drawWorld :: World -> Picture
drawWorld w = Pictures $ map drawSprite $ sprites w

-- | Advance the world for the next frame, using the time passed since the last
-- one.
step :: Float -> World -> World
step delta w0 = w0

-- | Any entity, like enemies. Comparable to classic sprites.
data Sprite = Sprite {
  pic :: Picture, -- ^ The picture representation, most likely a BMP
  pos :: (Float, Float), -- ^ The position of the sprite
  size :: (Float, Float) -- ^ The size of the sprite
}

instance Show Sprite where
  show (Sprite pic pos siz) = unlines [ "Sprite: " ++ show pic,
                                        "Pos: " ++ show pos,
                                        "Size: " ++ show siz ]

-- | Create a picture from a sprite.
drawSprite :: Sprite -> Picture
drawSprite = pic

