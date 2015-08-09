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
import           Graphics.Gloss.Data.Picture (
  Picture (Pictures), circle, polygon, translate
  )

-- | The global world state. This holds all the state the game has. Some of the
-- sprites have fixed, hardcoded positions in the list:
--
-- > 0   - Ball
-- > 1-4 - Platforms
data World = World {
  sprites :: [Sprite], -- ^ All currently present sprites
  ballSpeed :: (Float, Float) -- ^ The speed of the ball along both axes
} deriving (Show)

-- | Create an inital world state.
initalWorld :: World
initalWorld = World [ Sprite (circle 12.5) (0,0) (25,25), -- The ball
                      -- Platforms
                      Sprite (makeBox (100, 20)) (- 50,-210) (100, 20),
                      Sprite (makeBox (100, 20)) (- 50, 190) (100, 20),
                      Sprite (makeBox ( 20,100)) (-210,- 50) ( 20,100),
                      Sprite (makeBox ( 20,100)) ( 190,- 50) ( 20,100)
                      ]
                    (50,50) -- Ball speed

-- | Create a single picture from a world.
drawWorld :: World -> Picture
drawWorld w = Pictures $ map drawSprite $ sprites w

-- | Advance the world for the next frame, using the time passed since the last
-- one.
step :: Float -> World -> World
step delta w0 = moveBall delta w0

-- | Modify the world to move the ball according to the current ballspeed. Just
-- like 'step', it needs the time passed since the last frame to be
-- fps-independent.
moveBall :: Float -> World -> World
moveBall dl w0 = let b0 = head $ sprites w0
                     (b0x,b0y) = pos b0
                     (bsx,bsy) = ballSpeed w0
                     b1p = (dl*bsx + b0x, dl*bsy + b0y)
                     b1 = Sprite (pic b0) b1p (size b0)
                  in World (b1 : tail (sprites w0)) (ballSpeed w0)

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

-- | Create a picture from a sprite. Automatically translates it to the
-- position it needs to be in.
drawSprite :: Sprite -> Picture
drawSprite s = let (x,y) = pos s
                in translate x y $ pic s

-- | Construct a polygon box using the dimensions. They anchor at the bottom
-- left corner.
makeBox :: (Float, Float) -> Picture
makeBox (w,h) = polygon [(0,0), (w,0), (w,h), (0,h)]

