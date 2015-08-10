{-# OPTIONS_HADDOCK ignore-exports #-}
{-# LANGUAGE TemplateHaskell #-}

-- | This module contains everything specifig to this game's world, like the
-- global world state, the entities and such.

module Game.Glow.World (
  World,
  initalWorld, drawWorld,
  step, movePlatforms,
  drawSprite
) where

import           Control.Lens (
  (&), (+~), _1, _2, makeLenses, set, traverse, view
  )
import           Graphics.Gloss.Data.Bitmap (loadBMP)
import           Graphics.Gloss.Data.Picture (
  Picture (Pictures), circle, polygon, translate
  )

-- | Any entity, like enemies. Comparable to classic sprites.
data Sprite = Sprite {
  _pic :: !Picture, -- ^ The picture representation
  _pos :: !(Float, Float), -- ^ The position of the sprite
  _size :: !(Float, Float) -- ^ The size of the sprite
}

instance Show Sprite where
  show (Sprite pic pos siz) = unlines [ "Sprite: " ++ show pic,
                                        "Pos: " ++ show pos,
                                        "Size: " ++ show siz ]

makeLenses ''Sprite

-- | The global world state. This holds all the state the game has.
data World = World {
  _ball :: !Sprite, -- ^ The ball sprite
  _horPlatforms :: ![Sprite], -- ^ The horizontal platforms
  _verPlatforms :: ![Sprite], -- ^ The vertical platforms
  _sprites :: ![Sprite], -- ^ All other currently present sprites
  _ballSpeed :: !(Float, Float) -- ^ The speed of the ball along both axes
} deriving (Show)

makeLenses ''World

-- | Construct a polygon box using the dimensions. They anchor at the bottom
-- left corner.
makeBox :: (Float, Float) -> Picture
makeBox (w,h) = polygon [(0,0), (w,0), (w,h), (0,h)]

-- | Create an inital world state.
initalWorld :: World
initalWorld = World (Sprite (circle 12.5) (0,0) (25,25)) -- The ball
                    [ -- Horizontal platforms
                      Sprite (makeBox (100, 20)) (- 50,-210) (100, 20),
                      Sprite (makeBox (100, 20)) (- 50, 190) (100, 20) ]
                    [ -- Vertical platforms
                      Sprite (makeBox ( 20,100)) (-210,- 50) ( 20,100),
                      Sprite (makeBox ( 20,100)) ( 190,- 50) ( 20,100) ]
                    [] -- Other sprites
                    (0,0) -- Ball speed

-- | Create a picture from a sprite. Automatically translates it to the
-- position it needs to be in.
drawSprite :: Sprite -> Picture
drawSprite s = let (x,y) = view pos s
                in translate x y $ view pic s

-- | Create a single picture from a world.
drawWorld :: World -> Picture
drawWorld w = Pictures $ [ Pictures $ map drawSprite $ view sprites w,
                           Pictures $ map drawSprite $ view horPlatforms w,
                           Pictures $ map drawSprite $ view verPlatforms w,
                           drawSprite $ view ball w ]

-- | Advance the world for the next frame, using the time passed since the last
-- one.
step :: Float -> World -> World
step delta w0 = moveBall delta w0

-- | Modify the world to move the ball according to the current ballspeed. Just
-- like 'step', it needs the time passed since the last frame to be
-- fps-independent.
moveBall :: Float -> World -> World
moveBall delta w0 = w0 & (ball.pos._1) +~ (view (ballSpeed._1) w0 * delta)
                       & (ball.pos._2) +~ (view (ballSpeed._2) w0 * delta)

-- | Move the platforms according to the (x,y) coordinate tuple supplied.
movePlatforms :: (Float, Float) -> World -> World
movePlatforms (x,y) w0 = w0 & set (horPlatforms.traverse.pos._1) (x - 50)
                            & set (verPlatforms.traverse.pos._2) (y - 50)

