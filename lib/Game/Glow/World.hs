{-# OPTIONS_HADDOCK ignore-exports #-}
{-# LANGUAGE TemplateHaskell #-}

-- | This module contains everything specifig to this game's world, like the
-- global world state, the entities and such.

module Game.Glow.World (
  World,
  initalWorld, drawWorld, debugWorld,
  step, movePlatforms,
  drawSprite
) where

import           Control.Lens (
  (&), (+~), _1, _2, makeLenses, mapped, over, set, traverse, view
  )
import           Graphics.Gloss.Data.Color (white)
import           Graphics.Gloss.Data.Picture (
  Picture (Pictures), thickCircle, color, polygon, translate
  )

-- | Any entity, like enemies. Comparable to classic sprites.
data Sprite = Sprite {
  _pic :: !Picture, -- ^ The picture representation
  _pos :: !(Float, Float), -- ^ The position of the sprite
  _size :: !(Float, Float), -- ^ The size of the sprite
  _speed :: !(Float, Float) -- ^ Current speed of the sprite
}

instance Show Sprite where
  show (Sprite pic pos siz spd) = unlines [ "Sprite: " ++ show pic,
                                            "Pos: " ++ show pos,
                                            "Size: " ++ show siz,
                                            "Speed: " ++ show spd ]

makeLenses ''Sprite

-- | The global world state. This holds all the state the game has.
data World = World {
  _ball :: !Sprite, -- ^ The ball sprite
  _horPlatforms :: ![Sprite], -- ^ The horizontal platforms
  _verPlatforms :: ![Sprite], -- ^ The vertical platforms
  _sprites :: ![Sprite], -- ^ All other currently present sprites
  _frametime :: !Float -- ^ The time since the last rendered frame
} deriving (Show)

makeLenses ''World

-- | Construct a polygon box using the dimensions. They anchor at the bottom
-- left corner.
makeBox :: (Float, Float) -> Picture
makeBox (w,h) = polygon [(0,0), (w,0), (w,h), (0,h)]

-- | Create an inital world state.
initalWorld :: World
initalWorld = World -- The ball
                    (Sprite (thickCircle 10 20) (0,0) (-10,-10) (10,20))
                    [ -- Horizontal platforms
                      Sprite (makeBox (100, 20)) (- 50,-210) (100, 20) (0,0),
                      Sprite (makeBox (100, 20)) (- 50, 190) (100, 20) (0,0) ]
                    [ -- Vertical platforms
                      Sprite (makeBox ( 20,100)) (-210,- 50) ( 20,100) (0,0),
                      Sprite (makeBox ( 20,100)) ( 190,- 50) ( 20,100) (0,0) ]
                    [] -- Other sprites
                    0 -- Frametime

-- | Create a picture from a sprite. Automatically translates it to the
-- position it needs to be in.
drawSprite :: Sprite -> IO Picture
drawSprite s = do let (x,y) = view pos s
                  return $ translate x y $ view pic s

-- | Create a single picture from a world.
drawWorld :: World -> IO Picture
drawWorld w = do sprs <- mapM drawSprite $ view sprites w
                 hp <- mapM drawSprite $ view horPlatforms w
                 vp <- mapM drawSprite $ view verPlatforms w
                 b <- drawSprite $ view ball w
                 return $ color white $ Pictures $ concat [sprs, hp, vp, [b]]

-- | Put together all the info about the world we have and collect it in a
-- string for making a picture.
debugWorld :: World -> String
debugWorld w = let hp = map show $ view horPlatforms w
                   vp = map show $ view verPlatforms w
                   b = show $ view ball w
                   os = map show $ view sprites w
                   ft = "FPS: " ++ (show $ 1 / view frametime w)
                in ft -- unlines $ ft : b : hp ++ vp ++ os

-- | Advance the world for the next frame, using the time passed since the last
-- one.
step :: Float -> World -> IO World
step delta w0 = return $ moveSprites delta w0 & set frametime delta

-- | Move all sprites according to its speed times the time in seconds passed
-- since the last frame rendered. This only makes sense with
-- non-player-controlled sprites, like the ball and enemies.
moveSprites :: Float -> World -> World
moveSprites delta w0 = w0 & over (sprites.mapped) (moveSprite delta)
                          & over ball (moveSprite delta)
  where
    moveSprite :: Float -> Sprite -> Sprite
    moveSprite delta s0 = let (x,y) = view pos s0
                              (dx,dy) = view speed s0
                          in s0 & set pos (x + dx * delta, y + dy * delta)

-- | Move the platforms according to the (x,y) coordinate tuple supplied.
movePlatforms :: (Float, Float) -> World -> World
movePlatforms (x,y) w0 =
  let psx = head (view horPlatforms w0) & view (size._1)
      psy = head (view verPlatforms w0) & view (size._2)
  in w0 & set (horPlatforms.traverse.pos._1) (x - psx / 2)
        & set (verPlatforms.traverse.pos._2) (y - psy / 2)

