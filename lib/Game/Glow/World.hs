{-# OPTIONS_HADDOCK ignore-exports #-}
{-# LANGUAGE TemplateHaskell #-}

-- | This module contains everything specifig to this game's world, like the
-- global world state, the entities and such.

module Game.Glow.World (
  World,
  initalWorld, drawWorld, debugWorld,
  step, movePlatforms, resizeWorld,
  drawSprite
) where

import           Control.Lens (
  (&), (.~), (+~), (*~), _1, _2, makeLenses, mapped, over, set, traverse, view
  )
import           Graphics.Gloss.Data.Color (white)
import           Graphics.Gloss.Data.Picture (
  Picture (Pictures), circleSolid, color, rectangleSolid, translate
  )

-- | Any entity, like enemies. Comparable to classic sprites.
data Sprite = Sprite {
  _pic :: !Picture, -- ^ The picture representation
  _pos :: !(Float, Float), -- ^ The position of the sprite
  _size :: !(Float, Float), -- ^ The size of the sprite
  _speed :: !(Float, Float) -- ^ Current speed of the sprite
}

instance Show Sprite where
  show (Sprite pic pos siz spd) = unlines [ "Sprite " ++ show pic ++ ":",
                                            "Size: " ++ show siz,
                                            "Pos: " ++ show pos,
                                            "Speed: " ++ show spd ]

makeLenses ''Sprite

-- | The global world state. This holds all the state the game has.
data World = World {
  _wsize :: !(Int, Int), -- ^ The window size
  _ball :: !Sprite, -- ^ The ball sprite
  _horPlatforms :: ![Sprite], -- ^ The horizontal platforms
  _verPlatforms :: ![Sprite], -- ^ The vertical platforms
  _sprites :: ![Sprite], -- ^ All other currently present sprites
  _frametime :: !Float -- ^ The time since the last rendered frame
} deriving (Show)

makeLenses ''World

-- | Create an inital world state.
initalWorld :: World
initalWorld = World (1024, 768)
                (Sprite (circleSolid 10) (0,0) (20,20) (40,60)) -- The ball
                [ -- Horizontal platforms
                  Sprite (rectangleSolid 100 20) (0,-210) (100, 20) (0,0),
                  Sprite (rectangleSolid 100 20) (0, 190) (100, 20) (0,0) ]
                [ -- Vertical platforms
                  Sprite (rectangleSolid 20 100) (-210,0) ( 20,100) (0,0),
                  Sprite (rectangleSolid 20 100) ( 190,0) ( 20,100) (0,0) ]
                [] -- Other sprites
                0 -- Frametime

-- | Change the world size.
resizeWorld :: (Int, Int) -> World -> World
resizeWorld (w,h) w0 = w0 & wsize .~ (w,h)

-- | Create a picture from a sprite. Automatically translates it to the
-- position it needs to be in.
drawSprite :: Sprite -> Picture
drawSprite s = let (x,y) = view pos s
                in translate x y $ view pic s

-- | Create a single picture from a world.
drawWorld :: World -> Picture
drawWorld w = let sprs = map drawSprite $ view sprites w
                  hp = map drawSprite $ view horPlatforms w
                  vp = map drawSprite $ view verPlatforms w
                  b = drawSprite $ view ball w
              in color white $ Pictures $ concat [sprs, hp, vp, [b]]

-- | Put together all the info about the world we have and collect it in a
-- string for making a picture.
debugWorld :: World -> String
debugWorld w = let h = head $ view horPlatforms w
                   v = head $ view verPlatforms w
                   hp = show $ view (pos._1) h
                   vp = show $ view (pos._2) v
                   hs = show $ view (speed._1) h
                   vs = show $ view (speed._2) v
                   (bpx,bpy) = view (ball.pos) w
                   (bsx,bsy) = view (ball.speed) w
                   os = concat $ map show $ view sprites w
                   ft = "FPS: " ++ (show $ 1 / view frametime w)
                   ws = "World Size: " ++ show (view wsize w)
                in unlines $ [ft, ws,
                              "Platform Position:", "X: " ++ hp, "Y: " ++ vp,
                              "Platform Speed:", "X: " ++ hs, "Y: " ++ vs,
                              "Ball Position:",
                              "X: " ++ show bpx, "Y: " ++ show bpy,
                              "Ball Speed:",
                              "X: " ++ show bsx, "Y: " ++ show bsy,
                              os]

-- | Advance the world for the next frame, using the time passed since the last
-- one.
step :: Float -> World -> World
step delta w0 = moveSprites delta (bounce w0) & set frametime delta

-- | Move all sprites according to its speed times the time in seconds passed
-- since the last frame rendered. This only makes sense with
-- non-player-controlled sprites, like the ball and enemies. This also resets
-- the platform speeds, which is a hacky way to reset them properly when they
-- are not moving currently.
moveSprites :: Float -> World -> World
moveSprites delta w0 = w0 & over (sprites.mapped) (moveSprite delta)
                          & set (horPlatforms.traverse.speed._1) 0
                          & set (verPlatforms.traverse.speed._2) 0
                          & over ball (moveSprite delta)
  where
    moveSprite :: Float -> Sprite -> Sprite
    moveSprite delta s0 = let (x,y) = view pos s0
                              (dx,dy) = view speed s0
                          in s0 & set pos (x + dx * delta, y + dy * delta)

-- | Move the platforms according to the (x,y) coordinate tuple supplied, and
-- set the speeds so they can be used for collision detection/ball deflection.
movePlatforms :: (Float, Float) -> World -> World
movePlatforms (x,y) w0 = let opx = view (pos._1) $ head $ view horPlatforms w0
                             opy = view (pos._2) $ head $ view verPlatforms w0
                             dlt = view frametime w0
                             dx = (x - opx) * 1 / dlt
                             dy = (y - opy) * 1 / dlt
                          in w0 & set (horPlatforms.traverse.speed._1) dx
                                & set (verPlatforms.traverse.speed._2) dy
                                & set (horPlatforms.traverse.pos._1) x
                                & set (verPlatforms.traverse.pos._2) y

-- | Bounce the ball of the platforms.
bounce :: World -> World
bounce w0 = let (bx,by) = view (ball.pos) w0
                (px,py) = (view (pos._1) $ head $ view horPlatforms w0,
                           view (pos._2) $ head $ view verPlatforms w0)
                (sx,sy) = (view (speed._1) $ head $ view horPlatforms w0,
                           view (speed._2) $ head $ view verPlatforms w0)
                (ix,iy) = ((bx >= 160 || bx <= -180) && abs (py - by) <= 50,
                           (by >= 160 || by <= -180) && abs (px - bx) <= 50)
                (fx,fy) = (if ix then -1 else 1, if iy then -1 else 1)
                (cx,cy) = (if iy then sx/2 else 0, if ix then sy/2 else 0)
            in w0 & (ball.speed._1) *~ fx & (ball.speed._2) *~ fy
                  & (ball.speed._1) +~ cx & (ball.speed._2) +~ cy

-- | Check if two sprites are colliding.
collision :: Sprite -> Sprite -> Bool
collision s0 s1 = let (pos0x,pos0y) = view pos s0
                      (siz0x,siz0y) = view size s0
                      (pos1x,pos1y) = view pos s1
                      (siz1x,siz1y) = view size s1
                  in abs (pos0x - pos1x) <= siz0x/2 + siz1x/2 &&
                     abs (pos0y - pos1y) <= siz0y/2 + siz1y/2

-- | Check in which direction a collision is happening for bouncing. We do this
-- by checking in which dimension the overlap is bigger. Returns 'Nothing' if
-- there is no collision. 'True' means a collision in x-direction, 'False' in
-- y-direction.
collisionDirection :: Sprite -> Sprite -> Maybe Bool
collisionDirection s0 s1 =
  if collision s0 s1
    then let (pos0x,pos0y) = view pos s0
             (siz0x,siz0y) = view size s0
             (pos1x,pos1y) = view pos s1
             (siz1x,siz1y) = view size s1
          in Just $ abs (pos0x - pos1x) - (siz0x/2 + siz1x/2)
                 <= abs (pos0y - pos1y) - (siz0y/2 + siz1y/2)
    else Nothing

