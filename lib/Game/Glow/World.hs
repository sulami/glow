{-# OPTIONS_HADDOCK ignore-exports #-}

-- | This module contains everything specifig to this game's world, like the
-- global world state, the entities and such.

module Game.Glow.World (
  World,
  initalWorld,
  step
) where

-- | The global world state. This holds all the state the game has.
data World = World {
  player :: Player -- ^ The player
}

instance Show World where
  show (World p) = show p

-- | Create an inital world state.
initalWorld :: World
initalWorld = World $ initalPlayer

-- | Advance the world for the next frame, using the time passed since the last
-- one.
step :: Float -> World -> World
step delta (World w0) = World w0

-- | The player datatype, holds everything related to the player.
data Player = Player {
  position :: (Float, Float) -- ^ Player coordinates
}

instance Show Player where
  show (Player (x,y)) = unlines [ "Player:",
                                  "Pos: " ++ show x ++ ", " ++ show y ]

-- | Create the inital player data.
initalPlayer :: Player
initalPlayer = Player $ (0, 0)

