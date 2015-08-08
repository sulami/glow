module Game.Glow.World (
  World,
  initalWorld,
  step
) where

-- | The world datatype. Currently only holds a player.
data World = World {
  player :: Player
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

-- | The player datatype. Currently only holds the position.
data Player = Player {
  position :: (Float, Float)
}

instance Show Player where
  show (Player (x,y)) = unlines [ "Player:",
                                  "Pos: " ++ show x ++ ", " ++ show y ]

-- | Create the inital player data.
initalPlayer :: Player
initalPlayer = Player $ (0, 0)

