module Game.Glow.World (
  World,
  initalWorld,
  step
) where

-- | The world datatype. Currently only holds a single float that displays the
-- time.
data World = World Float

instance Show World where
  show (World f) = show f

-- | Create an inital world state.
initalWorld :: World
initalWorld = World 0

-- | Advance the world for the next frame, using the time passed since the last
-- one.
step :: Float -> World -> World
step delta (World w0) = World $ w0 + delta

