module Main where

import           Paths_glow (version)
import           Data.Version (showVersion)
import           Graphics.Gloss

import           Game.Glow.Event (handleEvent)
import           Game.Glow.Render (render)
import           Game.Glow.World (initalWorld, step)

main = play (InWindow ("Glow " ++ showVersion version) (1024, 768) (10, 10))
          black                                         -- background
          300                                           -- fps
          initalWorld                                   -- inital state
          render                                        -- rendering function
          handleEvent                                   -- event handler
          step                                          -- world advancement

