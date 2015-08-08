module Main where

import           Paths_glow (version)
import           Data.Version (showVersion)
import           Graphics.Gloss

import           Game.Glow.Input (handleInput)
import           Game.Glow.Render (render)
import           Game.Glow.World (initalWorld, step)

main = do play (InWindow ("Glow " ++ showVersion version)
                            (1024, 768)
                            (10, 10) )
            black                                         -- background
            60                                            -- fps
            initalWorld                                   -- inital state
            render                                        -- rendering function
            handleInput                                   -- input handler
            step                                          -- world advancement

