module Main where

import           Paths_glow (version)
import           Data.Version (showVersion)
import           Graphics.Gloss

import           Game.Glow.Input (handleInput)
import           Game.Glow.Render (render)

main = do play (InWindow ("Glow " ++ showVersion version)
                            (1024, 768)
                            (10, 10) )
            white                                         -- background
            60                                            -- fps
            0                                             -- inital state
            render                                        -- rendering function
            handleInput                                   -- input handler
            (\t w -> w + t)                               -- world advancement

