module Main where

import           Graphics.Gloss

import           Game.Glow.Input (handleInput)
import           Game.Glow.Render (render)

main = do play (InWindow "Hello, World!"
                            (1024, 768)
                            (10, 10) )
            white
            60
            0
            render
            handleInput
            (\t w -> w + t)

