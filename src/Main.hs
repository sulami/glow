module Main where

import           Graphics.Gloss

import           Input (handleInput)

main = do play (InWindow "Hello, World!"
                            (1024, 768)
                            (10, 10) )
            white
            60
            0
            (Text . show)
            handleInput
            (\t w -> w + t)

