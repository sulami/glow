module Main where

import           Graphics.Gloss

main = do play (InWindow "Hello, World!"
                            (1024, 768)
                            (10, 10) )
            white
            60
            0
            (Text . show)
            (\_ w -> w)
            (\t w -> w + t)

