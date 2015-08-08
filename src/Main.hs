module Main where

import           Graphics.Gloss

main = do display (InWindow "Hello, World!"
                            (1024, 768)
                            (10, 10) )
            white
            picture

picture = Text "Hello, World!"

