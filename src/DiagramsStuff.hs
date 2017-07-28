module DiagramsStuff where

import           System.Random
import           Diagrams.Prelude             hiding (Options)


colours :: [Colour Double]
-- colours = replicate 10 white ++ 
--           [ sRGB24 255 18 135 -- Fuschia
--           , sRGB24 16 180 232 -- Blue
--           ]
-- -- Colourful
colours = [ sRGB24 255 18 135 -- Fuschia
          , sRGB24 255 186 0  -- Orange
          , sRGB24 16 180 232 -- Blue
          , sRGB24 18 255 123 -- Cyan
          ]


randColours :: Int -> [Colour Double]
randColours seed = map (colours !!) $ randomRs (0, len) (mkStdGen seed)
    where
        len = length colours - 1

