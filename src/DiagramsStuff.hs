{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE PartialTypeSignatures     #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module DiagramsStuff where

import           Data
import           DanceView
import           Data.Generics.Record
import           System.Random
import           Diagrams.Prelude             hiding (Options, frame, names)


colours :: [Colour Double]
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


-- | Returns labels on top of the people, and write data about
--   the frame on the picture itself.
frameInfo :: _
          => Options
          -> Frame Person
          -> V2 Double
          -> QDiagram b V2 Double Any
frameInfo opts frame reasonableOrigin = mconcat [info, names]
    where
        peeps = getField @"people" frame
        names = mconcat (map namedXy peeps)

        -- If the keypoint we're going to draw the text at has no confidence,
        -- then don't bother to draw a label at all.
        nameKeyPoint p = neck (toSkeleton p)
        namedXy p = if getField @"score" (nameKeyPoint p) == 0
                       then mempty
                       else namedXy' p

        namedXy' p@Person {..} = (text (show name)  
                                    # fc black 
                                    # fontSizeL 60
                                )
                                # translate (r2 (xy opts (nameKeyPoint p)))
                                # translate reasonableOrigin

        frameDeets = "#" ++ show (frameNumber frame) ++ ", " ++ show (length peeps) ++ " people."
        info = baselineText frameDeets
                         # fc red
                         # fontSizeL 50
                         # translate reasonableOrigin




xy :: Fractional a => Options -> KeyPoint -> (a, a)
xy opts KeyPoint {..} = (realToFrac x, realToFrac (fromIntegral (videoHeight opts) - y))
