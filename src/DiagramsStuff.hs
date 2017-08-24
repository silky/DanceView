{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE FlexibleContexts          #-}
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


-- INSANITY! We use partial type signatures so that we don't need to require
-- anything of 'b' until "just in time".
asDiagrams :: _ => Options 
                -> Colour Double 
                -> Frame Person 
                -> QDiagram b V2 Double Any
asDiagrams opts colour frame = mconcat [bones, r]
                                # pad 1.0
                                # lwG 5
                                # bg white

    where
        nameKeyPoint p = neck (toSkeleton p)

        xy KeyPoint {..} = (realToFrac x, realToFrac (fromIntegral (videoHeight opts) - y))

        keyPoints        = concat (asKeyPoints True frame)
        points           = (map . map) xy keyPoints
        edges            = concatMap (\xs -> zip xs (tail xs)) points

        bones =  mconcat [ fromVertices [ p2 p, p2 q ] | (p,q) <- edges ]
                         # lc colour
                         # lineCap  LineCapRound
                         # lineJoin LineJoinRound
                         # centerXY

        -- Encase the thing in a region as large as the
        -- original video, with some buffer for no particular reason.
        
        w = fromIntegral (videoWidth  opts) + 100
        h = fromIntegral (videoHeight opts) + 100
        r = phantom (rect w h :: D V2 Double)

