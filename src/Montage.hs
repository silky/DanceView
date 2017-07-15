{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}

module Montage where

import           Data
import           DanceView
import           System.Random
import           Data.List.Split        (divvy)
import           Diagrams.Prelude       hiding (Options)
import           Diagrams.Backend.Cairo


doMontage :: [Frame] -> Options -> IO ()
doMontage allFrames opts = do
    -- First we need to pick out rows*columns frames. We should pick them so
    -- that they are evenly spaced
    let frameCount  = rows opts * columns opts
        totalFrames = length allFrames
        stepSize    = totalFrames `div` frameCount
        frames      = [ allFrames !! (stepSize * n) | n <- [0 .. frameCount - 1] ]

    -- Now, having an even selection of frames, we need to render them
    let cs       = randColours 3
        diagrams = zipWith (\f c -> asDiagrams opts c (asKeyPoints f)) frames cs
        gridded  = divvy (columns opts) (columns opts) diagrams
        joined   = vcat (map hcat gridded)
    

    let outSize = mkWidth (outWidth opts)
        diagram = joined # bg white

    -- renderPretty (outFile opts) size diagram
    renderCairo  (outFile opts) outSize diagram


colours :: [Colour Double]
colours = [ sRGB24 255 18 135 -- Fuschia
          , sRGB24 255 186 0  -- Orange
          , sRGB24 16 180 232 -- Blue
          , sRGB24 18 255 123 -- Cyan
          ]


randColours :: Int -> [Colour Double]
randColours seed = map (colours !!) $ randomRs (0, len) (mkStdGen seed)
    where
        len = length colours - 1


-- TODO: Add an optional box that is the size of the frame (maybe?) so that
-- all the sub-boxes are the same size.
asDiagrams :: Options -> Colour Double -> [[KeyPoint]] -> Diagram B
asDiagrams opts colour keyPoints = mconcat [bones, r]
                            # pad 1.0
                            # lwG 8

    where
        bones = mconcat [ fromVertices [ p2 p, p2 q ] | (p,q) <- edges ]
                            # lc colour
                            # alignX 0
                            # alignY 0
                            # lineCap LineCapRound

        -- Encase the thing in a region as large as the
        -- original video.
        
        w = fromIntegral $ videoWidth  opts
        h = fromIntegral $ videoHeight opts
        r = phantom (rect w h :: D V2 Double)

        points = (map . map) (\KeyPoint {..} -> 
                    (realToFrac x, realToFrac (fromIntegral (videoHeight opts) - y))) keyPoints
        edges  = concatMap (\xs -> zip xs (tail xs)) points

