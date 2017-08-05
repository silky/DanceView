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

module Gif where

import           Data
import           DanceView
import           DiagramsStuff
import           Diagrams.Prelude             hiding (Options)
import           Diagrams.Backend.Rasterific  hiding (Options)


doGif :: [Frame Person] -> Options -> IO ()
doGif allFrames opts = do
    let outSize = mkSizeSpec $ V2 (outWidth opts) (outHeight opts)

        -- TODO: This probably shouldn't be hard-coded.
        delay   = 15

    let totalFrames = length allFrames
        stepSize    = 5
        frameCount  = totalFrames `div` stepSize
        frames      = [ allFrames !! (stepSize * n) | n <- [0 .. frameCount - 1] ]

    let cs       = randColours 3
        diagrams = zipWith (\f c -> asDiagrams opts c (concat (asKeyPoints True f))) frames cs

    animatedGif (outFile opts) outSize LoopingForever delay diagrams


asDiagrams :: Options -> Colour Double -> [[KeyPoint]] -> Diagram B
asDiagrams opts colour keyPoints = mconcat [bones, r]
                            # pad 1.0
                            # lwG 5

    where
        bones = mconcat [ fromVertices [ p2 p, p2 q ] | (p,q) <- edges ]
                            # lc colour
                            # centerXY
                            # lineCap  LineCapRound
                            # lineJoin LineJoinRound

        -- Encase the thing in a region as large as the
        -- original video.
        
        w = fromIntegral (videoWidth  opts `div` 2)
        h = fromIntegral (videoHeight opts)
        r = phantom (rect w h :: D V2 Double)

        points = (map . map) (\KeyPoint {..} -> 
                    (realToFrac x, realToFrac (fromIntegral (videoHeight opts) - y))) keyPoints
        edges  = concatMap (\xs -> zip xs (tail xs)) points
