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
import           DiagramsStuff
import           Data.List.Split        (divvy)
import           Diagrams.Prelude       hiding (Options)
import           Diagrams.Backend.Cairo
        
-- 35 inches = 3360 px
-- 20 inches = 1920 px

doMontage :: [Frame Person] -> Options -> IO ()
doMontage allFrames opts = do
    -- First we need to pick out rows*columns frames. We should pick them so
    -- that they are evenly spaced
    let frameCount  = rows opts * columns opts
        totalFrames = length allFrames
        stepSize    = totalFrames `div` frameCount
        frames      = [ allFrames !! (stepSize * n) | n <- [0 .. frameCount - 1] ]

    -- Now, having an even selection of frames, we need to render them
    let cs       = randColours 3
        diagrams = zipWith (\f c -> asDiagrams opts c (concat (asKeyPoints True f))) frames cs
        gridded  = divvy (columns opts) (columns opts) diagrams
        joined   = vcat (map hcat gridded)
    

    let outSize = mkSizeSpec $ V2 (outWidth opts) (outHeight opts)
        diagram = joined # bg white

    renderCairo (outFile opts) outSize diagram


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
