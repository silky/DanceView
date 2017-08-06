{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}

module Gif where

import           Data
import           DanceView
import           DiagramsStuff
import           Data.Generics.Record
import           Diagrams.Prelude             hiding (Options)
import           Diagrams.Backend.Rasterific  hiding (Options)


doGif :: [Frame Person] -> Options -> IO ()
doGif allFrames opts = do
    let outSize = mkSizeSpec $ V2 (outWidth opts) (outHeight opts)
        delay   = round $ fps opts

    let totalFrames = length allFrames
        stepSize    = 5
        frameCount  = totalFrames `div` stepSize
        frames      = [ allFrames !! (stepSize * n) | n <- [0 .. frameCount - 1] ]

    let cs       = randColours 3
        diagrams = zipWith (\f c -> asDiagrams opts c f) frames cs

    animatedGif (outFile opts) outSize LoopingForever delay diagrams
