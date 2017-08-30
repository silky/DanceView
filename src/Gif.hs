{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
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
        delay   = round $ fps opts

    let frames   = sampleFrames (length allFrames `div` 3) allFrames
    let cs       = randColours 3
        diagrams = zipWith (flip (asDiagrams opts)) frames cs

    animatedGif (outFile opts) outSize LoopingForever delay diagrams
