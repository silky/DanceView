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
import           DiagramsDesigns
import           Diagrams.Prelude             hiding (Options, each)
import           Diagrams.Backend.Rasterific  hiding (Options)


doGif :: [Frame Person] -> Options -> IO ()
doGif allFrames opts = do
    let outSize = mkSizeSpec $ V2 (outWidth opts) (outHeight opts)
        delay   = round $ fps opts

    let frames   = each 2 allFrames
        cs       = cycle [black]
        diagrams = zipWith (flip (montageSingle opts)) frames cs

    animatedGif (outFile opts) outSize LoopingForever delay diagrams


