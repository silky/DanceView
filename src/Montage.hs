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

module Montage where

import           Data
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
        diagrams = zipWith (flip (asDiagrams opts)) frames cs
        gridded  = divvy (columns opts) (columns opts) diagrams
        joined   = vcat (map hcat gridded)
    

    let outSize = mkSizeSpec $ V2 (outWidth opts) (outHeight opts)
        diagram = joined # bg white

    renderCairo (outFile opts) outSize diagram
