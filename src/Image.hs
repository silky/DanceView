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

module Image where

import           Data
import           DanceView
import           DiagramsDesigns
import           DiagramsStuff
import           Data.List.Split        (divvy)
import           Diagrams.Prelude       hiding (Options)
import           Diagrams.Backend.Cairo
        

-- 35 inches = 3360 px
-- 20 inches = 1920 px

doMontage :: [Frame Person] -> Options -> IO ()
doMontage allFrames opts = do
    let frames     = sampleFrames (rows' * cols') allFrames
        seed       = 3
        diagrams   = zipWith (flip (montageSingle opts)) frames (randColours seed)
        Just cols' = columns opts
        Just rows' = rows opts
        gridded    = divvy (cols') (cols') diagrams
        joined     = vcat (map hcat gridded)

    let outSize = mkSizeSpec $ V2 (outWidth opts) (outHeight opts)
        diagram = joined

    renderCairo (outFile opts) outSize diagram


-- |
doFractal :: [Frame Person] -> Options -> IO ()
doFractal allFrames opts = do
    -- let frames  = sampleFrames 3 allFrames
    let frames  = sampleFrames (min 20 (length allFrames)) allFrames
 
    let diagram = fractal opts frames
        outSize = mkSizeSpec $ V2 (outWidth opts) (outHeight opts)

    renderCairo (outFile opts) outSize diagram
