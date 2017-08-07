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
import           DanceView
import           DiagramsStuff
import           Data.List.Split        (divvy)
import           Diagrams.Prelude       hiding (Options)
import           Diagrams.Backend.Cairo
        
-- 35 inches = 3360 px
-- 20 inches = 1920 px

-- TODO:
--  - WHY is it overwriting the numbers? Is confusing ...
--          Maybe could output a number of people or something?

doMontage :: [Frame Person] -> Options -> IO ()
doMontage allFrames opts = do
    let frames  = sampleFrames (rows opts * columns opts) allFrames
 
    -- | Convert the frames into a convient row & column based rendering
    let seed     = 3
        diagrams = zipWith (flip (asDiagrams opts)) frames (randColours seed)
        gridded  = divvy (columns opts) (columns opts) diagrams
        joined   = vcat (map hcat gridded)

    let outSize = mkSizeSpec $ V2 (outWidth opts) (outHeight opts)
        diagram = joined

    renderCairo (outFile opts) outSize diagram
