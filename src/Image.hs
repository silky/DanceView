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
import           Diagrams.Prelude       hiding (Options, each)
import           Diagrams.Backend.Cairo
        

-- 35 inches = 3360 px
-- 20 inches = 1920 px

meh opts frame = edges -- Path [ fromVertices [ p2 p, p2 q ] | (p,q) <- edges ]
    where
        keyPoints = concat (asKeyPoints True frame)
        points    = (map . map) ((\(a,b) -> (round a, round b)) . (xy opts)) keyPoints
        edges     = concatMap (\xs -> zipWith (\a b -> [a,b]) xs (tail xs)) points


doMontage :: [Frame Person] -> Options -> IO ()
doMontage allFrames opts = do
    -- let frames     = each (length allFrames `div` (rows' * cols')) allFrames
    -- let frames = take 100 $ drop 100 $ allFrames
    let frames     = take 100 $ repeat (allFrames !! 2)
        seed       = 3 -- error $ show (meh opts (frames !! 0))
        -- seed       = error $ show (meh opts (frames !! 2))
        diagrams   = zipWith (flip (montageSingle opts)) frames (randColours seed)
        Just cols' = columns opts
        Just rows' = rows opts
        gridded    = divvy (cols') (cols') diagrams
        joined     = vcat (map hcat gridded)

    let outSize = mkSizeSpec $ V2 (outWidth opts) (outHeight opts)
        diagram = joined

    renderCairo (outFile opts) outSize diagram


-- | A weird design that never worked.
doFractal :: [Frame Person] -> Options -> IO ()
doFractal allFrames opts = do
    let frames  = each (length allFrames `div` 20) allFrames
 
    let diagram = fractal opts frames
        outSize = mkSizeSpec $ V2 (outWidth opts) (outHeight opts)

    renderCairo (outFile opts) outSize diagram
