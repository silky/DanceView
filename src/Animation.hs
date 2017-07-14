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
{-# LANGUAGE UndecidableInstances      #-}

module Animation where

import           Data
import           DanceView
import           Graphics.Gloss


doAnimation :: [Frame] -> Options -> IO ()
doAnimation frames opts = 
    animate (InWindow "DanceView" (videoWidth opts, videoHeight opts) (0,0))
            white
            (danceStep opts frames)


danceStep :: Options -> [Frame] -> Float -> Picture
danceStep opts frames elapsed = 
    Pictures $ [background, danceFloor] ++ bodies
    where
        background = translate (w/2) (h/2) $ color (greyN 0.8) $ rectangleSolid w h
        danceFloor = color azure $ polygon [ (w/9, h/2)
                                           , (w - (w/9), h/2)
                                           , (w,0)
                                           , (0,0)
                                           ]

        -- TOOD: Make sure we don't go too far in frame steps.
        frame = frames !! round (elapsed * fps opts)

        w = fromIntegral $ videoWidth  opts
        h = fromIntegral $ videoHeight opts

        points :: [[Point]]
        points     = (map . map) (\KeyPoint {..} -> (x, h - y)) (asKeyPoints frame)
        
        bodies :: [Picture]
        bodies     = map line points

