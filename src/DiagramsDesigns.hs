{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE PartialTypeSignatures     #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module DiagramsDesigns where

import           Data
import           DanceView
import           DiagramsStuff
import           Data.Generics.Record
import           System.Random
import           Diagrams.Prelude             hiding (Options, frame, names)

-- INSANITY We use partial type signatures so that we don't need to require
-- anything of 'b' until "just in time".


bones :: _ => Options
           -> Frame Person
           -> QDiagram b V2 Double Any
bones opts frame = mconcat [ fromVertices [ p2 p, p2 q ] | (p,q) <- edges ]
                           # lineCap  LineCapRound
                           # lineJoin LineJoinRound
    where
        keyPoints = concat (asKeyPoints True frame)
        points    = (map . map) (xy opts) keyPoints
        edges     = concatMap (\xs -> zip xs (tail xs)) points


montageSingle :: _ => Options 
                   -> Colour Double 
                   -> Frame Person 
                   -> QDiagram b V2 Double Any
montageSingle opts colour frame = mconcat [bones', r, info]
                                    # pad 1.0
                                    # lwG 5
                                    # bg white

    where
        bones' = bones opts frame # lc colour
                                  # (if showFrameInfo opts 
                                        then translate reasonableOrigin
                                        else centerXY
                                    )

        info   = if showFrameInfo opts
                    then frameInfo opts frame reasonableOrigin
                    else mempty

        reasonableOrigin = r2 (- (w / 2), - (h / 2))

        -- Encase the thing in a region as large as the
        -- original video, with some buffer for no particular reason.
        
        w = fromIntegral (videoWidth  opts) + 100
        h = fromIntegral (videoHeight opts) + 100
        r = phantom (rect w h :: D V2 Double)


fractal :: _ => Options
             -> [Frame Person]
             -> QDiagram b V2 Double Any
fractal opts frames = foldl1 (flip atop) bones'
    where
        bones' = zipWith3 f frames [1..] (randColours 2)
                    # translate reasonableOrigin
                    # centerXY

        f frame k colour = bones opts frame # lwG (fromIntegral ((length frames - k) * 5))
                                            # lc colour

        reasonableOrigin = r2 (- (w / 2), - (h / 2))

        -- Encase the thing in a region as large as the
        -- original video, with some buffer for no particular reason.
        
        w = fromIntegral (videoWidth  opts) + 100
        h = fromIntegral (videoHeight opts) + 100

