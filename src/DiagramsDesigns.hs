{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE PartialTypeSignatures     #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module DiagramsDesigns where

import Data
import DanceView
import DiagramsStuff
import Data.Generics.Record
import System.Random
import Diagrams.Prelude hiding ( Options
                               , frame
                               , union
                               , names
                               )
import Diagrams.TwoD.Points
import Control.Monad (join)
import Data.List (nub)
import Diagrams.TwoD.Offset
import Diagrams.TwoD.Path.Boolean (union)


-- bonesPaths :: Diagram B
bonesPath opts frame = Path [ fromVertices [ p2 p, p2 q ] | (p,q) <- edges ]
    where
        keyPoints = concat (asKeyPoints True frame)
        points    = (map . map) (xy opts) keyPoints
        edges     = concatMap (\xs -> zip xs (tail xs)) points


bones :: _ => Options
           -> Frame Person
           -> QDiagram b V2 Double Any
bones = undefined
-- bones opts frame = mconcat [ fromVertices [ p2 p, p2 q ] | (p,q) <- edges ]
--                            # lineCap  LineCapRound
--                            # lineJoin LineJoinRound
--     where
--         keyPoints = concat (asKeyPoints True frame)
--         points    = (map . map) (xy opts) keyPoints
--         edges     = concatMap (\xs -> zip xs (tail xs)) points


joints = [ [ (0,0) , (2,2) ] -- leg 1
         , [ (2,2) , (4,0) ] -- leg 2
         , [ (2,2) , (2,8) ] -- torso
         , [ (4,6) , (2,6) ] -- left arm
         , [ (0,6) , (2,6) ] -- right arm
         ]

bbones = Path trails
    where
        verts  = (map . map) p2 joints
        trails = map fromVertices verts

montageSingle :: _ => Options 
                   -> Colour Double 
                   -> Frame Person 
                   -> QDiagram b V2 Double Any
montageSingle opts colour frame = (rect w h <> bones')
                                    # strokeP
                                    # fc white
                                    # lcA transparent

    where
        bones' = bonesPath opts frame
                        # expandPath 10
                        # union Winding
                        # centerXY

            -- # reversePath
        -- bones' = bones opts frame # lc colour
        --                           # centerXY
                                  -- # translate reasonableOrigin
                                  -- # (if showFrameInfo opts 
                                  --       then translate reasonableOrigin
                                  --       else centerXY
                                  --   )
                                  --

        -- ROTOSCOPE
        -- hullPts = convexHull2D [ p2 p | p <- (nub (join points)) ]
        -- TODO: Use
        -- boundry :: _ => QDiagram b V2 Double Any
        -- boundry = fromVertices hullPts
        --             # closeLine
        --             # stroke
        --             # centerXY 

        -- keyPoints = concat (asKeyPoints True frame)
        -- points    = (map . map) (xy opts) keyPoints
        -- END ROTOSCOPE

        -- info   = if showFrameInfo opts
        --             then frameInfo opts frame reasonableOrigin
        --             else mempty

        reasonableOrigin = r2 (- (w / 2), - (h / 2))

        -- Encase the thing in a region as large as the
        -- original video, with some buffer for no particular reason.
        
        w = fromIntegral (videoWidth  opts) -- + 100
        h = fromIntegral (videoHeight opts) -- + 100
        -- r = phantom (rect w h :: D V2 Double)
        -- r = rect w h # lwG 4 # lc blue # bg gray

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

