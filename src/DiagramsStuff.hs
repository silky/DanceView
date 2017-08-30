{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE PartialTypeSignatures     #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}


module DiagramsStuff where

import           Data
import           DanceView
import           Data.Generics.Record
import           System.Random
import           Diagrams.Prelude             hiding (Options, frame, names)


colours :: [Colour Double]
-- -- Colourful
colours = [ sRGB24 255 18 135 -- Fuschia
          , sRGB24 255 186 0  -- Orange
          , sRGB24 16 180 232 -- Blue
          , sRGB24 18 255 123 -- Cyan
          ]


randColours :: Int -> [Colour Double]
randColours seed = map (colours !!) $ randomRs (0, len) (mkStdGen seed)
    where
        len = length colours - 1


-- | Returns labels on top of the people, and write data about
--   the frame on the picture itself.
frameInfo :: _ => Options
               -> Frame Person
               -> V2 Double
               -> QDiagram b V2 Double Any
frameInfo opts frame reasonableOrigin = mconcat [info, names]
    where
        peeps = getField @"people" frame
        names = mconcat (map namedXy peeps)

        -- If the keypoint we're going to draw the text at has no confidence,
        -- then don't bother to draw a label at all.
        nameKeyPoint p = neck (toSkeleton p)
        namedXy p = if getField @"score" (nameKeyPoint p) == 0
                       then mempty
                       else namedXy' p

        namedXy' p@Person {..} = (text (show name)  
                                    # fc black 
                                    # fontSizeL 60
                                )
                                # translate (r2 (xy opts (nameKeyPoint p)))
                                # translate reasonableOrigin

        frameDeets = "#" ++ show (frameNumber frame) ++ ", " ++ show (length peeps) ++ " people."
        info = baselineText frameDeets
                         # fc red
                         # fontSizeL 50
                         # translate reasonableOrigin


-- INSANITY! We use partial type signatures so that we don't need to require
-- anything of 'b' until "just in time".
asDiagrams :: _ => Options 
                -> Colour Double 
                -> Frame Person 
                -> QDiagram b V2 Double Any
asDiagrams opts colour frame = mconcat [bones, r, info]
                                # pad 1.0
                                # lwG 5
                                # bg white

    where
        keyPoints = concat (asKeyPoints True frame)
        points    = (map . map) (xy opts) keyPoints
        edges     = concatMap (\xs -> zip xs (tail xs)) points

        bones =  mconcat [ fromVertices [ p2 p, p2 q ] | (p,q) <- edges ]
                         # lc colour
                         # lineCap  LineCapRound
                         # lineJoin LineJoinRound
                         # (if showFrameInfo opts 
                               then translate reasonableOrigin
                               else centerXY
                           )

        info = if showFrameInfo opts
                  then frameInfo opts frame reasonableOrigin
                  else mempty

        reasonableOrigin = r2 (- (w / 2), - (h / 2))

        -- Encase the thing in a region as large as the
        -- original video, with some buffer for no particular reason.
        
        w = fromIntegral (videoWidth  opts) + 100
        h = fromIntegral (videoHeight opts) + 100
        r = phantom (rect w h :: D V2 Double)


xy :: Fractional a => Options -> KeyPoint -> (a, a)
xy opts KeyPoint {..} = (realToFrac x, realToFrac (fromIntegral (videoHeight opts) - y))
