{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE UndecidableInstances      #-}

module DanceView where

import           Data
import           Data.List
import           Control.Monad
import           Data.Generics.Record
import           Data.List.Split        (chunksOf)
import           Debug.Trace


toSkeleton :: Person -> Skeleton2D
toSkeleton Person {..} = Skeleton {..}
    where
        (nose
         : neck
         : rightShoulder
         : rightElbow
         : rightWrist
         : leftShoulder
         : leftElbow
         : leftWrist
         : rightHip
         : rightKnee
         : rightAnkle
         : leftHip
         : leftKnee
         : leftAnkle
         : rightEye
         : leftEye
         : rightEar
         : leftEar
         : [])    = keyPoints
        -- [KeyPoint 910 720 0.4, ...]
        keyPoints = map (\[x, y, score] -> KeyPoint x y score) 
                        (chunksOf 3 poseKeyPoints)


toThreePoint :: Options -> [[Float]] -> KeyPoint -> ThreePoint
toThreePoint opts depthMap KeyPoint {..} = ThreePoint 
    { x     = x
    , y     = y
    , z     = z
    , score = score
    }
        where
            ly = length depthMap
            lx = length $ head depthMap

            x' = round $ x * fromIntegral (lx `div` videoWidth  opts)
            y' = round $ y * fromIntegral (ly `div` videoHeight opts)

            x'' = min (lx - 1) x'
            y'' = min (ly - 1) y'

            z = (depthMap !! y'') !! x''


asThreePoints :: Options -> Frame Person -> [[Float]] -> [[[ThreePoint]]]
asThreePoints opts frame depthMap = threePoints
    where
        keyPoints   = asKeyPoints False frame
        threePoints = (map . map . map) (toThreePoint opts depthMap) keyPoints


asKeyPoints :: Bool -> Frame Person -> [[[KeyPoint]]]
asKeyPoints cleanup frame = keyPoints
    where
        components :: [Skeleton2D]
        components = map toSkeleton (getField @"people" frame)
        keyPoints  = map (keyPointPaths cleanup True) components


keyPointPaths :: Bool -> Bool -> Skeleton2D -> [[KeyPoint]]
keyPointPaths cleanup includePelvis Skeleton {..} =
    map op
        [ rightFace
        , leftFace
        , spine
        , body
        , rightArm
        , leftArm
        , rightLeg
        , leftLeg
        ]
    where
        rightFace = [nose, rightEye, rightEar]
        leftFace  = [nose, leftEye, leftEar]
        spine     = [nose, neck]

        (hipJointPoint, body) = if includePelvis then (pelvis, [neck, pelvis])
                                                 else (neck, [])
        
        -- Let's construct a pelvis
        (KeyPoint x1 y1 s1) = leftHip
        (KeyPoint x2 y2 s2) = rightHip
        px = (x1 + x2) / 2
        py = (y1 + y2) / 2
        pelvis = KeyPoint px py (min s1 s2)

        rightArm  = [neck, rightShoulder, rightElbow, rightWrist]
        leftArm   = [neck, leftShoulder, leftElbow, leftWrist]
        rightLeg  = [hipJointPoint, rightHip, rightKnee, rightAnkle]
        leftLeg   = [hipJointPoint, leftHip, leftKnee, leftAnkle]

        -- Drop any zero-score elements
        op        = if cleanup then dropEmpty else id
        dropEmpty = takeWhile (\(KeyPoint _ _ s) -> (s /= 0.0))


round2 :: (Fractional a, RealFrac a) => a -> a
round2 f = fromInteger (round $ f * (10^n)) / (10.0^^n)
    where
        n = 2 :: Int


toList :: Skeleton2D -> [KeyPoint]
toList Skeleton {..} =
    [ nose
    , neck
    , rightShoulder
    , rightElbow
    , rightWrist
    , leftShoulder
    , leftElbow
    , leftWrist
    , rightHip
    , rightKnee
    , rightAnkle
    , leftHip
    , leftKnee
    , leftAnkle
    , rightEye
    , leftEye
    , rightEar
    , leftEar
    ]


-- | For a given list of people (in a frame) and a second group of people (in
--   a second, later, frame) compute the matchings; i.e. which people are the
--   same?
--
--   Afterwards, we get a map which says "Person n in frame k is person m in
--   frame (k+1)".
--
--   The first argument is the _later_ frame, and the second argument is the
--   earlier one.
matchings :: [Person] -> [Person] -> [(Person, Maybe Person)]
matchings xs ys = proposed
    -- traceShow ("nubbed: " ++ show ff ++ "..after.." ++ show fg) $ proposed
    where
        -- TODO: It's the diff between foldl and foldr again. Basically, I
        -- just need it to walk these lists in order!
        new' = foldl' g [] sorted
        -- fg = map (\(p, mp) -> (getField @"name" p, fmap (getField @"name") mp)) r
        -- ff = map (\(p1, p2, d) -> (getField @"name" p1, getField @"name" p2, d)) nubbed
        -- Match p1 and p2, unless p2 is already in cs,
        -- in which case we'd assing nothing to p1.
        --
        -- TODO: Put some minimum bound on dh. If it's greater
        -- than 100, then let's just say that nothing matches.
        g cs (p1, p2, _dh) = elt : cs
            where
                elt = if Just p2 `elem` map snd cs
                         then (p1, Nothing)
                         else (p1, Just p2)

        combs :: [(Person, Person)]
        combs = ap (map (,) xs) ys

        -- diffs = map (\(p1, p2) -> (p1, p2, diff (neck (toSkeleton p1)) (neck (toSkeleton p2)))) combs
        diffs = map (\(p1, p2) -> ( p1
                                  , p2
                                  , toSkeleton p1 `cartesianDifference` toSkeleton p2
                                  )) combs

        -- Sort things; smallest first
        sorted   = sortBy (\(_, _, d1) (_, _, d2) -> d1 `compare` d2) diffs
        noMaybes = filter (\(_, m) -> m /= Nothing) new'
        proposed = nubBy (\(p1, _) (p2, _) -> p1 == p2) (reverse noMaybes)


-- | Given some matchings, update the names. We will either yield the same
--   person from frame n, or the person from frame m with the name of the person
--   from frame n.
--
--   TODO: This is the problem. We always need to be yielding the same person,
--   just with a different name
--
applyMatchings :: [(Person, Maybe Person)] -> [Person]
applyMatchings = map go
    where
        go (p,  Nothing) = p
        -- Update the names
        go (p1, Just p2) = setField @"name" (getField @"name" p2) p1


diff :: KeyPoint -> KeyPoint -> Float
diff k1 k2 = dh
    where
        s1 = getField @"score" k1
        s2 = getField @"score" k2

        dx = (s1 * getField @"x" k1) - (s2 * getField @"x" k2)
        dy = (s1 * getField @"y" k1) - (s2 * getField @"y" k2)

        dh' = dx ** 2 + dy ** 2
        dh  = if s1 * s2 == 0
                 then 0
                 else dh'


cartesianDifference :: Skeleton2D -> Skeleton2D -> Float
cartesianDifference s1 s2 = dd
    -- traceShow ("dd: " ++ show dd) $ dd
    where
        average xs = sum xs / genericLength xs
        dd = average $ zipWith diff (toList s1) (toList s2)


-- | Calculate the area of the bounding box of a given person. If we
--   weren't able to determine any valid keypoints, just return the
--   area as zero.
area :: Person -> Float
area p@Person {..} = go keyPoints
    where
        skeleton  = toSkeleton p
        keyPoints = concat $ keyPointPaths True False skeleton

        go [] = 0
        go kpps = h * w
            where
                kx   = map (\KeyPoint {..} -> x) kpps
                ky   = map (\KeyPoint {..} -> y) kpps
                maxx = maximum kx
                maxy = maximum ky
                minx = minimum kx
                miny = minimum ky
                w    = maxx - minx
                h    = maxy - miny


-- | Take always the biggest person in the frame.
takeLargest :: Frame Person -> Frame Person
takeLargest frame@Frame {..} = newFrame (getField @"people" frame)
    where
        -- If there are no people, just return the frame,
        -- otherwise return the biggest.
        newFrame [] = frame
        newFrame ps = setField @"people" [maximumBy biggest ps] frame
        biggest a b = area a `compare` area b


-- | Require that each frame has exactly one person.
onePerson :: Frame Person -> Bool
onePerson f = length (getField @"people" f) == 1



-- | We are given a big bunch of frames; say N. We would like
--   to evenly sample `m` frames from this set. We will just 
--   divide N by k and take every kth element.
--
--   [0, k*(N/m), ... | k <- [0..m]]
--
--  e.g. k = 2
--
--   [0, N/2]
--
sampleFrames :: Int -> [Frame Person] -> [Frame Person]
sampleFrames quantity inFrames = frames
    where
        totalFrames = length inFrames
        stepSize    = totalFrames `div` quantity
        frames      = [ inFrames !! (k * stepSize) | k <- [0 .. quantity] ]
