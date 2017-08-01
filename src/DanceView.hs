{-# LANGUAGE RecordWildCards           #-}

module DanceView where

import           Data
import           Data.List.Split        (chunksOf)

toSkeleton :: Person -> Skeleton
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

            -- depthMap = 160 x 128
            x' = round $ x * fromIntegral (lx `div` videoWidth  opts)
            y' = round $ y * fromIntegral (ly `div` videoHeight opts)

            x'' = min (lx - 1) x'
            y'' = min (ly - 1) y'

            z = (depthMap !! y'') !! x''


asThreePoints :: Options -> Frame -> [[Float]] -> [[ThreePoint]]
asThreePoints opts frame depthMap = threePoints
    where
        keyPoints   = asKeyPoints False frame
        threePoints = (map . map) (toThreePoint opts depthMap) keyPoints


asKeyPoints :: Bool -> Frame -> [[KeyPoint]]
asKeyPoints cleanup frame = keyPoints
    where
        components :: [Skeleton]
        components = map toSkeleton (people frame)
        keyPoints :: [[KeyPoint]]
        keyPoints  = concatMap (keyPointPaths cleanup True) components


keyPointPaths :: Bool -> Bool -> Skeleton -> [[KeyPoint]]
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


