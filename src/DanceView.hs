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


asKeyPoints :: Frame -> [[KeyPoint]]
asKeyPoints frame = keyPoints
    where
        components :: [Skeleton]
        components = map toSkeleton (people frame)
        keyPoints :: [[KeyPoint]]
        keyPoints  = concatMap keyPointPaths components


keyPointPaths ::  Skeleton -> [[KeyPoint]]
keyPointPaths Skeleton {..} =
    map dropEmpty
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
        body      = [neck, pelvis]
        
        -- Let's construct a pelvis
        (KeyPoint x1 y1 s1) = leftHip
        (KeyPoint x2 y2 s2) = rightHip
        px = (x1 + x2) / 2
        py = (y1 + y2) / 2
        pelvis = KeyPoint px py (min s1 s2)

        rightArm  = [neck, rightShoulder, rightElbow, rightWrist]
        leftArm   = [neck, leftShoulder, leftElbow, leftWrist]
        rightLeg  = [pelvis, rightHip, rightKnee, rightAnkle]
        leftLeg   = [pelvis, leftHip, leftKnee, leftAnkle]

        -- Drop any zero-score elements
        dropEmpty = takeWhile (\(KeyPoint _ _ s) -> (s /= 0.0))


