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
        , rightArm
        , leftArm
        , rightLeg
        , leftLeg
        ]
    where
        rightFace = [nose, rightEye, rightEar]
        leftFace  = [nose, leftEye, leftEar]
        spine     = [nose, neck]
        rightArm  = [neck, rightShoulder, rightElbow, rightWrist]
        leftArm   = [neck, leftShoulder, leftElbow, leftWrist]
        rightLeg  = [neck, rightHip, rightKnee, rightAnkle]
        leftLeg   = [neck, leftHip, leftKnee, leftAnkle]

        -- Drop any zero-score elements
        dropEmpty = takeWhile (\(KeyPoint _ _ s) -> (s /= 0.0))


