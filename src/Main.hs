{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE QuasiQuotes               #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE RecordWildCards           #-}

-- TODO:
--
-- Use "start" and "end" times.
-- Body Rotation Options?
-- Filter Out Non-Moving People?
-- Some Kind Of Person Identification?

module Main where

import           Data.Maybe
import           Data.List.Split        (chunksOf)
import           Data.Generics.Record
import           GHC.Generics
import           Options.Generic
import           Data.Aeson
import           System.FilePath.Find
import qualified Data.ByteString        as B
import           Data.String.Conv       (toS)
import           Graphics.Gloss

data Options' w = Options
        { width           :: w ::: Int         <?> "Video width in pixels."
        , height          :: w ::: Int         <?> "Video height in pixels."
        , fps             :: w ::: Float       <?> "Frames per second."
        , sourceDirectory :: w ::: FilePath    <?> "Directory in which to find the pose annotation json files."
        , start           :: w ::: Maybe Float <?> "Time (in minutes) at which we should start."
        , end             :: w ::: Maybe Float <?> "Time (in minutes) at which we should end."
        } deriving (Generic)

type Options = Options' Unwrapped

deriving instance Show Options

instance ParseRecord (Options' Wrapped) where 
    -- So we get "sourceDirectory" -> "source-directory"
    parseRecord = parseRecordWithModifiers lispCaseModifiers

data Person = Person 
    { poseKeyPoints :: ![Float]
    } deriving (Show)

data Frame = Frame 
    { people :: ![Person]
    } deriving (Show)

instance FromJSON Person where
    parseJSON = withObject "person" $ \o ->
        Person <$> o .: "pose_keypoints"

instance FromJSON Frame where
    parseJSON = withObject "frame" $ \o ->
        Frame <$> o .: "people"

data Skeleton = Skeleton
    { nose          :: !KeyPoint
    , neck          :: !KeyPoint
    , rightShoulder :: !KeyPoint
    , rightElbow    :: !KeyPoint
    , rightWrist    :: !KeyPoint
    , leftShoulder  :: !KeyPoint
    , leftElbow     :: !KeyPoint
    , leftWrist     :: !KeyPoint
    , rightHip      :: !KeyPoint
    , rightKnee     :: !KeyPoint
    , rightAnkle    :: !KeyPoint
    , leftHip       :: !KeyPoint
    , leftKnee      :: !KeyPoint
    , leftAnkle     :: !KeyPoint
    , rightEye      :: !KeyPoint
    , leftEye       :: !KeyPoint
    , rightEar      :: !KeyPoint
    , leftEar       :: !KeyPoint
    } deriving (Show)

data KeyPoint = KeyPoint 
    { x     :: !Float
    , y     :: !Float
    , score :: !Float
    } deriving (Show)


main :: IO ()
main = do
    opts :: Options <- unwrapRecord "DanceView - Watch the poses generated by OpenPose."

    jsonFiles <- find (depth ==? 0) (extension ==? ".json") (getField @"sourceDirectory" opts)
    frames    <- mapM readFrame jsonFiles

    animate (InWindow "DanceView" (width opts, height opts) (0,0))
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
        frame      = frames !! round (elapsed * (fps opts))

        components :: [Skeleton]
        components = map toSkeleton (people frame)

        keyPoints :: [[KeyPoint]]
        keyPoints  = concat $ map keyPointPaths components

        w = fromIntegral (width opts)
        h = fromIntegral (height opts)

        points :: [[Point]]
        points     = (map . map) (\(KeyPoint {..}) -> (x, h - y)) keyPoints
        
        bodies :: [Picture]
        bodies     = map line points


toSkeleton :: Person -> Skeleton
toSkeleton (Person {..}) = Skeleton {..}
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
        keyPoints = map (\(x:y:score:[]) -> KeyPoint x y score) 
                        (chunksOf 3 poseKeyPoints)


keyPointPaths ::  Skeleton -> [[KeyPoint]]
keyPointPaths (Skeleton {..}) =
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


readFrame :: FilePath -> IO Frame
readFrame f = do
    -- Note: We strictly read here because otherwise we die from too many open
    -- files. There's probably a nicer way to do this, but hey.
    file <- B.readFile f

    return $ fromMaybe (error $ "Couldn't load Frame from file: " ++ (show file))
                       (decode' (toS file)) 

