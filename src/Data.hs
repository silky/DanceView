{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE UndecidableInstances      #-}

module Data where

import           GHC.Generics
import           Options.Generic
import           Data.Aeson
--
-- | Non-type safe options. Sad days.
data Options' w = 
        DoAnimation
            { videoWidth           :: w ::: Int         <?> "Video width in pixels."
            , videoHeight          :: w ::: Int         <?> "Video height in pixels."
            , sourceDirectory :: w ::: FilePath    <?> "Directory in which to find the pose annotation json files."
            , start           :: w ::: Maybe Float <?> "Time (in minutes) at which we should start."
            , end             :: w ::: Maybe Float <?> "Time (in minutes) at which we should end."
            , fps             :: w ::: Float       <?> "Frames per second."
            } 
        | DoMontage
            { videoWidth           :: w ::: Int         <?> "Video width in pixels."
            , videoHeight          :: w ::: Int         <?> "Video height in pixels."
            , sourceDirectory :: w ::: FilePath    <?> "Directory in which to find the pose annotation json files."
            , start           :: w ::: Maybe Float <?> "Time (in minutes) at which we should start."
            , end             :: w ::: Maybe Float <?> "Time (in minutes) at which we should end."
            , rows            :: w ::: Int         <?> "Number of rows in the resulting grid."
            , columns         :: w ::: Int         <?> "Number of columns in the resulting grid."
            , outFile         :: w ::: FilePath    <?> "Name of the file that we output: out.png"
            , outWidth        :: w ::: Double      <?> "Width of the resulting image."
            , outHeight       :: w ::: Double      <?> "Height of the resulting image."
            }
        deriving (Generic)

type Options = Options' Unwrapped

deriving instance Show Options

instance ParseRecord (Options' Wrapped) where 
    -- So we get "sourceDirectory" -> "source-directory"
    parseRecord = parseRecordWithModifiers lispCaseModifiers

newtype Person = Person 
    { poseKeyPoints :: [Float]
    } deriving (Show)

newtype Frame = Frame 
    { people :: [Person]
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

