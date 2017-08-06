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


data Filter = OnlySolo
            | TakeLargest
            deriving (Show, Read, Generic)

instance ParseField Filter

-- | Non-type safe options. Sad days.
data Options' w = 
        JsonExport
            { videoWidth      :: w ::: Int          <?> "Video width in pixels."
            , videoHeight     :: w ::: Int          <?> "Video height in pixels."
            , sourceDirectory :: w ::: FilePath     <?> "Directory in which to find the pose annotation json files."
            , outFile         :: w ::: FilePath     <?> "Name of the file that we output."
            , onlySolo        :: w ::: Bool         <?> "Only emit one person per frame."
            , withDepth       :: w ::: Bool         <?> "Also emit the z-coordinate by looking for corresponding Depth data."
            , filterOpt       :: w ::: Maybe Filter <?> "How to filter the results, if at all."
            }
        | DoGif
            { videoWidth      :: w ::: Int          <?> "Video width in pixels."
            , videoHeight     :: w ::: Int          <?> "Video height in pixels."
            , sourceDirectory :: w ::: FilePath     <?> "Directory in which to find the pose annotation json files."
            , start           :: w ::: Maybe Float  <?> "Time (in minutes) at which we should start."
            , end             :: w ::: Maybe Float  <?> "Time (in minutes) at which we should end."
            , fps             :: w ::: Float        <?> "Frames per second."
            , outFile         :: w ::: FilePath     <?> "Name of the file that we output."
            , outWidth        :: w ::: Maybe Double <?> "Width of the resulting image."
            , outHeight       :: w ::: Maybe Double <?> "Height of the resulting image."
            , filterOpt       :: w ::: Maybe Filter <?> "How to filter the results, if at all."
            } 
        | DoAnimation
            { videoWidth      :: w ::: Int          <?> "Video width in pixels."
            , videoHeight     :: w ::: Int          <?> "Video height in pixels."
            , sourceDirectory :: w ::: FilePath     <?> "Directory in which to find the pose annotation json files."
            , start           :: w ::: Maybe Float  <?> "Time (in minutes) at which we should start."
            , end             :: w ::: Maybe Float  <?> "Time (in minutes) at which we should end."
            , fps             :: w ::: Float        <?> "Frames per second."
            , filterOpt       :: w ::: Maybe Filter <?> "How to filter the results, if at all."
            } 
        | DoMontage
            { videoWidth      :: w ::: Int          <?> "Video width in pixels."
            , videoHeight     :: w ::: Int          <?> "Video height in pixels."
            , sourceDirectory :: w ::: FilePath     <?> "Directory in which to find the pose annotation json files."
            , start           :: w ::: Maybe Float  <?> "Time (in minutes) at which we should start."
            , end             :: w ::: Maybe Float  <?> "Time (in minutes) at which we should end."
            , outFile         :: w ::: FilePath     <?> "Name of the file that we output."
            , rows            :: w ::: Int          <?> "Number of rows in the resulting grid."
            , columns         :: w ::: Int          <?> "Number of columns in the resulting grid."
            , outWidth        :: w ::: Maybe Double <?> "Width of the resulting image."
            , outHeight       :: w ::: Maybe Double <?> "Height of the resulting image."
            , filterOpt       :: w ::: Maybe Filter <?> "How to filter the results, if at all."
            }
        deriving (Generic)

type Options = Options' Unwrapped

deriving instance Show Options

instance ParseRecord (Options' Wrapped) where 
    -- So we get "sourceDirectory" -> "source-directory"
    parseRecord = parseRecordWithModifiers lispCaseModifiers



-- | A person, as found in the pose output JSON.
data Person = Person
    { poseKeyPoints :: [Float]
    , name          :: !Integer
    } deriving (Show, Generic, Eq)

instance ToJSON Person

newtype PersonData = PersonData
    { poseKeyPoints :: [Float]
    } deriving (Show, Generic, Eq)

instance ToJSON PersonData

instance FromJSON PersonData where
    parseJSON = withObject "person" $ \o ->
        PersonData <$> o .: "pose_keypoints"
                   


-- | A keypoint is the position of a joint; if they score is 
--   low then there is less confidence about the position.
data KeyPoint = KeyPoint 
    { x     :: !Float
    , y     :: !Float
    , score :: !Float
    } deriving (Show, Generic)

instance ToJSON KeyPoint


-- | A collection of keypoints that define a person. We allow
--   a parameter that should either by a KeyPoint or a ThreePoint.
data Skeleton a = Skeleton
    { nose          :: !a
    , neck          :: !a
    , rightShoulder :: !a
    , rightElbow    :: !a
    , rightWrist    :: !a
    , leftShoulder  :: !a
    , leftElbow     :: !a
    , leftWrist     :: !a
    , rightHip      :: !a
    , rightKnee     :: !a
    , rightAnkle    :: !a
    , leftHip       :: !a
    , leftKnee      :: !a
    , leftAnkle     :: !a
    , rightEye      :: !a
    , leftEye       :: !a
    , rightEar      :: !a
    , leftEar       :: !a

    -- | The name of this skeleton. Used for identification
    --   purposes.
    , name          :: !Integer
    } deriving (Show, Generic)

instance ToJSON (Skeleton KeyPoint)
instance ToJSON (Skeleton ThreePoint)

type Skeleton2D = Skeleton KeyPoint
type Skeleton3D = Skeleton ThreePoint


-- | A frame has people, and also a frame number. We let this
--   be parametrized because sometimes we'll want a frame to
--   have a list of "Person"'s, and at other times we'll like
--   to have a list of "Skeleton"s.
data Frame a = Frame
    { people      :: [a]
    , frameNumber :: !Integer
    } deriving (Show, Generic)

instance ToJSON (Frame Person)
instance ToJSON (Frame Skeleton2D)
instance ToJSON (Frame Skeleton3D)


-- | Frame data coming out of a pose network.
newtype FrameData a = FrameData
    { people :: [a]
    } deriving (Show, Generic)

instance ToJSON (FrameData Person)

instance FromJSON (FrameData PersonData) where
    parseJSON = withObject "frame" $ \o ->
        FrameData <$> o .: "people"


-- | A keypoint with depth information.
data ThreePoint = ThreePoint 
    { x     :: !Float
    , y     :: !Float
    , z     :: !Float
    , score :: !Float
    } deriving (Show, Generic)

instance ToJSON ThreePoint
