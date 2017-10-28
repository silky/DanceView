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

import           Data
import           DanceView
import           DiagramsStuff
import           Diagrams.Prelude hiding (Unwrapped
                                         , Options
                                         , Wrapped
                                         )
import           Diagrams.Backend.Cairo
import           GHC.Generics
import           Options.Generic


data GradientOptions' w = G
            { videoWidth      :: w ::: Int            <?> "Video width in pixels."
            , videoHeight     :: w ::: Int            <?> "Video height in pixels."
            , sourceDirectory :: w ::: Maybe FilePath <?> "Directory in which to find the pose annotation json files."
            , sourceJson      :: w ::: Maybe FilePath <?> "Source JSON file."
            , start           :: w ::: Maybe Float    <?> "Time (in minutes) at which we should start."
            , end             :: w ::: Maybe Float    <?> "Time (in minutes) at which we should end."
            , outFile         :: w ::: FilePath       <?> "Name of the file that we output."
            , fps             :: w ::: Float          <?> "Frames per second."
            , rows            :: w ::: Maybe Int      <?> "Number of rows in the resulting grid."
            , columns         :: w ::: Maybe Int      <?> "Number of columns in the resulting grid."
            , outWidth        :: w ::: Maybe Double   <?> "Width of the resulting image."
            , outHeight       :: w ::: Maybe Double   <?> "Height of the resulting image."
            , filterOpt       :: w ::: Maybe Filter   <?> "How to filter the results, if at all."
            } deriving (Generic)

type GradientOptions = GradientOptions' Unwrapped
deriving instance Show GradientOptions
instance ParseRecord (GradientOptions' Wrapped) where 
    -- So we get "sourceDirectory" -> "source-directory"
    parseRecord = parseRecordWithModifiers lispCaseModifiers


main :: IO ()
main = do
    let size = mkSizeSpec $ V2 (Just 100) (Just 100)

    opts :: GradientOptions <- unwrapRecord "Gradients ..."

    renderCairo "a.png" size diag


diag :: Diagram B
diag = circle 1
