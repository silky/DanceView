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
{-# LANGUAGE DataKinds                 #-}

-- TODO: Use "start" and "end" times.
-- TODO: Body Rotation Options?
-- TODO: Filter Out Non-Moving People?
-- TODO: Print time
-- TODO: It currently crashes when it ends. We should stop the animation (or
--       loop it) when we run out of time.
-- TODO: If we're going for POAM we need to have a bunch of configurations that
--       let us design a particular item and then emit a file of the right type.
--
--       Basically, we need to cut off the thing. Then it works.
-- TODO: Visible body-part count filter

module Main where


import           Data
import           Animation
import           Gif
import           Montage
import           Smoothing
import           DanceView
import           Data.Generics.Record
import           Control.Monad
import           Options.Generic
import           Data.Maybe
import           Data.Aeson
import           System.FilePath
import           System.FilePath.Find   hiding (directory)
import           Data.String.Conv       (toS)
import qualified Data.ByteString        as B
import qualified Data.ByteString.Lazy   as LB


main :: IO ()
main = do
    opts :: Options <- unwrapRecord "DanceView - Watch the poses generated by pose networks."

    jsonFiles  <- find (depth ==? 0) (extension ==? ".json") (sourceDirectory opts)

    frameDatas :: [FrameData PersonData] <- mapM readJson jsonFiles

    let fds :: [FrameData Person]
        fds = map (FrameData .  map ((flip smash . Person []) "0") . getField @"people")
                  frameDatas
 

        -- | Munge them into frames with frame numbers.
        frames' :: [Frame Person]
        frames' = zipWith (flip smash . Frame []) [1..] fds


        -- Note: Pattern-match failure if there is not at least 1 frame
        (frame1:remainingFrames) = frames'

        matchedFrames :: [Frame Person]
        matchedFrames = map snd $ scanl f (1, frame1) remainingFrames
            where
                f :: (Int, Frame Person) -> Frame Person -> (Int, Frame Person)
                f (counter, prevFrame) curFrame = (counter', newFrame)
                    where
                        counter'  = counter + length (getField @"people" curFrame)

                        newPeople :: [Person]
                        newPeople = zipWith rename matches names

                        rename (p1, Nothing, _) name = setField @"name" (show name) p1
                        rename (p1, Just p2, _) _    = setField @"name" (getField @"name" p2) p1

                        names     = [counter ..]
                        matches   = matchings (getField @"people" curFrame)
                                              (getField @"people" prevFrame)

                        -- Update the people in this frame
                        newFrame :: Frame Person
                        newFrame = setField @"people" newPeople curFrame

        (frame1':remainingFrames') = matchedFrames
        finalFrames = scanl forwardFill frame1' remainingFrames'

    let frames = 
            case filterOpt opts of 
              Nothing          -> finalFrames
              Just OnlySolo    -> filter onePerson finalFrames
              Just TakeLargest -> map takeLargest finalFrames

    case opts of
      DoAnimation {..} -> doAnimation frames opts
      DoMontage   {..} -> doMontage   frames opts
      DoGif       {..} -> doGif       frames opts
      JsonExport  {..} -> jsonExport  frames opts



readJson :: (FromJSON a) => FilePath -> IO a
readJson f = do
    -- Note: We strictly read here because otherwise we die from too many open
    -- files. There's probably a nicer way to do this, but hey.
    file <- B.readFile f

    return $ fromMaybe (error $ "Couldn't load data from file: " ++ show file)
                       (decode' (toS file)) 


-- | Writes what we know back to a convenient JSON format.
jsonExport :: [Frame Person] -> Options -> IO ()
jsonExport frames opts = 
    if withDepth opts
       then
        (do
            let directory = sourceDirectory opts </> "depth"
            threePoints <- zipWithM (\k f -> do dm <- readJson (directory </> show k <.> "json")
                                                return $ asThreePoints opts f dm
                                    ) [( 0 :: Integer) ..] frames
            LB.writeFile (outFile opts) (encode threePoints))
       else
        (do
            let skeletonFrames = map (\(Frame p s) -> Frame (map toSkeleton p) s) frames
            LB.writeFile (outFile opts) (encode skeletonFrames))

