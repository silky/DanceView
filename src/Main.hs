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

-- TODO:
--
-- Use "start" and "end" times.
-- Body Rotation Options?
-- Filter Out Non-Moving People?
--
-- TODO: Print time
--
-- TODO: It currently crashes when it ends. We should stop the animation (or
-- loop it) when we run out of time.
--
-- TODO:
--   - If we're going for POAM we need to have a bunch of configurations that
--   let us design a particular item and then emit a file of the right type.
--
--   Basically, we need to cut off the thing. Then it works.
--
-- TODO: Visible body-part count filter
--
-- TODO: Person Re-Identification

module Main where


import           Data
import           Animation
import           Gif
import           Montage
import           DanceView
import           Data.List             hiding (find)
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

    let -- Hahaha ... A bit of insanity around assigning incremental ids to
        -- people, which converts them to `Person` instead of `PersonData`.
        fds :: [FrameData Person]
        fds = map (\fd -> FrameData (
                        zipWith (flip smash . Person []) [1..] (getField @"people" fd)
                        )
                  ) (drop 1000 $ take 2000 $ frameDatas)

        -- | Munge them into frames with frame numbers
        frames' :: [Frame Person]
        frames' = zipWith (flip smash . Frame []) [1..] fds

        -- | We now build up frame-pairs that is (frame_{n}, franem_{n+1}).
        framePairs :: [(Frame Person, Frame Person)]
        framePairs = zip frames' (tail frames')

        -- | We then use those frame-pairs to update our frames with the
        --   new people whom have their ids changed to be the right kind.
        -- 
        --   We need to update 2 based on 1, 3 based on 2, 4 based on 3,
        --   and so on ...
        matchedFrames' :: [Frame Person]
        matchedFrames' = foldl' g [] framePairs
            where
                g fs (fp1, fp2) = setField @"people" (applyMatchings (matches fp2 fp1)) fp2 : fs
                matches a b     = matchings (getField @"people" a) (getField @"people" b)

        matchedFrames  = fst (head framePairs) : matchedFrames'
    
    let frames = 
            case filterOpt opts of 
              Nothing          -> matchedFrames
              Just OnlySolo    -> filter onePerson matchedFrames
              Just TakeLargest -> map takeLargest matchedFrames

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

