{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE DataKinds                 #-}

module Smoothing where

import           Data
import           Data.List
import           Data.Generics.Record
import Debug.Trace

-- | Given frame_{n} and frame_{n+1}, yield a new frame_{n+1} where
--   the people have had any joints that were 0 made given the value 
--   from f_{n}
forwardFill :: Frame Person -> Frame Person -> Frame Person
forwardFill f0 f1 = setField @"people" smoothed f1
    where
        ps0 = getField @"people" f0
        ps1 = getField @"people" f1

        candidates = map match ps1
        smoothed   = map (uncurry fillPerson) candidates

        match :: Person -> (Maybe Person, Person)
        match p = (find (\p' -> getField @"name" p == getField @"name" p') ps0, p)


-- | Returns a _new_ p1 where it has those values that it thought were
--   zero filled in by the value from p0.
fillPerson :: Maybe Person -> Person -> Person
fillPerson Nothing   p1 = p1
fillPerson (Just p0) p1 = setField @"poseKeyPoints" newKeyPoints p1
    where
        newKeyPoints :: [Float]
        newKeyPoints = zipWith f (getField @"poseKeyPoints" p0) (getField @"poseKeyPoints" p1)
        f k0 k1      = if k1 == 0 then k0 else k1
