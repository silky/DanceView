{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE DataKinds                 #-}

import Test.Hspec

import Data
import DanceView
import Control.Monad
import Data.Generics.Record


rawKps1 = [ 0,0,0,442.1,209.347,0.695157,412.838
          , 219.106,0.479542 , 262.192,217.143,0.473475,189.773,268.065
          , 0.635878,475.367 , 207.423,0.677412,551.767,191.757,0.792323
          , 500.895,133.037 , 0.771379,434.342,365.826,0.563359,383.463
          , 483.28,0.467892 , 0,0,0,477.408,367.805
          , 0.568175,489.1,489.135,0.643315 , 483.238,631.949,0.657399
          , 0,0,0,0,0,0,0
          , 0,0,397.158,191.758 , 0.105753 
          ]

rawKps2 = [ 352.19,226.939,0.643588,442.135,211.243,0.653615,412.839 
          ,  215.217,0.279017,0,0,0,0,0
          , 0,475.393,207.422,0.669059,555.741 , 187.794,0.825329
          , 504.774,129.137,0.751591,446.042,354.142 , 0.493266,491.051
          , 479.335,0.443765,490.999,626.073,0.576288 , 481.309,354.172
          , 0.546769,479.396,477.38,0.55193,481.283 , 618.331,0.206265
          , 350.207,215.221,0.257444,363.899,223.071 , 0.545837,0
          , 0,0,397.153,219.145,0.297215 
          ]


main :: IO ()
main = hspec $ do
    describe "Matchings" $ do
        it "matches the same thing to itself" $ do
            let p1 = Person rawKps1 "a"

            let matches = matchings [p1] [p1]

            matches `shouldBe` [(p1, p1, 0)]


        it "matches something close from frame to frame" $ do
            let p1 = Person rawKps1 "a"
                p2 = Person rawKps2 "b"

            let matches         = matchings [p1] [p2]
                [(p1', p2', _)] = matches

            (p1', p2') `shouldBe` (p1, p2)


        it "matches multiple things that should match, to themselves, in any order" $ do
            let p1 = Person rawKps1 "a"
                p2 = Person rawKps2 "b"

            let combs = [ ([p1, p2], [p1, p2])
                        , ([p2, p1], [p1, p2])
                        , ([p2, p1], [p2, p1])
                        , ([p1, p2], [p2, p1])
                        ]

            forM_ combs $ \(c1, c2) -> do
                let matches = matchings c1 c2
                    [(a, b, _), (c, d, _)] = matches 

                (a, b) `shouldBe` (a, a)
                (c, d) `shouldBe` (c, c)

