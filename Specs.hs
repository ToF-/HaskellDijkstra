import Test.Hspec
import Dijkstra
import Data.PSQueue

main = hspec $ do
    describe "minDistanceView" $ do
        it "is the nearest node, and the rest of distant nodes" $ do
            let i = fromList (map (:-> 1000000) [0..3])
                d = update (\p -> Just 0) 2 i
            minDistanceView d  `shouldBe` Just (2 :-> 0, fromList [0:->1000000,1:->1000000,3:->1000000])
