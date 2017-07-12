import Test.Hspec
import Dijkstra
import Data.PSQueue

main = hspec $ do
    describe "a distance" $ do
        describe "when defined" $ do
            let d = Distance (100,1) (Just 0)
            it "has a cost " $ do
                cost d  `shouldBe` (100,1)  

            it "has a via node" $ do
                via d  `shouldBe` (Just 0) 
            
        describe "when undefined" $ do
            let d = infinity
            it "has an infinite cost " $ do
                cost d  `shouldBe` (maxBound,maxBound)

            it "has no via node" $ do
                via d  `shouldBe` Nothing
