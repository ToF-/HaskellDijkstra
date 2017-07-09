import Test.Hspec
import Dijkstra
import Data.PSQueue

main = hspec $ do
    describe "a route" $ do
        describe "when undefined" $ do
            it "has an infinite distance" $ do
                distance undefinedRoute `shouldBe` infinite

            it "has no via node" $ do
                via undefinedRoute `shouldBe` Nothing

        describe "when defined" $ do
            let r = route 100 4807
            it "has a finite distance" $ do
                distance r `shouldBe` 100

            it "has a via node" $ do
                via r `shouldBe` Just 4807
                
