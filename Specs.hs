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
