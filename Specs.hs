import Test.Hspec
import Dijkstra
import Data.PSQueue

main = hspec $ do
    describe "a route" $ do
        describe "when undefined" $ do
            it "as an infinite distance" $ do
                distance undefinedRoute `shouldBe` infinite
