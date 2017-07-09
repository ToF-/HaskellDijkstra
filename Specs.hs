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

        describe "when initial" $ do
            let r = initial
            it "has a null distance" $ do
                distance r `shouldBe` 0

            it "has no via node" $ do
                via r `shouldBe` Nothing

        describe "can be updated" $ do
            let r = undefinedRoute
                r'= updateRoute r 100 4807 
            it "with a smaller distance" $ do
                r' `shouldBe` Route 100 (Just 4807)
            it "and with a smaller distance only" $ do
                let r'' = updateRoute r' 150 42
                r'' `shouldBe` r' 

            

    describe "an itinerary" $ do
        it "can be initialized with a list of nodes and a starting node" $ do
            let i = itinerary [0..4] 3
            toList i  `shouldBe` [0 :-> Route infinite Nothing
                                 ,1 :-> Route infinite Nothing
                                 ,2 :-> Route infinite Nothing
                                 ,3 :-> Route 0 Nothing
                                 ,4 :-> Route infinite Nothing] 
            
                
