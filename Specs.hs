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
                r'= updateRoute 100 4807 r
            it "with a smaller distance" $ do
                r' `shouldBe` Route 100 (Just 4807)
            it "and with a smaller distance only" $ do
                let r'' = updateRoute 150 42 r'
                r'' `shouldBe` r' 

    describe "an itinerary" $ do
        let i = itinerary [0..4] 3
        it "can be initialized with a list of nodes and a starting node" $ do
            toList i  `shouldBe` [0 :-> Route infinite Nothing
                                 ,1 :-> Route infinite Nothing
                                 ,2 :-> Route infinite Nothing
                                 ,3 :-> Route 0 Nothing
                                 ,4 :-> Route infinite Nothing] 

        it "can be updated with a list of neighbors to a node" $ do
            let i'= updateItinerary i [(0,200),(1,400)] (3,0)
            toList i' `shouldBe` [0 :-> Route 200 (Just 3)
                                 ,1 :-> Route 400 (Just 3)
                                 ,2 :-> Route infinite Nothing
                                 ,3 :-> Route 0 Nothing
                                 ,4 :-> Route infinite Nothing] 
            let i''= updateItinerary i' [(2,300)] (0,200)
            toList i'' `shouldBe` [0 :-> Route 200 (Just 3)
                                 ,1 :-> Route 400 (Just 3)
                                 ,2 :-> Route 500 (Just 0) 
                                 ,3 :-> Route 0 Nothing
                                 ,4 :-> Route infinite Nothing] 
    describe "a routeMap" $ do
        let m = routeMap [(0,1,300),(0,2,400),(0,3,200)
                         ,(1,4,200),(1,2,400),(2,4,600)
                         ,(3,4,100),(3,5,400),(4,5,200)]
        it "can be searched for neighbors of a node" $ do
            neighbors 0 m `shouldBe` [(1,300),(2,400),(3,200)] 
    

   
            
                
