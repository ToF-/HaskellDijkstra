import Test.Hspec
import Dijkstra
import Data.PSQueue

main = hspec $ do
    describe "a route" $ do
        describe "when undefined" $ do
            it "has an infinite distance" $ do
                distance undefinedRoute `shouldBe` maxBound

            it "has no via node" $ do
                via undefinedRoute `shouldBe` Nothing

        describe "when defined" $ do
            let r = route (100,2) 4807
            it "has a finite distance" $ do
                distance r `shouldBe` 100

            it "has a via node" $ do
                via r `shouldBe` Just 4807

        describe "when initial" $ do
            let r = initialRoute
            it "has a null distance" $ do
                distance r `shouldBe` 0

            it "has no via node" $ do
                via r `shouldBe` Nothing

        describe "can be updated" $ do
            let r = undefinedRoute
                r'= updateRoute (100,0) 4807 r
            it "with a smaller distance" $ do
                r' `shouldBe` Route (100,0) (Just 4807)
            it "and with a smaller distance only" $ do
                let r'' = updateRoute (150,0) 42 r'
                r'' `shouldBe` r' 

    describe "an itinerary" $ do
        let i = itinerary [0..4] 3
        it "can be initialized with a list of nodes and a starting node" $ do
            toList i  `shouldBe` [0 :-> Route infinite Nothing
                                 ,1 :-> Route infinite Nothing
                                 ,2 :-> Route infinite Nothing
                                 ,3 :-> Route (0,0) Nothing
                                 ,4 :-> Route infinite Nothing] 

        it "can be updated with a list of neighbors to a node" $ do
            let i'= updateItinerary i [(0,200),(1,400)] (3,0) 1
            toList i' `shouldBe` [0 :-> Route (200,1) (Just 3)
                                 ,1 :-> Route (400,1) (Just 3)
                                 ,2 :-> Route infinite Nothing
                                 ,3 :-> Route (0,0) Nothing
                                 ,4 :-> Route infinite Nothing] 
            let i''= updateItinerary i' [(2,300)] (0,200) 1
            toList i'' `shouldBe` [0 :-> Route (200,1) (Just 3) 
                                 ,1 :-> Route (400,1) (Just 3)
                                 ,2 :-> Route (500,1) (Just 0) 
                                 ,3 :-> Route (0,0) Nothing
                                 ,4 :-> Route infinite Nothing] 
    describe "a routeMap" $ do
        let m = routeMap [(0,1,300),(0,2,400),(0,3,200)
                         ,(1,4,200),(1,2,400),(2,4,600)
                         ,(3,4,100),(3,5,400),(4,5,200)]
        it "can be searched for neighbors of a node" $ do
            neighbors 0 m `shouldBe` [(1,300),(2,400),(3,200)] 

        it "has a list of nodes" $ do
            nodes m  `shouldBe` [0,1,2,3,4,5]

    describe "routes" $ do
        it "finds the routes in a route map from a node to all other nodes" $ do
            let m = routeMap [(0,1,300),(0,2,400),(0,3,200)
                             ,(1,4,200),(1,2,400),(2,4,600)
                             ,(3,4,100),(3,5,400),(4,5,200)]

            toList (routes m 0) `shouldBe`  [0 :-> Route (0,0) Nothing
                                            ,1 :-> Route (300,1) (Just 0)
                                            ,2 :-> Route (400,1) (Just 0)
                                            ,3 :-> Route (200,1) (Just 0)
                                            ,4 :-> Route (300,2) (Just 3)
                                            ,5 :-> Route (500,3) (Just 4)]

    describe "shortest" $ do
        it "is the shortest path from a node to another in a route map" $ do
            let m = routeMap [(0,1,300),(0,2,400),(0,3,200)
                             ,(1,4,200),(1,2,400),(2,4,600)
                             ,(3,4,100),(3,5,400),(4,5,200)]
            shortest m 0 5 `shouldBe` [(0,0),(3,200),(4,300),(5,500)]

        it "select the path with less nodes in case where several are possible" $ do
            let m = routeMap [(0,1,100),(0,2,100),(1,3,050),(2,4,100),(3,4,50)]
            toList (routes m 0) `shouldBe` [0 :-> Route (0,0) Nothing
                                           ,1 :-> Route (100,1) (Just 0)
                                           ,2 :-> Route (100,1) (Just 0)
                                           ,3 :-> Route (150,2) (Just 1)
                                           ,4 :-> Route (200,2) (Just 2)]
            shortest m 0 4 `shouldBe` [(0,0),(2,100),(4,200)]
            let m = routeMap [(0,1,100),(0,2,100),(1,4,100),(2,3,50),(3,4,50)]
            toList (routes m 0) `shouldBe` [0 :-> Route (0,0) Nothing
                                           ,1 :-> Route (100,1) (Just 0)
                                           ,2 :-> Route (100,1) (Just 0)
                                           ,3 :-> Route (150,2) (Just 2)
                                           ,4 :-> Route (200,2) (Just 1)]
            shortest m 0 4 `shouldBe` [(0,0),(1,100),(4,200)]

    describe "solve" $ do
        it "solve a shortpath problem from a list of integers" $ do
            let ns = [[6,0,5],[0,1,300],[0,2,400],[0,3,200]
                             ,[1,4,200],[1,2,400],[2,4,600]
                             ,[3,4,100],[3,5,400],[4,5,200]]
            solve ns `shouldBe` (500,[0,3,4,5])

    describe "process" $ do
        it "process a list of string to solve the problem" $ do
            let ss = ["6 0 5","0 1 300","0 2 400","0 3 200"
                             ,"1 4 200","1 2 400","2 4 600"
                             ,"3 4 100","3 5 400","4 5 200"]
            process ss  `shouldBe` ["500","0 3 4 5"]


            


            
            
    

   
            
                
