module Dijkstra where
import Data.PSQueue
import Data.Map
import Data.List

type Edge     = (Node, Node, Distance)
type Neighbor = (Node, Distance)
type RouteMap = Map Node [Neighbor]
type Distance = Int
type Node     = Int
data Route    = Route Distance (Maybe Node)
    deriving (Eq,Ord,Show)
type Itinerary = PSQ Node Route

infinite :: Distance
infinite = maxBound

distance :: Route -> Distance
distance (Route d _) = d

undefinedRoute :: Route
undefinedRoute = Route infinite Nothing

via :: Route -> Maybe Node
via (Route _ n) = n

route :: Distance -> Node -> Route
route d n = Route d (Just n)

initial :: Route
initial = Route 0 Nothing

itinerary :: [Node] -> Node -> Itinerary
itinerary ns st = Data.PSQueue.fromList $ Prelude.map initRoute ns
    where
    initRoute :: Node -> Binding Node Route
    initRoute n | n == st   = n :-> Route 0 Nothing
                | otherwise = n :-> Route infinite Nothing

updateRoute :: Distance -> Node -> Route -> Route
updateRoute d n r | d < distance r = Route d (Just n)
                  | otherwise      = r


updateItinerary :: Itinerary -> [Neighbor] -> Neighbor -> Itinerary
updateItinerary i [] _ = i
updateItinerary i ((n,d):nds) (v,c) = updateItinerary adjusted nds (v,c)
    where
    adjusted :: Itinerary
    adjusted = Data.PSQueue.adjust (updateRoute (c+d) v) n i

routeMap :: [Edge] -> RouteMap
routeMap = Data.Map.fromList 
    . Prelude.map (\ps -> (fst (head ps),Prelude.map snd ps))
    . groupBy (\a b -> fst a == fst b)
    . sort
    . Prelude.map nodeAndNeighbor
    where
    nodeAndNeighbor (n,m,d) = (n,(m,d))

neighbors :: Node -> RouteMap ->[Neighbor]
neighbors = findWithDefault []

nodes :: RouteMap -> [Node]
nodes r = Data.Map.keys r

routes :: RouteMap -> Node -> Itinerary
routes r n = itinerary (nodes r) n
