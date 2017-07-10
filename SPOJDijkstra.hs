import Data.PSQueue
import Data.Map
import Data.List

main = interact (unlines.process.lines)

type Edge     = (Node, Node, Distance)
type Neighbor = (Node, Distance)
type RouteMap = Map Node [Neighbor]
type Distance = Int
type NodeCount= Int
type Cost     = (Distance,NodeCount)
type Node     = Int
data Route    = Route Cost (Maybe Node)
    deriving (Eq,Ord,Show)
type Itinerary = PSQ Node Route

infinite :: Cost
infinite = (maxBound,maxBound)

cost :: Route -> Cost
cost (Route c _) = c

distance :: Route -> Distance
distance (Route (d,c) _) = d

undefinedRoute :: Route
undefinedRoute = Route infinite Nothing

via :: Route -> Maybe Node
via (Route _ n) = n

route :: Cost -> Node -> Route
route c n = Route c (Just n)

initialRoute :: Route
initialRoute = Route (0,0) Nothing

itinerary :: [Node] -> Node -> Itinerary
itinerary ns st = Data.PSQueue.fromList $ Prelude.map initRoute ns
    where
    initRoute :: Node -> Binding Node Route
    initRoute n | n == st   = n :-> initialRoute
                | otherwise = n :-> Route infinite Nothing

updateRoute :: Cost -> Node -> Route -> Route
updateRoute c n r | c < cost r = Route c (Just n)
                  | otherwise  = r


updateItinerary :: Itinerary -> [Neighbor] -> Neighbor -> NodeCount -> Itinerary
updateItinerary i [] _ _ = i
updateItinerary i ((n,d):nds) (via,dist) nc = updateItinerary adjusted nds (via,dist) nc
    where
    adjusted :: Itinerary
    adjusted = Data.PSQueue.adjust (updateRoute (dist+d,nc) via) n i

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
nodes r = nub $ sort $ (Data.Map.keys r) ++ Data.List.map fst (concat (Data.Map.elems r))

routes :: RouteMap -> Node -> Itinerary
routes r n = calcRoutes r Data.PSQueue.empty (itinerary (nodes r) n)

calcRoutes :: RouteMap -> Itinerary -> Itinerary -> Itinerary
calcRoutes m dest srce = case Data.PSQueue.minView srce of
                    Nothing -> dest
                    Just ((n :-> Route (dist,nc) v),srce') -> calcRoutes m dest' srce''
                        where
                        dest' = Data.PSQueue.insert n (Route (dist,nc) v) dest
                        srce''= updateItinerary srce' (neighbors n m) (n,dist) (nc+1)
                                
 
shortest :: RouteMap -> Node -> Node -> [(Node,Distance)]
shortest m s e = reverse (pathTo (Just e) (routes m s))

pathTo :: Maybe Node -> Itinerary -> [(Node,Distance)]
pathTo Nothing _ = []
pathTo (Just n) i = case Data.PSQueue.lookup n i of
    Just (Route (d,c) v) -> (n,d) : pathTo v i

solve :: [[Int]] -> (Distance,[Node])
solve ([n,start,end]:edges) = let
    m = routeMap (Data.List.map (\[a,b,c]->(a,b,c)) edges) 
    p = shortest m start end
    d = snd (last p)
    ns= Data.List.map fst p
    in (d,ns)
solve ns = error $ "unexpected entry:" ++ (show ns)

process :: [String] -> [String]
process ss = let p = Data.List.map (Data.List.map read . words) ss 
                 (d,ns) = solve p
             in [show d, concat (intersperse " " (Data.List.map show ns))]
