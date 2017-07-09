module Dijkstra where
import Data.PSQueue

type Distance = Int
type Node     = Int
data Route    = Route Distance (Maybe Node)

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
