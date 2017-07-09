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
