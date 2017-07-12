module Dijkstra where


type Node = Int
type Cost = (Length, NodeCount)
type Length = Int
type NodeCount = Int
data Distance = Distance Cost (Maybe Node)

cost :: Distance -> Cost
cost (Distance c _) = c

via :: Distance -> Maybe Node
via (Distance _ n) = n

infinity :: Distance
infinity = Distance (maxBound, maxBound) Nothing
