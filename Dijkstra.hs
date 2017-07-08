module Dijkstra where
import Data.PSQueue

minDistanceView :: PSQ Int Int -> Maybe (Binding Int Int,PSQ Int Int)
minDistanceView = minView
