# HaskellDijkstra - Simple Dijkstras algorithm
(source: http://www.spoj.com/problems/IITKESO207SPA3F1/)
## Problem statement
 
Implement the Dijkstra's algorithm for the simple case of an undirected graph. You will be given two nodes, a source and a destination. You have to compute the shortest path between these two nodes.

## Input format

The input consists of multiple lines. The first line holds three numbers n s d denoting the number of nodes, the source node, and the destination node. Notice that nodes are labelled from 0 to n-1.

The next lines contain the edges in the format i j w which denotes the presence of an edge between nodes i and j of weight w.

## Output format

Your output must be in multiple lines. The first line must be the value (as an integer) of the shortest path length. The next lines must be the nodes on the path in order. If there are multiple paths with the same weight, choose the path that has minimum number of edges

## Sample input

    6 0 5
    0 1 3
    0 2 4
    0 3 2
    1 4 2
    1 2 4
    2 4 6
    3 4 1
    3 5 4
    4 5 2

## Sample output

    5
    0 3 4 5
