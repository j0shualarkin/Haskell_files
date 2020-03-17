module Graphs where 
import qualified Data.Map as Map
{-
createa a data structure for a graph

a graph is a set of vertices and an adj-matrix
  where for each vert v, 
    there is a list of vertices that v has an edge to

implement the functions:

numVertices :: Graph -> Int 

numEdges :: Graph -> Vertice -> Int

hasEdge :: Graph -> Vertice -> Vertice -> Bool

adjacent :: Graph -> Vertice -> [Vertice]
  returns the vertices that have an edge 
  from the given vertice in the given graph

twoHopsAway :: Graph -> Vertice -> [Vertice]

topological :: Graph -> Graph 

    -} 
type Vertex = Int 
type Vertices = [Vertex]
type Edge = (Vertex, Vertices)
type Graph = [Edge]

mtg :: Graph
mtg = []

hasVertex :: Vertex -> Graph -> Bool
hasVertex _ [] = False 
hasVertex v ((x,_):xs) 
    | v == x = True 
    | otherwise = hasVertex v xs

hasEdge :: (Vertex,Vertex) -> Graph -> Bool
hasEdge (v1,v2) g 
    | hasVertex v1 g = v2 `elem` (adjacent v1 g)
    | otherwise = False

adjacent :: Vertex -> Graph -> [Vertex]
adjacent v [] = []
adjacent v ((x,ex):xs)
    | v == x    = ex 
    | otherwise = adjacent v xs

addVertex :: Vertex -> Graph -> Graph
addVertex new [] = [(new,[])]
addVertex new g 
    | hasVertex new g = g 
    | otherwise       = (new,[]):g
    
    
insert :: (Vertex,Vertex) -> Graph -> Graph
insert (p,q) [] = [(p,[q])]
insert (p,q) ((x, ex):xs)
    | p == x = ((x,(q:ex)):xs)
    | otherwise = (x,ex):(insert (p,q) xs) 

addEdge :: Vertex -> Vertex -> Graph -> Graph 
addEdge v1 v2 g = (insert (v1,v2) g2)
    where g1 = addVertex v1 g
          g2 = addVertex v2 g1

numVertices :: Graph -> Int 
numVertices = foldr (\_ ans -> (1+ans)) 0

vertices :: Graph -> [Vertex]
vertices g = [ x | (x,_) <- g]


bfs :: Vertex -> Vertex -> Graph -> Bool
bfs src tgt g = bfs' src tgt g []

bfs' :: Vertex -> Vertex -> Graph -> [Vertex] -> Bool
bfs' src tgt g visited = 
    src == tgt || or [ bfs' nghbr tgt g (src:visited) | nghbr <- nghbrs, not (nghbr `elem` visited)]
        where nghbrs = adjacent src g 
{-        
dfs :: Vertex -> Vertex -> Graph -> Bool
dfs src tgt g = dfs' src tgt g []

dfs' :: Vertex -> Vertex -> Graph -> [Vertex] -> Bool
dfs' src tgt g visited 
    | src == tgt = True 
-}
exgraph :: Graph
exgraph = addEdge 1 4 (addEdge 2 3 (addEdge 1 2 (addEdge 0 2 (addEdge 0 1 mtg))))

dag :: Graph
dag = foldr (\pr ans -> case pr of (x,y) -> (addEdge x y ans)) mtg [(1,2),(1,3),(2,3),(2,4),(3,4),(3,5)]

topSort :: Graph -> Vertices
topSort g = topological g (start_map Map.empty)
    where start_map m = Map.fromList [ (e,0) | (e,es) <- g ]

topological :: Graph -> (Map.Map Vertex Int) -> Vertices
topological g in_degree = [] 

{-
    0  --> 1  --> 4
     \     |
      \    >
        >  2  --> 3

-}

-- addEdge 'a' 'b' (G []) 
-- (G [('a', ['b']),
--     ('b', []   )])

-- addEdge 'a' 'c' (G [('a', ['b']),('b', []   )])
--  (G [('a', ['c', 'b']),('b', []   )])

-- addEdge 'b' 'c' (G [('a', ['c', 'b']),('b', []   )])
-- (G [('a', ['c','b']),
--     ('b', ['c'])
--     ('c', [])])