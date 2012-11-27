module SCC(scc, sccEq) where
-- Compute strongly connected components.
-- The graph is represented as a list of (node, neighbour list) pairs.
-- A list of list of nodes is returned, each connected.
-- Original code by John Launchbury.
{-import List(partition)-}

type Node v = (v, [v])
type Edge v = (v, v)

scc :: (Eq node) => [Node node] -> [[node]]
scc ns = sccEq (==) ns

sccEq :: (node->node->Bool) -> [Node node] -> [[node]]
sccEq (===) ns = sccEdgeEq (===) [(x,y) | (x, ys) <- ns, y<-ys] (map fst ns) 

sccEdgeEq :: (node->node->Bool) -> [Edge node] -> [node] -> [[node]]
sccEdgeEq (===) es vs
  = snd (span_tree (new_range reversed_edges)
                   ([],[])
                   (snd (dfs (new_range es) ([],[]) vs) )
        )
  where

    reversed_edges = [(y,x) | (x,y)<-es]

    new_range    []       w = []
    new_range ((x,y):xys) w
         = if x === w
           then y : new_range xys w
           else new_range xys w

    -- new elem using eq!
    elem x []     = False
    elem x (y:ys) = x === y || x `elem` ys

    span_tree r (vs,ns) []   = (vs,ns)
    span_tree r (vs,ns) (x:xs)
         | x `elem` vs = span_tree r (vs, ns) xs
         | otherwise   = case dfs r (x:vs, []) (r x) of (vs',ns') -> span_tree r (vs', (x:ns'):ns) xs

    dfs r (vs,ns)   []   = (vs,ns)
    dfs r (vs,ns) (x:xs) | x `elem` vs = dfs r (vs,ns) xs
                         | otherwise   = case dfs r (x:vs, []) (r x) of (vs',ns') -> dfs r (vs', (x:ns')++ns) xs

