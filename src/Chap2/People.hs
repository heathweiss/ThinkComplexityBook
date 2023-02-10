{- |
Use Data.Graph to recreate the people graph.

Will not use this, till after I try FGL, as it seems limited.
-}
module Chap2.People() where

import Data.Graph(Graph, vertices, edges, buildG)
--import Data.Array(Array(..))

import GraphQl.RingLattice(ToRingLattice(..))
import GraphQl.Types(ToTextQDT(..))

--instance ToTextQDT Graph where
  --toTextQDT (Array vertex vertexs) = toTextQDT "fksjdl"
------------------------------- Data.Graph ---------------------------------------
myGraph :: Graph
myGraph = buildG bounds edges
          where
          bounds = (1,4)
          edges  = [(1,3)]

showMyGraph :: IO ()
showMyGraph = do
  print("my graph: " ++ show myGraph)
  print("edges: " ++ (show . edges) myGraph)
  print("vertices: " ++ (show . vertices) myGraph)
