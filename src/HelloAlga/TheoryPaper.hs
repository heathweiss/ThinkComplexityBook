{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Work through the paper for algebraic-graphs.

https://dl.acm.org/doi/pdf/10.1145/3122955.3122956

-}
module HelloAlga.TheoryPaper() where

import Algebra.Graph
import Algebra.Graph.Export.Dot

import Data.String
import Test.HUnit

import RIO hiding (ByteString)
import RIO.Text (Text)
import qualified RIO.Set as Set
import qualified Prelude as P

----------------------------------------------------------------------------------------------------------------------
-------------------------------------------------- 2: The Core -------------------------------------------------------

myEmptyIntGraph :: Graph Int
myEmptyIntGraph = Empty

myVertexGraph1 :: Graph Int
myVertexGraph1 = Vertex 1

myVertexGraph2 :: Graph Int
myVertexGraph2 = Vertex 1

equalityTests = do
  let   
    vertex1IsNotEqualToVertex2 = do
  
     TestCase
      (do
         assertEqual
          "2 diferent Vertex: They are not equal"
          False
          (myVertexGraph1 == myVertexGraph2)
        )
  _ <- runTestTT vertex1IsNotEqualToVertex2

  let   
    vertex1IsNotEqualToVertex1Equivalent = do
  
     TestCase
      (do
         assertEqual
          "2 of the same Vertex: They are equal"
          True
          (myVertexGraph1 == (Vertex 1))
        )
  _ <- runTestTT vertex1IsNotEqualToVertex1Equivalent
  return ()


myVertexGraph3 :: Graph Int
myVertexGraph3 = Vertex 3

myOverlayGraph :: Graph Int
myOverlayGraph = overlay myVertexGraph3 $ overlay myVertexGraph1 myVertexGraph2

myConnectGraph :: Graph Int
myConnectGraph = connect myVertexGraph1 myVertexGraph2

{-
Try the 1 -> (2 + 3) example
-}
myPathVertexes =
  Vertex 1 
  `connect`
  (Vertex 2 `overlay` Vertex 3)
  

{-
Try the 1 -> 2 + 2 -> 3 example
-}
myPathVertexes2 =
  (connect myVertexGraph1 myVertexGraph2) 
  `overlay`
  (connect myVertexGraph2 myVertexGraph3)

myEdge = 
  edge 1 2

myVertices = vertices [(Vertex 1), (Vertex 2), (Vertex 3)]
{-
Overlay 
  (Vertex (Vertex 1))
  (Overlay (Vertex (Vertex 2)) (Vertex (Vertex 3))
  )
-}


{-
Algebra.Graph.Export.Dot

Figure out how to export to view with graphViz

Or should I convet it to a python graph, then use that system.
Would have to create a graphql server, and all that.
-}
showMyVertices = P.print $ export style (1 * 2 + 3 * 4 * 5 :: Graph Int) -- myVertices

style :: Style Int String
style = Style
    { graphName               = "Example"
    , preamble                = ["  // This is an example", ""]
    , graphAttributes         = ["label" := "Example", "labelloc" := "top"]
    , defaultVertexAttributes = ["shape" := "circle"]
    , defaultEdgeAttributes   = mempty
    , vertexName              = \x   -> "v" ++ show x
    , vertexAttributes        = \x   -> ["color" := "blue"   | odd x      ]
    , edgeAttributes          = \x y -> ["style" := "dashed" | odd (x * y)]
    , attributeQuoting        = DoubleQuotes }

------------------------------------------------------------------------------------------------------------------
--create a text graph to go with the people graph example in chap 2
myPersonVertex :: Graph Text
myPersonVertex = Vertex ("Heath" :: Text)