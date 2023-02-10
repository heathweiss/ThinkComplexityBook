{-# LANGUAGE NoImplicitPrelude #-}
{- |
Create ring lattice graph from various inputs such as Alga, [Text]
-}
module GraphQl.RingLattice(ToRingLattice(..)) where

import RIO hiding (ByteString)
import RIO.Text (Text)
import qualified RIO.Set as Set
import Data.ByteString.Lazy.Char8 (ByteString)

import GraphTypes(TextEdge(..))

----------------------------------------------------------------------------------------------------------
{- |
Given:
  a: represents some type of graph node.
Task:
  convert the [a] into edges represented by [['Text']]
Return:
  A ring lattice with 1, 2, or 3 forward connected edges.
-}
class ToRingLattice a where
  
  ringEdges :: [a] -> [TextEdge]
  ringEdges2 :: [a] -> [TextEdge]
  ringEdges3 :: [a] -> [TextEdge]

instance ToRingLattice Text where
  ringEdges textNodes = ringNodes' textNodes
  ringEdges2 textNodes = ringNodes2' textNodes
  ringEdges3 textNodes = ringNodes3' textNodes
  

{- |
given:
  List of nodes as ['Text']. This is used for base for all higher level node types, such as Alga or FGL nodes.
task:
Create a [[Text]] where each inner [] is a node pair where each node is connected to the following node. The last node gets connected to initial node.
It represents the Chap2 Ring Lattice, of degree 1
return: eg:
  [Heath, Dominic, Dolu] = [[Heath,Dominic],[Dominic,Dolu],[Dolu,Heath]] which is a ring graph.
-}
ringNodes' :: [Text] -> [TextEdge]
ringNodes' [] = []
ringNodes' [node] = []
ringNodes' (node:nodes) =
  base node nodes []
  where
  base :: Text -> [Text] -> [TextEdge] -> [TextEdge]
  base prevNode [] workingList = TextEdge prevNode node : workingList
  base prevNode (n:ns) workingList =
    base n ns (TextEdge prevNode n : workingList)


  
ringNodes2' :: [Text] -> [TextEdge]
ringNodes2' [] = []
ringNodes2' [_] = []
ringNodes2' [_,_] = []
ringNodes2' [heath,dominic,dolu] = [TextEdge heath dolu, TextEdge dominic heath, TextEdge dolu dominic]
ringNodes2' people =
  ringEdges people ++ attach2down people 

attach2down :: [Text] -> [TextEdge]
attach2down [] = []
attach2down [_,_] = []
attach2down [heath,dominic,dolu] = [TextEdge heath dolu, TextEdge dominic heath, TextEdge dolu dominic]
attach2down  (heath:dominic:dolu:people) = 
  base dominic (dolu:people) [TextEdge heath dolu]
  where
  base :: Text ->  [Text] -> [TextEdge] -> [TextEdge]
  base dominic (dolu:kitty:people) workingList =
    base dolu (kitty:people) (TextEdge dominic kitty : workingList)
  base kitty [spike] workingList = 
    TextEdge kitty heath : TextEdge spike dominic : workingList

attach3down :: [Text] -> [TextEdge]
attach3down [] = []
attach3down [_,_] = []
attach3down [_,_,_] = []
attach3down [heath,dominic, dolu,kitty] = [TextEdge heath kitty , TextEdge dominic heath,
                                                 TextEdge dolu dominic, TextEdge kitty dolu]
attach3down (heath:dominic:dolu:kitty:people) =
  base dominic (dolu:kitty:people) [TextEdge heath kitty]
  where
  base :: Text ->  [Text] -> [TextEdge] -> [TextEdge]  
  base dolu2 [kitty,spike] workingList = TextEdge dolu2 heath : TextEdge kitty dominic : TextEdge spike dolu : workingList
  base dominic (dolu:kitty:spike:people) workingList =
    base dolu (kitty:spike:people) (TextEdge dominic spike : workingList)


ringNodes3' :: [Text] -> [TextEdge]
ringNodes3' people = 
  ringEdges people ++ attach2down people ++ attach3down people
  
