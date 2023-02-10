{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Chap2.FglPeople(theListOfPeople) where
--import Data.Graph.Inductive.Graph(Node, Graph)

{-type Adj b = [(b,Node)]
type Context a b = (Adj b, Node, a, Adj b)
type Decomp g a b = (Context a b, g a b)
empty :: Graph gr => gr ab-}
import Data.Graph.Inductive.Graph(UNode, LNode, Edge, UEdge, Graph, mkGraph, mkUGraph, empty, (&), buildGr, labNodes)
import Data.Graph.Inductive.PatriciaTree

import Data.Graph.Inductive.Monad
import Data.Graph.Inductive.Monad.IOArray

import GraphQl.RingLattice(ToRingLattice(..))
import GraphQl.Types(ToTextQDT(..))

import RIO hiding (ByteString, (&))
import RIO.Text (Text)
import qualified RIO.Set as Set
import qualified Prelude as P

import Test.HUnit

instance ToTextQDT (LNode Text) where
    toTextNodeQDT (node,label) = toTextNodeQDT label
 
instance ToRingLattice (LNode Text) where
  ringEdges listOfLnodes = ringEdges $ toTextList listOfLnodes
  ringEdges2 listOfLnodes = ringEdges2 $ toTextList listOfLnodes
  ringEdges3 listOfLnodes = ringEdges3 $ toTextList listOfLnodes

toText :: LNode Text -> Text
toText (lNode, txt) = txt
toTextList :: [LNode Text] -> [Text]
toTextList = map toText 

theListOfPeopleAsGraph :: Gr Text Int
theListOfPeopleAsGraph =
  mkGraph [(1,"Heath"), (2,"Dominic"),(3,"Lucy"), (4,"Jose"),(5,"Miriam"),(6,"Josue")] []

showtheListOfPeopleAsGraph = P.print theListOfPeopleAsGraph

-- | Used by GraphQlServer to create graph of people.
-- Same name as exported by Alga and Types modules, so each can be tried in the server.
theListOfPeople = labNodes theListOfPeopleAsGraph
showtheListOfPeople = P.print theListOfPeople


theListOfPeopleTest = do
  let   
    haveLookAttheListOfPeople = do
  
     TestCase
      (do
         assertEqual
          "what is b"
          ((mkGraph [(1,"Heath"),(2,"Dominic")] [])) 
          theListOfPeopleAsGraph
        )
  _ <- runTestTT haveLookAttheListOfPeople

  let
    nowExtractFromMkGraph = do
      TestCase
        (do
           assertEqual
             "extract the nodes"
             [(1,"Heath"),(2,"Dominic")] 
             (theListOfPeople)
            )
  _ <- runTestTT nowExtractFromMkGraph
  
  return ()


