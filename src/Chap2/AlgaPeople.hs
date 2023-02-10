{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE FlexibleInstances #-}
{- |
Create a Ring Lattice of people using the Alga graph library.
-}
module Chap2.AlgaPeople(theListOfPeople) where

import Algebra.Graph

import Data.String
import Test.HUnit

import RIO hiding (ByteString)
import RIO.Text (Text)
import qualified RIO.Set as Set
import qualified Prelude as P

import GraphQl.RingLattice(ToRingLattice(..))
import GraphQl.Types(ToTextQDT(..))

theListOfPeople :: [Graph Text]
theListOfPeople = map Vertex ["Heath", "Gord","Wayne","Evan","Rodney","Larry","Dennis","Phil"]

instance ToRingLattice (Graph Text) where
  --ringEdges graphList = ringEdges $ toTextList graphList
  --ringEdges2 graphList = ringEdges2 $ toTextList graphList
  --ringEdges3 graphList = ringEdges3 $ toTextList graphList
  ringEdges graphList = ringEdges $ toTextList graphList
  ringEdges2 graphList = ringEdges2 $ toTextList graphList
  ringEdges3 graphList = ringEdges3 $ toTextList graphList

instance ToTextQDT (Graph Text) where
  toTextNodeQDT (Vertex text) = toTextNodeQDT text
  --For now, only 'Vertex' can be converted.
  toTextNodeQDT _             = toTextNodeQDT ("NFG" :: Text)

toText :: Graph Text -> Text
toText (Vertex txt) = txt
toTextList :: [Graph Text] -> [Text]
toTextList graphList' = map toText graphList'