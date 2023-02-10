{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveAnyClass #-}
--from GraphQl.API
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NoImplicitPrelude #-}
{- |
Server for graphql graphs.
Should there be a separate server, one for each type of input. Eg: [Text] vs Alga vs ...
And should there be a separate server for type of graph, such as Ring Lattice?
-}
module GraphQl.GraphQlServer() where


import Data.Morpheus (interpreter)
import Data.Morpheus.Document (importGQLDocument)
import Data.Morpheus.Types (RootResolver (..), Undefined (..), GQLType(..), ResolverQ(..))
import GHC.Generics

import RIO hiding (ByteString)
import RIO.Text (Text)
import qualified RIO.Set as Set
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified RIO.Map as Map 

import Algebra.Graph

import Web.Scotty
import Control.Monad.IO.Class(liftIO)

import GraphQl.Types(TxtNodesQDT(..), TxtNodeQArgs(..), --TxtEdgeQArg(..), TxtEdgesQDT(..),
                     TxtEdgeQDT(..),
                     TextNodeQDT(..), ToTextQDT(toTextNodeQDT), CityLatLongQDT(..), toCityLatLong, 
                     TxtEdgeQArg(..),  toTxtEdgeQDT)
import GraphQl.RingLattice(ringEdges, ringEdges2, ringEdges3)
--import Chap2.AlgaPeople(theListOfPeople)
import Chap2.FglPeople(theListOfPeople)
import Chap2.AlgaCities(cityMap, )
  
-------------------------------------------------------------------------------------------------
----------------------------------- create the API server ----------------------------------------
data Query m = Query 
  {peopleNodeQuery :: TxtNodeQArgs -> m TxtNodesQDT,
   peopleRingeLatticeEdgeQuery :: TxtEdgeQArg -> m [TxtEdgeQDT], 
   citiesLatLong :: [CityLatLongQDT]
  }
  deriving (Generic,GQLType)

--Represents the people as text.
--Replace with imports of other representations, such as Alga or FGL.
--theListOfPeople = ["Heath", "Dominic", "Dolu", "Kitty", "Spike", "Cory", "Jodi", "Richard", "Lonnie", "Toni"]

runServer :: IO ()
runServer = do
  let
    peopleNodeResolver :: TxtNodeQArgs -> ResolverQ e IO TxtNodesQDT
    peopleNodeResolver (TxtNodeQArgs peopleArg) = 
        pure $ TxtNodesQDT $ map toTextNodeQDT theListOfPeople
        
    peopleRingeLatticeEdgeQueryResolver :: TxtEdgeQArg -> ResolverQ e IO [TxtEdgeQDT]
    peopleRingeLatticeEdgeQueryResolver (TxtEdgeQArg peopleArg) =
      case peopleArg of
        1 -> pure $ map toTxtEdgeQDT $ ringEdges theListOfPeople
        2 -> pure $ map toTxtEdgeQDT $ ringEdges2 theListOfPeople
        3 -> pure $ map toTxtEdgeQDT $ ringEdges3 theListOfPeople
        _ -> pure $ map toTxtEdgeQDT $ ringEdges theListOfPeople

    citiesLatLongResolver :: [CityLatLongQDT]
    citiesLatLongResolver = 
      map toCityLatLong $ Map.elems cityMap
    
    api :: ByteString -> IO ByteString
    api = 
      interpreter rootResolver
      where
      rootResolver :: RootResolver IO () Query Undefined Undefined
      rootResolver = 
        RootResolver 
          {queryResolver = Query { peopleNodeQuery = peopleNodeResolver,
                                   peopleRingeLatticeEdgeQuery = peopleRingeLatticeEdgeQueryResolver,
                                   citiesLatLong = citiesLatLongResolver
                                 }
          }
  scotty 3000 $ post "/api" $ raw =<< (liftIO . api =<< body)