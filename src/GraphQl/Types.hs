{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveAnyClass #-}

{-  |
Types for converting Haskell ADT's into GraphQl types.
-}
module GraphQl.Types(-- * Text Nodes
                     TxtNodesQDT(..), TxtNodeQArgs(..), TextNodeQDT(..), ToTextQDT(..),
                     -- * Text Edge Pairs
                     TxtEdgeQDT(..), 
                     TxtEdgeQArg(..), 
                     toTxtEdgeQDT,
                     -- * Cities with lat long
                     CityLatLongQDT(..), toCityLatLong
                     ) where

import Data.Morpheus (interpreter)
import Data.Morpheus.Document (importGQLDocument)
import Data.Morpheus.Types (RootResolver (..), Undefined (..), GQLType(..), ResolverQ(..))
import GHC.Generics

import RIO hiding (ByteString)
import RIO.Text (Text)
import qualified RIO.Set as Set
import Data.ByteString.Lazy.Char8 (ByteString)

import GraphQl.RingLattice
import GraphTypes(TextEdge(..))

import Chap2.AlgaCities(City(..))
import Geo.Computations(Point(..))

-- Can't have multiple constructors. Not just the order that matters.
newtype TxtNodesQDT =  TxtNodesQDT {txtNodesQDT :: [TextNodeQDT]} 
                -- |  EmptyTxtQDT
  deriving (Generic,GQLType)

newtype TxtNodeQArgs = TxtNodeQArgs {txtNodeQArg :: Text}
  deriving (Generic,GQLType)

--And this one can only have a singel constructor. Perhaps replace Text with something else?
newtype TextNodeQDT = TextNodeQDT {txtNodeQDT :: Text} 
  deriving (Generic,GQLType)
{- |
Convert various graph(fgl, alga) nodes into 'TextNodeQDT'.
-}
class ToTextQDT a where
  toTextNodeQDT :: a -> TextNodeQDT
  fromTextNodeQDT :: TextNodeQDT -> a

instance ToTextQDT Text where
  toTextNodeQDT text = TextNodeQDT text
  fromTextNodeQDT (TextNodeQDT label) = label


---------------------------------------------------------------------------------------
---------------------------------------- Edges --------------------------------------------
-- | Return a [node1,node2] that represents an edge because
-- graphql doesn't support Tuple, or Map.
-- Morpheus does have a pair type that might be better suited if I can get it to work.
-- Should use a smart constuctor to build to ensure there are 2 and only 2 items in the list.


data TxtEdgeQDT = TxtEdgeQDT {txtEdge1 :: Text, txtEdge2 :: Text}
  deriving (Generic,GQLType)

toTxtEdgeQDT :: TextEdge -> TxtEdgeQDT
toTxtEdgeQDT (TextEdge node1 node2) = TxtEdgeQDT node1 node2


newtype TxtEdgeQArg = TxtEdgeQArg {txtEdgeQArg :: Int}
  deriving (Generic,GQLType)


----------------------------------------------------------------------------------
--create a city with name, lat, long
data CityLatLongQDT = CityLatLongQDT {cityName :: Text, lat :: Double, long :: Double}
  deriving (Generic,GQLType)

toCityLatLong :: City -> CityLatLongQDT
toCityLatLong (City name (Point lat long _ _)) =
  CityLatLongQDT name lat long