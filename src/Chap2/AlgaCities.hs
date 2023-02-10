{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{- | Chapter 2 cities with locations and distances done with Alga graph library 

Should look at using the 'gps' module on hackage for the coordinates.
-}
module Chap2.AlgaCities(City(..), cityMap) where

import Algebra.Graph

import Data.String
import Test.HUnit

import RIO hiding (ByteString)
import RIO.Text (Text)
import qualified RIO.Set as Set
import qualified Prelude as P
import qualified RIO.Map as Map

--import GraphQl.RingLattice(ToRingLattice(..))
--import GraphQl.Types(ToTextQDT(..))

import Geo.Types()
import Geo.Computations(Point(..), Distance(..), totalDistance)

--data City = City {_cityName :: Text, _lat :: Int, _long :: Int}
  --deriving (Eq,Show)
data City = City {_cityName :: Text, _point :: Point}

albany  = City "Albany" $ Point 42.6526 (-73.7562) Nothing Nothing
boston  = City "Boston" $ Point 42.3601 (-71.0589) Nothing Nothing
nyc     = City "NYC"    $ Point 40.7128 (-74.0060) Nothing Nothing

cities :: [City]
cities = [albany, boston,nyc
         ]

 
--distanceAlbanyToBoston :: Distance
--distanceAlbanyToBoston = totalDistance [_point albany, _point boston]

distanceBetweenCities :: City -> City -> Distance
distanceBetweenCities city1 city2 = totalDistance [_point city1, _point city2]

type DriveTime = Double

{-
given:
  distance: the 'Distance' between 2 cities
Task:
Calculate the travel time in hours. Round to 1 decmial place.
-}
getDriveTime :: Distance -> DriveTime
getDriveTime distance = 0.1 *  (fromIntegral $ round ((distance / 100000) * 10.25)::Double)

--driveTimeAlbanyToBoston = getDriveTime distanceAlbanyToBoston
driveTimeAlbanyToBoston :: DriveTime
driveTimeAlbanyToBoston = getDriveTime $ distanceBetweenCities albany boston

type CityMap = RIO.Map Text City -- Point
cityMap = 
    Map.insert "Albany" (City "Albany" (Point 42.6526 (-73.7562) Nothing Nothing)) $
    Map.insert "Boston" (City "Boston" (Point 42.3601 (-71.0589) Nothing Nothing)) $ 
    Map.insert "NYC"    (City "NYC"    (Point 40.7128 (-74.0060) Nothing Nothing)) $
    Map.insert "Philly" (City "Philly" (Point 39.9526 (-75.1653) Nothing Nothing)) 
      Map.empty 
{-
cityMap = 
    Map.insert "Boston" (Point (-71.0589) 42.3601 Nothing Nothing) $ 
    Map.insert "Albany" (Point (-73.7562) 42.6526 Nothing Nothing)  Map.empty 

-}
type CityName = Text
{-getCityPoints :: CityName -> Maybe Point
getCityPoints cityName  = Map.lookup cityName cityMap

getCityDistance :: CityName -> CityName -> Maybe Distance
getCityDistance cityName1 cityName2 =
  getPoints (getCityPoints cityName1) (getCityPoints cityName2)
  where
  getPoints :: Maybe Point -> Maybe Point -> Maybe Distance
  getPoints (Just point1) (Just point2) = Just $ totalDistance [point1,point2]-}