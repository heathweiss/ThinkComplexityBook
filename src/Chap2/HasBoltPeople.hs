{-# LANGUAGE OverloadedStrings #-}
module Chap2.HasBoltPeople() where

import Database.Bolt

import Data.Default
import Data.Text
import Control.Monad
import Control.Monad.Except

people :: BoltActionT IO [Text]
people = do records <- query "MATCH (n:Person) RETURN n.name"
            forM records $ \record -> record `at` "n.name"



deletePerson :: Text -> BoltActionT IO [Text]
deletePerson personName = do
  let queryString = "Match (n:Person {:name '" <> personName <> "'}) DETACH DELETE n"
  records <- query queryString
  forM records $ \record -> record `at` "n.name"

insertPerson :: Text -> BoltActionT IO [Text]
insertPerson personName = do
  let queryString = "CREATE (n:Person {name: '" <> personName <> "'}) RETURN n.name"
  --records <- query "CREATE (n:Person {name: 'Heath'}) RETURN n.name"
  records <- query queryString -- "CREATE (n:Person {name: 'Heath'}) RETURN n.name"
  forM records $ \record -> record `at` "n.name"

{-
Given:
groupName: The name of the group to be added.
Task:
Insert the group into the db.
Gets run in main with 'run pipe $'
Return:
The inserted group.
-}
insertGroupOfFriends :: GroupName -> BoltActionT IO [Text]
insertGroupOfFriends groupName  = do
  let queryString = "CREATE (" <> "n:GroupOfFriends { groupName: '" <> groupName <> "' }) RETURN n.groupName"
  records <- query queryString
  forM records $ \record -> record `at` "n.groupName"
{-
 CREATE
  (laurence:Person {name: 'Laurence Fishburne'}),
  (laurence)-[:ACTED_IN]->(theMatrix);
-}




type PersonName = Text
type QueryString = Text
type GroupName = Text
buildGroupOfFriendsQString :: [PersonName] -> GroupName -> QueryString
buildGroupOfFriendsQString friends groupName = 
  --base friends "Create (PeaceRiver:GroupOfFriends), " 
  base friends "CREATE " -- $ "Create (" <> groupName <> ":GroupOfFriends),"
  where
  
  base :: [PersonName] -> Text -> Text
  base [] workingText = workingText --should never get here.
  --base [lastFriend] workingText = workingText <> "(" <> lastFriend <> ":Person {name: '" <> lastFriend <> "'})," 
  base [lastFriend] workingText = workingText <> "(" <> lastFriend <> ":Person {name: '" <> lastFriend <> "'})," 
                                  -- <> "(p)-[:IN_GROUP]->( GroupOfFriends { groupName:" <>  groupName <> "})"
                                  <> "(" <> lastFriend <> ")-[:IN_GROUP]->(" <>  groupName <> ")"
  base (f:friends) workingText = 
    --base friends $ workingText <> "(" <> f <> ":Person {name: '" <> f <> "'})," 
    base friends $ workingText <> "(" <> f <> ":Person {name: '" <> f <> "'})," 
                    -- <> "(" <> f <> ")-[:IN_GROUP]->(" <> groupName <> ")," 
                    <> "(" <> f <> ")-[:IN_GROUP]->(" <> groupName <> ")," 

insertFriendsIntoGroup :: [PersonName] -> GroupName -> BoltActionT IO [Text]
insertFriendsIntoGroup friends group = do
  records <- query $ buildGroupOfFriendsQString friends group
  forM records $ \record -> record `at` "n.name"

showPeaceRiverGroupQString :: IO ()
showPeaceRiverGroupQString = print $ buildGroupOfFriendsQString ["Wayne", "Gord"] "PeaceRiver"
{-
CREATE
  (keanu:Person {name: 'Keanu Reever'}),
  (laurence:Person {name: 'Laurence Fishburne'}),
  (theMatrix:Movie {title: 'The Matrix'}),
  (keanu)-[:ACTED_IN]->(theMatrix),
  (laurence)-[:ACTED_IN]->(theMatrix)
-}
      

{-
nineties :: BoltActionT IO [Text]
nineties = do records <- query "MATCH (nineties:Movie) WHERE nineties.released >= 1990 AND 
                                nineties.released < 2000 RETURN nineties.title"
              forM records $ \record -> record `at` "nineties.title"
-}

main :: IO ()
main = do 
          print "fjklsdfjklsa"
          
          --use the local neoj4 db. Password was set by cypher-shell
          --pipe <- connect $ def { user = "neo4j", password = "neo546698" }
          --use my current instance on neo4j.com
          pipe <- connect True "neo4j+s://85ec53b5.databases.neo4j.io"  7474 10 -- $ def { user = "neo4j", password = "neo546698" }
          --pipe <- connect $ def {user = "neo4j", password = "fxZ_IPWpSuwz5TUXH5MQ4e4Bw7FKBfi23D7fY7I0f7g", 
            --                     port = 7474}--, version = 2
          --insertedPerson <- run pipe $ insertPerson "Cypher" 
          --persons <- run pipe people 
          --forM_ persons print
          insertedGroup <- run pipe $ insertGroupOfFriends "PeaceRiver"
          forM_  insertedGroup print
          
          insertedFriends <- run pipe $ insertFriendsIntoGroup ["Wayne", "Gord"] "PeaceRiver"
          forM_ insertedFriends print
          close pipe