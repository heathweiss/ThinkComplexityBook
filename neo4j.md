CREATE
  (keanu:Person {name: 'Keanu Reever'}),
  (laurence:Person {name: 'Laurence Fishburne'}),
  (theMatrix:Movie {title: 'The Matrix'}),
  (keanu)-[:ACTED_IN]->(theMatrix),
  (laurence)-[:ACTED_IN]->(theMatrix)
  
 MATCH (n:Person {name: 'Andy'}) DELETE n;
 
 CREATE
  (laurence:Person {name: 'Laurence Fishburne'}),
  (laurence)-[:ACTED_IN]->(theMatrix);
  
 CREATE (a:Person {name: 'Andy', nationality: 'Irish'})
 CREATE (a:Person {name: 'Helga', nationality: 'Swedish'})
 
 match (n:Person {name: 'Andy', nationality: 'Swedish'}) return n
 ==========================================================================================
 MATCH (n:Product) RETURN n
 
MATCH (:Person)-->(GroupOfFriends)
RETURN GroupOfFriends.groupName

MATCH (n) DETACH DELETE n;

MATCH (p:Person)-[:IN_GROUP]->(g:GroupOfFriends)
WHERE g.groupName = "PeaceRiver"
RETURN p.name;

MATCH (:Person)-[:KNOWS]->(:Person {name: 'Alice'})
RETURN count(*) AS rows

MATCH (n:Person)-[:KNOWS]->(m:Person)
WHERE n.name = 'Alice'
RETURN m AS person

CREATE
  (Gord:Person {name: 'Gord Allen'}),
  (wayne:Person {name: 'Wayne Duperon'}),
  (PeaceRiver:GroupOfFriends {groupName: 'Peace River'}),
  (Gord)-[:IN_GROUP]->(PeaceRiver),
  (Wayne)-[:IN_GROUP]->(PeaceRiver)
  
