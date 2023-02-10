from python_graphql_client import GraphqlClient
import networkx as nx 
from matplotlib import pyplot as plt
import numpy as np
"""
Query server for a ring lattice of people.
Load into a networkx graph, and display with matplotlib.
Depending on which server I run, will choose what is the souce of the nodes and edges,
but should make no difference to this module.
"""

client = GraphqlClient(endpoint="http://localhost:3000/api")

queryPeopleNodes = """ 
        query($graphTxtQVar:String!) 
          { peopleNodeQuery
            (txtNodeQArg : $graphTxtQVar)
            {            
              txtNodesQDT
               { txtNodeQDT}
            }
          }
          
        """ 

def getPeopleNodes():
    return client.execute(query=queryPeopleNodes, variables={'graphTxtQVar' : "not used"})
peopleNodesData = getPeopleNodes()
#print(peopleNodesData)

def addNodes():
    G = nx.DiGraph()
    for person in peopleNodesData['data']['peopleNodeQuery']['txtNodesQDT']: #:
        G.add_node(str(person['txtNodeQDT']))
    return G
G = addNodes()


#---------------------------------------------- load the edges ------------------------------------------
queryPeopleEdges = """
   query ($graphTxtQVar:Int!) 
     { peopleRingeLatticeEdgeQuery
        (txtEdgeQArg : $graphTxtQVar)  
            { txtEdge1
              txtEdge2
            } 
     }
   """


def getPeopleEdges():
    return client.execute(query=queryPeopleEdges, variables={'graphTxtQVar' : 2})
peopleEdgePairsStrong = getPeopleEdges()
#print(peopleEdgePairsStrong)
"""for edges in peopleEdgePairsStrong['data']['peopleRingeLatticeEdgeQuery']:
    print(edges['txtEdge1'])
    print(edges['txtEdge2'])
"""
#task:
#  Yield the 2 nodes that make up each edge from the graphQl query 'queryPeopleEdges'
def edges_from_edge_pairs():
    for edges in peopleEdgePairsStrong['data']['peopleRingeLatticeEdgeQuery']:
      yield (edges['txtEdge1']), (edges['txtEdge2'])

#G.add_edges_from(edges_from_edge_pairs(peopleEdgePairs))
G.add_edges_from(edges_from_edge_pairs())
nx.draw_circular(G, node_size=2000, with_labels=True)
plt.show()