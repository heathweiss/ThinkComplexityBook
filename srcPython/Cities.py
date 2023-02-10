from python_graphql_client import GraphqlClient
import networkx as nx 
from matplotlib import pyplot as plt
import numpy as np
"""
Chapter 2
Create cities that have assoc'd locations, distances, and drive times
"""

client = GraphqlClient(endpoint="http://localhost:3000/api")

citiesQuery =   """
     query
       { citiesLatLong
           {  
               cityName
               lat
               long
           }
       }
   """

def getCities():
    return client.execute(query=citiesQuery)

cities = getCities()
print(cities)

"""for city in cities["data"]["citiesLatLong"]:
    print(city["cityName"])
"""

def getMapOfNodes():
    cityMap = {}
    for city in cities["data"]["citiesLatLong"]:
        cityNamee = city["cityName"]
        cityMap.update({cityNamee : (city["long"],city["lat"])})
    return cityMap
cityNodes = getMapOfNodes()
print(cityNodes)

def addNodes():
    G = nx.Graph()
    G.add_nodes_from(cityNodes)
    return G
G = addNodes()

nx.draw(G, node_size=2000, with_labels=True)
plt.show()