ringlattice.py:
-done: ringEdgesStrong
-done" ringEdges2Strong
-done" ringEdges3Strong
-get rid of ringEdges3

GraphQl.Types:
-done: TxtEdgeStrongQDT
-done: TxtEdgesStrongQDT
 -should delete and use [TxtEdgeStrongQDT] once conversion is done
-done: TxtEdgeStrongQArg
 

GraphQlServer:
-created: peopleRingeLatticeEdgeStrongQuery
-done:peopleRingeLatticeEdgeStrongQueryResolver
 -handles 1 or 2 or 3 edges
 
 
PeopleRingLattice.py:
-done: queryPeopleEdgesStrong
-done: queries and load the edges into graph.

queryPeopleEdgesStong = """
   query ($graphTxtQVar:Int!) 
     { peopleRingeLatticeEdgeStrongQuery
        (txtEdgeStrongQArg : $graphTxtQVar)  
        { txtEdgesStrong
            { txtEdgeStrong1
              txtEdgeStrong2
            } 
        } 
     }
   """

