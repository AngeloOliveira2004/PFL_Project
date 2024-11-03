# PFL - Haskell Coursework

### Created by:
Ângelo Oliveira - up202207798@up.pt
José Costa - up202207871@up.pt

### Tasks Performed by each member:
#### Ângelo Oliveira:
- areAdjacent (task 2)
- adjacent (task 4)
- rome (task 6)
- isStronglyConnected (task 7)
- shortestPath (task 8)

#### José Costa
- cities (task 1)
- distance (task 3)
- pathDistance (task 5)
- isStronglyConnected (task 7)
- travelSales (task 9)


#### ShortestPath Explanation:

The shortestPath function implements Dijkstra's algorithm to find the shortest paths from a source city to a target city within a road map represented as a graph.

##### Overview of the Implementation

**Data Structures Used**

- **MinHeap:** A priority queue implemented as a list of tuples (City, Distance). This structure allows for efficient retrieval of the city with the minimum distance, which is crucial for Dijkstra's algorithm. The heap enables efficient access to the next city to process based on the shortest known distance.
- **CityDistances:** A list of tuples storing distances from the starting city to each city. It keeps track of the shortest distance discovered so far for each city during the algorithm's execution.
- **CityParentNodes:** A list of tuples mapping each city to its predecessor cities. This structure is used to reconstruct all possible paths from the start city to the end city after the distances have been determined.

**Algorithm Description**

**Dijkstra's Algorithm:** The core algorithm used here is Dijkstra's algorithm, which efficiently finds the shortest path in a weighted graph with non-negative weights (our case). The algorithm works by maintaining a set of vertices whose shortest distance from the source is known and repeatedly selects the vertex with the smallest known distance to explore its neighbors.

**Initialization:** 

The algorithm begins by assigning a tentative distance value to every vertex. For the initial vertex (the starting city), this value is set to zero, while all other vertices are initialized to infinity, indicating that they are initially unreachable.

A priority queue is used to keep track of vertices to explore, starting with the source vertex.

**Main "Loop":**

**While there are still vertices to explore in the queue:**

- The vertex with the smallest tentative distance is extracted from the queue (this is the current vertex).
- All unvisited neighbors of the current vertex are examined.
- For each neighbor, the algorithm calculates the potential distance from the source vertex to the neighbor via the current vertex.
- If this calculated distance is less than the previously recorded distance for that neighbor, the algorithm updates the neighbor’s distance and records the current vertex as a predecessor.

**Path Reconstruction:**

Once the algorithm has processed all vertices, it can reconstruct the shortest paths from the source to each vertex by backtracking through the recorded predecessor information.

**Functions:**
- **shortestPathDijkstra:** Orchestrates the overall process by initializing distances and invoking the core dijkstra function.
- **initializeDijkstra:** Sets up the initial distances, queue, and predecessor tracking for the starting city.
- **dijkstra: Implements the main logic of the algorithm, recursively exploring neighbors, updating distances, and predecessors.
- **relaxNeighbors and relaxNeighbor:** Handle the process of updating distances for neighboring cities based on the current city being processed.
- **getAllShortestPaths:** Facilitates path reconstruction by backtracking through the predecessor nodes once the algorithm has finished running.

**Key Concepts:**

- **Relaxation:** A critical step where the algorithm checks if a shorter path to a neighboring city can be found through the current city, leading to updates in the distances and predecessor relationships.
- **Backtracking:** After the main loop, the implementation uses recorded predecessors to construct all possible shortest paths from the source city to the target city, which is useful for scenarios where multiple paths of equal length exist.

##### Justification of Data Structures

**MinHeap:** The use of a min-heap is critical for performance in Dijkstra's algorithm. It allows for efficient extraction of the city with the smallest distance and insertion of new distances. Although lists are used for simplicity, in practice, a more sophisticated data structure (like a binary heap) would provide better performance.
Even though we couldn't implement a heap fully in nature, we made it in a way , such that all the operations used for searching have linear time complexity, since we keep our minHeap sorted at all times.

**CityDistances** and **CityParentNodes**: These lists are effective for managing distance and predecessor information. While other structures like dictionaries could be used for faster lookups, the tuple lists provide simplicity in maintaining order and iterating through the cities.