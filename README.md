


### How does the TravelSales work

The implementation employs a dynamic programming approach with bit masking for efficiency, particularly when the number of cities exceeds eight. For smaller sets, a simpler direct method is used. The algorithm involves:

    Initialization: Setting up mappings of city names to IDs, creating a distance matrix, and initializing the visited state.
    Connectivity Check: Verifying if the roadmap is strongly connected before proceeding with TSP calculations.
    Recursive Exploration: Utilizing depth-first search to explore all possible paths, updating the best path and distance as shorter paths are discovered.
    Path Comparison: Comparing found paths to determine the shortest cycle that covers all cities.

The time complexity of the algorithm when n is greater than is O(n^2 * 2^n), where n is the number of cities. Otherwise the time complexity is O(n!). We chose to implement the algorithm this way in order to not only waste a first approach to the problem, but also because for small number of cities, the time complexity is actually smaller in this case than in the brute force approach.