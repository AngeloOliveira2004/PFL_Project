import qualified Data.List
import qualified Data.Array
import qualified Data.Bits
-- PFL 2024/2025 Practical assignment 1

-- Uncomment the some/all of the first three lines to import the modules, do not change the code of these lines.

type City = String
type Path = [City]
type Distance = Int

type RoadMap = [(City,City,Distance)]

-- Remove duplicates from a list
removeDup :: Eq a => [a] -> [a]
removeDup [] = [] -- If the list is empty, return an empty list
removeDup (x:xs) = x : removeDup (filter (/= x) xs) -- Add the first element to the list and call removeDup recursively with the rest of the list, filtering out the first element

--4.1 - Return all the cities in the road map
cities :: RoadMap -> [City]
cities roadMap = removeDup [city | (city1 , city2 , _ ) <- roadMap , city <- [city1 , city2]] -- Add all the cities to a list and remove duplicates
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

--4.2 - Check if two cities are adjacent

areAdjacent :: RoadMap -> City -> City -> Bool
areAdjacent roadMap city1 city2 =
    any (\(c1 , c2 , _) -> c1 == city1 && c2 == city2 || c1 == city2 && c2 == city1) roadMap -- Verifies if there is a tuple in the roadMap that has the two cities as the first and second element

--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

--4.3 - Return the distance between two cities

distance :: RoadMap -> City -> City -> Maybe Distance
distance roadMap city1 city2
        | null city1 = Nothing -- If the first city is null , return Nothing
        | null city2 = Nothing -- If the second city is null , return Nothing
        | city1 == city2 = Just 0 -- If the two cities are the same , return 0
        | otherwise = case [d | (c1 , c2 , d) <- roadMap , c1 == city1 && c2 == city2 || c1 == city2 && c2 == city1] of -- Verifies if there is a tuple in the roadMap that has the two cities as the first and second element
            [] -> Nothing -- If there is no tuple that has the two cities as the first and second element , return Nothing
            (d:_) -> Just d -- If there is a tuple that has the two cities as the first and second element , return the distance

--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

 --4.4 - Return the list of cities adjacent to a given city

adjacent :: RoadMap -> City -> [(City,Distance)]
adjacent roadMap city
        | null city = [] -- If the city is null , return an empty list
        | otherwise = [(if c1 == city then c2 else c1, d) | (c1, c2, d) <- roadMap, c1 == city || c2 == city] -- Verifies if there is a tuple in the roadMap that has the city as the first element , if it has , then it adds the second element and the distance to a list , if it doesn't have the city as the first element , then it adds the first element and the distance to a list

--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

--4.5 - Return the distance of a path
pathDistance :: RoadMap -> Path -> Maybe Distance
pathDistance roadMap [] = Just 0 -- No cities in the path , so the distance is 0
pathDistance roadMap [_] = Just 0 -- Only one city in the path , so the distance is 0
pathDistance roadMap (c1:c2:xs) =
    case distance roadMap c1 c2 of
        Nothing -> Nothing -- There is no path between the two cities
        Just distance -> case pathDistance roadMap (c2:xs) of
            Nothing -> Nothing -- There is no path between the two cities
            Just restDistance -> Just (distance + restDistance) -- Exists a path between the two cities , so the distance is the distance between the two cities plus the distance of the rest of the path

--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

--4.6 - Return the names of the cities with the highest number of roads connecting to them

rome :: RoadMap -> [City]
rome roadMap = let
    citiesVar = cities roadMap -- All the cities in the roadMap
    in
        let
            adjacentCitiesWD = [(city , length (adjacent roadMap city)) | city <- citiesVar]
            maxAdjacents = maximum (map snd adjacentCitiesWD) -- Fetches the maximum number of adjacent cities
        in
            [city | (city , adj) <- adjacentCitiesWD , adj == maxAdjacents] -- Fetches the cities that have the maximum number of adjacent cities

--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

--4.7 - Check if the road map is strongly connected

isStronglyConnected :: RoadMap -> Bool
isStronglyConnected roadMap = let
    citiesVar = cities roadMap -- Fetches all the cities in the roadMap
    startCity = head citiesVar -- Fetches the first city in the roadMap
    in
        length (dfs roadMap startCity []) == length citiesVar -- For all the cities in the roadMap , checks if the length of the list of visited cities is equal to the length of the list of all the cities in the roadMap

dfs :: RoadMap -> City -> [City] -> [City]
dfs roadMap city visited
    | city `elem` visited = visited -- If the city is already visited , return the list of visited cities
    | otherwise = -- If the city is not visited
        let
            -- Fetches the adjacent cities of the current city
            adjacentCities = adjacent roadMap city
            -- Removes the cities that are already visited from the list of adjacent cities
            notVisited = [c | (c, _) <- adjacentCities, c `notElem` visited]
        in
            -- Adds the current city to the list of visited cities and calls dfs recursively for all the cities that are not visited
            foldr (dfs roadMap) (city:visited) notVisited

--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

--4.8 - Shortest path between two cities

type DistanceCityTuple = (City, Distance) -- Represents a city and the distance to it
type PathDistances = [DistanceCityTuple] -- Represents a path with with a city root and the distance to it

-- Finds the distance between two adjacent cities in the road map
adjacentCityDistances :: RoadMap -> City -> [DistanceCityTuple]
adjacentCityDistances roadMap city = [(city2, dist) | (city1, city2, dist) <- roadMap, city1 == city] ++ [(city1, dist) | (city1, city2, dist) <- roadMap, city2 == city]

-- Recursive DFS to collect all paths with distances from source to destination
allPathsWithDistances :: RoadMap -> City -> City -> PathDistances -> [PathDistances]
allPathsWithDistances roadMap source destination visited
    | source == destination = [visited ++ [(destination, 0)]]  -- Found a path
    | otherwise =
        let adjacentCities = adjacentCityDistances roadMap source in concat
        [ allPathsWithDistances roadMap adj destination (visited ++ [(source, dist)])
        | (adj, dist) <- adjacentCities, adj `notElem` map fst visited ]

-- Calculate the total distance of a path
totalDistPath :: PathDistances -> Distance
totalDistPath = sum . map snd

-- Find the shortest path(s) between source and destination
shortestPath :: RoadMap -> City -> City -> [Path]
shortestPath roadMap source destination =
    [map fst path | path <- allPaths, totalDistPath path == minDistance]
  where
    allPaths = allPathsWithDistances roadMap source destination []
    minDistance = minimum (map totalDistPath allPaths)


--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

--4.9 - TSP

-- | A type synonym that maps unique integer IDs to city names.
--
-- ### Type Definition:
-- * `CitiesToIDs` is defined as a list of pairs where:
--     - The first element is an `Int`, representing the unique ID of a city.
--     - The second element is a `City` (a `String`), representing the name of the city.
--
-- ### Usage:
-- * Used to convert between integer-based city IDs and their corresponding names.
type CitiesToIDs = [(Int, City)]

-- | A type synonym for representing a 2D distance matrix.
--
-- ### Type Definition:
-- * `Matrix` is a list of lists (2D array) of integers, where `matrix[i][j]` represents the distance from city `i` to city `j`.
--
-- ### Usage:
-- * Primarily used in the Traveling Salesman Problem (TSP) algorithm to store distances between cities.
-- * Each city is represented by a unique integer ID, corresponding to an index in this matrix.
type Matrix = [[Int]]

-- | A type synonym for an array indicating the visitation status of cities.
--
-- ### Type Definition:
-- * `VisitedArray` is a list of integers, where each element can be:
--     - `1` if the corresponding city has been visited.
--     - `0` if the corresponding city has not been visited.
--
-- ### Usage:
-- * Used in the TSP algorithm to keep track of which cities have been visited during the tour.
-- * Index positions correspond to unique city IDs.
type VisitedArray = [Int] 

-- | A type synonym for storing a path and its total distance.
--
-- ### Type Definition:
-- * `PathAndDistance` is a tuple with:
--     - A list of integers (`[Int]`), representing a path where each integer is a city ID.
--     - A `Distance`, representing the total distance traveled along the path.
--
-- ### Usage:
-- * This type is used to store and compare possible paths and their distances in the TSP algorithm.
type PathAndDistance = ([Int], Distance)

-- | A constant representing a large "infinity" value used to denote no connection between cities.
--
-- ### Definition:
-- * `inf` is set to a large integer value, `1000000000000`.
--
-- ### Usage:
-- * Used as a placeholder for "infinite" distance where cities are not directly connected in the matrix.
inf :: Int
inf = 1000000000000  -- or any large number you choose

-- | Maps a list of cities to unique integer IDs, starting from a specified ID.
--
-- ### Arguments:
-- * `cities` - A list of `City` names to map to unique IDs.
-- * `startId` - The starting integer ID for mapping.
--
-- ### Returns:
-- * `CitiesToIDs` - A list of pairs where each city name is mapped to a unique integer ID.
--
-- ### Logic:
-- * Uses `zip` to create a list of pairs associating each city with a unique ID, starting from `startId`.
initializeCitiesToIDs :: [City] -> Int -> CitiesToIDs
initializeCitiesToIDs cities startId = zip [startId..] cities

-- | Retrieves the city name corresponding to a given integer ID from a mapping dictionary.
--
-- ### Arguments:
-- * `citiesToIds` - A `CitiesToIDs` dictionary that maps city IDs to city names.
-- * `cityId` - The integer ID for which the city name is to be retrieved.
--
-- ### Returns:
-- * The `City` name associated with `cityId`, or an empty string if `cityId` is not in the dictionary.
--
-- ### Logic:
-- * Uses `lookup` to search for `cityId` in `citiesToIds`, returning the city name if found.
getCityFromCitiesToIDs :: CitiesToIDs -> Int -> City
getCityFromCitiesToIDs citiesToIds cityId = maybe "" id (lookup cityId citiesToIds)

-- | Converts a list of city IDs to a list of city names using a mapping dictionary.
--
-- ### Arguments:
-- * `ids` - A list of city IDs to convert.
-- * `citiesToIds` - A dictionary mapping city IDs to city names.
--
-- ### Returns:
-- * A `Path` (list of `City` names) corresponding to the given list of city IDs.
--
-- ### Logic:
-- * Uses `map` and `getCityFromCitiesToIDs` to convert each city ID in `ids` to its corresponding city name.
convertPath :: [Int] -> CitiesToIDs -> Path
convertPath ids citiesToIds = map (getCityFromCitiesToIDs citiesToIds) ids

-- | Retrieves the distance between two cities from the road map.
--
-- ### Arguments:
-- * `roadMap` - A `RoadMap` containing tuples of connected cities and their distances.
-- * `city1` - The starting city.
-- * `city2` - The destination city.
--
-- ### Returns:
-- * The `Distance` between `city1` and `city2` if they are connected; otherwise, returns `inf`.
--
-- ### Logic:
-- * Uses `findDistance` to get the distance between `city1` and `city2` (in either direction).
-- * If `findDistance` returns `Nothing`, returns `inf`.
getDistanceBetweenCities :: RoadMap -> City -> City -> Distance
getDistanceBetweenCities roadMap city1 city2 =
    maybe inf snd (findDistance roadMap city1 city2)

-- | Finds the distance between two cities, searching for connections in both directions.
--
-- ### Arguments:
-- * `roadMap` - A `RoadMap` containing tuples of connected cities and their distances.
-- * `city1` - The starting city.
-- * `city2` - The destination city.
--
-- ### Returns:
-- * `Maybe (City, Distance)` - A tuple with `city2` and the `Distance` if a direct connection exists; `Nothing` otherwise.
--
-- ### Logic:
-- * Searches `roadMap` for a direct connection from `city1` to `city2`.
-- * If no direct connection is found, it checks the reverse connection.
findDistance :: RoadMap -> City -> City -> Maybe (City, Distance)
findDistance roadMap city1 city2 = case distance roadMap city1 city2 of
    Just dist -> Just (city2, dist)
    Nothing -> case distance roadMap city2 city1 of
        Just dist -> Just (city2, dist)
        Nothing -> Nothing

-- | Initializes a distance matrix for the Traveling Salesman Problem.
--
-- ### Arguments:
-- * `size` - The number of cities.
-- * `citiesIds` - A dictionary mapping city IDs to city names.
-- * `roadMap` - A `RoadMap` containing distances between cities.
--
-- ### Returns:
-- * A `Matrix` where `matrix[i][j]` represents the distance from city `i` to city `j`.
--
-- ### Logic:
-- * Creates a 2D list using nested list comprehensions.
-- * Each element `matrix[i][j]` is obtained by `getDistanceBetweenCities` for the cities at `i` and `j`.
initializeMatrix :: Int -> CitiesToIDs -> RoadMap -> Matrix
initializeMatrix size citiesIds roadMap = 
    [ [ getDistanceBetweenCities roadMap (getCityFromCitiesToIDs citiesIds row) (getCityFromCitiesToIDs citiesIds col)
      | col <- [0..size - 1]
      ]
    | row <- [0..size - 1]
    ]

-- | Gets the bit (1 or 0) at a specific index in a visitation mask.
--
-- ### Arguments:
-- * `index` - The index to retrieve from the visitation mask.
-- * `visitedArray` - A `VisitedArray` where each bit represents whether a city has been visited.
--
-- ### Returns:
-- * The bit (1 or 0) at `index`, indicating if the city has been visited.
--
-- ### Logic:
-- * Uses list indexing to retrieve the bit at the given index.
getVisited :: Int -> VisitedArray -> Int
getVisited index visitedArray = visitedArray !! index

-- | Sets the bit at a specific index in a visitation mask to 1, marking the city as visited.
--
-- ### Arguments:
-- * `index` - The index to set in the visitation mask.
-- * `visitedArray` - A `VisitedArray` where each bit represents whether a city has been visited.
--
-- ### Returns:
-- * A new `VisitedArray` with the bit at `index` set to 1.
--
-- ### Logic:
-- * Uses `take` and `drop` to set the bit at `index` to 1 while preserving the rest of the list.

setVisited :: Int -> VisitedArray -> VisitedArray
setVisited index visitedArray = take index visitedArray ++ [1] ++ drop (index + 1) visitedArray

-- | Checks if all cities have been visited by confirming all bits in the mask are 1.
--
-- ### Arguments:
-- * `visitedArray` - A `VisitedArray` where each bit represents whether a city has been visited.
--
-- ### Returns:
-- * `True` if all cities are visited (all bits are 1); `False` otherwise.
--
-- ### Logic:
-- * Uses `all` to check if every element in `visitedArray` is equal to 1.
areAllVisited :: VisitedArray -> Bool
areAllVisited = all (== 1)

-- | Updates the best path and distance if the current path is shorter.
--
-- ### Arguments:
-- * `currCity` - The current city (as an integer index) at the end of the current path.
-- * `currPath` - The current path taken so far, represented as a list of city indices.
-- * `currDist` - The total distance traveled so far along the `currPath`.
-- * `matrix` - The `Matrix` (2D list) representing distances between cities.
-- * `bestPath` - The current best path found, represented as a list of city indices.
-- * `bestDist` - The minimum distance found so far for a complete path.
--
-- ### Returns:
-- * A tuple: `(newBestPath, newBestDist)`, where:
--   - `newBestPath` is the updated best path if `currPath` forms a shorter route.
--   - `newBestDist` is the updated shortest distance.
--
-- ### Logic:
-- * Calculates `returnToStart`, the distance to return from `currCity` to the start (assumed at index 0).
-- * If `currDist + returnToStart` is less than `bestDist`, it updates the best path and distance.
-- * Otherwise, it retains the previous best path and distance.
updateBestPath :: Int -> [Int] -> Distance -> Matrix -> [Int] -> Distance -> PathAndDistance
updateBestPath currCity currPath currDist matrix bestPath bestDist
    | returnToStart /= inf && currDist + returnToStart < bestDist =
        (currPath ++ [0], currDist + returnToStart)
    | otherwise = (bestPath, bestDist)
    where
        returnToStart = head (matrix !! currCity)

-- | Attempts to visit a neighboring city and update the best path and distance.
--
-- ### Arguments:
-- * `matrix` - A `Matrix` (2D list) where each element represents the distance between cities.
-- * `currMask` - A `VisitedArray`, which tracks which cities have been visited.
-- * `currCity` - The current city index from which neighbors are being explored.
-- * `currPath` - The current path of visited cities.
-- * `currDist` - The total distance traveled so far along `currPath`.
-- * `(bestPath, bestDist)` - A tuple representing the current best path and distance.
-- * `neighbor` - The index of a neighboring city to explore.
--
-- ### Returns:
-- * A tuple `(updatedPath, updatedDist)`, where:
--   - `updatedPath` is the new best path if exploring `neighbor` improves the result.
--   - `updatedDist` is the shortest distance found for a complete path after considering `neighbor`.
--
-- ### Logic:
-- * Updates the `VisitedArray` for `neighbor` using `setVisited`.
-- * Adds `neighbor` to `currPath` and calculates the new distance `nextDist`.
-- * Calls `findOptimalPath` with updated parameters to explore the neighbor recursively.
exploreNeighbors :: Matrix -> VisitedArray -> Int -> [Int] -> Distance -> PathAndDistance -> Int -> PathAndDistance
exploreNeighbors matrix currMask currCity currPath currDist (bestPath, bestDist) neighbor =
    let nextMask = setVisited neighbor currMask
        nextPath = currPath ++ [neighbor]
        nextDist = currDist + matrix !! currCity !! neighbor
    in findOptimalPath matrix nextMask neighbor nextPath bestPath nextDist bestDist


-- | Recursively solves the Traveling Salesman Problem (TSP) by exploring paths, updating the best path and minimum distance.
--
-- ### Arguments:
-- * `distanceMatrix` - A `Matrix` where each cell `distanceMatrix[i][j]` represents the distance from city `i` to city `j`.
-- * `visitedArray` - A `VisitedArray` indicating which cities have been visited.
-- * `currentLocation` - The index of the current city in the tour.
-- * `currentTour` - A list of city indices representing the path taken so far.
-- * `bestTour` - The current best path found that covers all cities.
-- * `currentCost` - The accumulated cost of the `currentTour`.
-- * `bestCost` - The minimum cost found so far for any complete path.
--
-- ### Returns:
-- * A tuple `(optimalTour, minimalCost)`, where:
--   - `optimalTour` is the path with the shortest distance that covers all cities and returns to the start.
--   - `minimalCost` is the shortest distance of any complete path found.
--
-- ### Logic:
-- * **Base Case**: If all cities are visited (`areAllVisited`), updates the best path with `updateBestPath`.
-- * **Recursive Case**: If there are unvisited cities:
--    * Filters `remainingCities` to find all cities that haven't been visited.
--    * For each remaining city, calls `exploreNeighbors` to attempt visiting it and updating the best path and cost.
findOptimalPath :: Matrix -> VisitedArray -> Int -> [Int] -> [Int] -> Distance -> Distance -> PathAndDistance
findOptimalPath distanceMatrix visitedArray currentLocation currentTour bestTour currentCost bestCost
    | areAllVisited visitedArray = updateBestPath currentLocation currentTour currentCost distanceMatrix bestTour bestCost
    | otherwise = foldl (exploreNeighbors distanceMatrix visitedArray currentLocation currentTour currentCost) (bestTour, bestCost) remainingCities
    where
        distanceToStart = head (distanceMatrix !! currentLocation)
        remainingCities = filter (\city -> getVisited city visitedArray == 0) [0..length visitedArray - 1]

--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

-- | Recursively generates all possible paths that visit each city exactly once, returning to the starting city.
--
-- ### Arguments:
-- * `roadMap` - A `RoadMap` representing connections between cities and distances.
-- * `start` - The initial city where the path begins and ends.
-- * `unvisited` - A list of cities that have not yet been visited.
-- * `visited` - The path taken so far, represented as `PathDistances`, a list of `(City, Distance)` tuples.
--
-- ### Returns:
-- * A list of complete paths, each represented as `PathDistances`, where each path covers all cities exactly once and returns to `start`.
-- 
-- ### Logic:
-- 1. **Base Case**: If there are no unvisited cities left (`null unvisited`):
--    * Check if there's a return path to the starting city.
--      - If so, append `(start, returnDist)` to complete the cycle and return the path.
--      - If not, return an empty list (indicating no complete cycle).
-- 2. **Recursive Case**: If there are still unvisited cities:
--    * For each adjacent city `(adj, dist)` from the last visited city:
--      - Recursively call `tspPaths` with `adj` removed from `unvisited` and added to `visited`.
tspPaths :: RoadMap -> City -> [City] -> PathDistances -> [PathDistances]
tspPaths roadMap start unvisited visited
    | null unvisited = case lookup start (adjacentCityDistances roadMap (fst (last visited))) of
        Just returnDist -> [visited ++ [(start, returnDist)]]  -- Cycle complete
        Nothing -> []  -- No return path, incomplete cycle
    | otherwise =
        concat [ tspPaths roadMap start (filter (/= adj) unvisited) (visited ++ [(adj, dist)])
               | (adj, dist) <- adjacentCityDistances roadMap (fst (last visited)), adj `elem` unvisited ]

-- | Finds the shortest path that forms a cycle covering all cities from a starting city.
--   Uses Depth-First Search to explore paths and then selects the shortest.
--
-- ### Arguments:
-- * `roadMap` - A `RoadMap` representing connections between cities and distances.
--
-- ### Returns:
-- * `PathDistances`, the shortest complete path that covers all cities and returns to the start.
--   If no cycle exists, it returns an empty path.
--
-- ### Logic:
-- 1. **Initialize**:
--    * `allCities` - Retrieves all unique cities from the `roadMap`.
--    * `start` - Chooses the first city as the starting point.
--    * `allCycles` - Uses `tspPaths` to generate all valid cycles from `start`.
-- 2. **Cycle Selection**:
--    * If `allCycles` is empty (indicating no cycle exists), return `[]`.
--    * Otherwise, select the path with the smallest total distance using `minimumBy compareDist`.
--    * `compareDist` is a helper function comparing paths by their total distances (`totalDistPath`).
--
tspShortestPath :: RoadMap -> PathDistances
tspShortestPath roadMap = if null allCycles then [] else minimumBy compareDist allCycles
  where
    allCities = cities roadMap
    start = head allCities
    allCycles = tspPaths roadMap start (filter (/= start) allCities) [(start, 0)]
    compareDist p1 p2 = compare (totalDistPath p1) (totalDistPath p2)

-- | Helper function to find the minimum element of a list based on a specific comparison function.
--
-- ### Arguments:
-- * `cmp` - A comparison function defining the ordering between two elements of type `a`.
-- * `[a]` - The list from which to find the minimum element.
--
-- ### Returns:
-- * The element of type `a` in the list for which `cmp` determines the smallest value.
--
-- ### Logic:
-- * `foldl` iterates through the list, comparing each element `y` with the current minimum `acc`.
-- * If `y` is smaller than `acc` based on `cmp`, `y` becomes the new minimum.
--
minimumBy :: (a -> a -> Ordering) -> [a] -> a
minimumBy cmp (x:xs) = foldl (\acc y -> if cmp y acc == LT then y else acc) x xs

--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

-- | Solves the Traveling Salesman Problem (TSP) using a dynamic programming approach with bit masking.
-- The function attempts to find the shortest possible route that visits every city exactly once and
-- returns to the starting city.
--
-- ### Arguments:
-- * `roadmap` - A `RoadMap`, which is a list of tuples representing connections between cities and distances.
--               Each tuple `(cityA, cityB, distance)` represents a direct road between `cityA` and `cityB`
--               with a given `distance`.
--
-- ### Returns:
-- * If the graph represented by `roadmap` is strongly connected:
--   * If the number of cities is greater than 8, the function uses dynamic programming with bit masking
--     to find the shortest path. This is computationally efficient for larger sets of cities.
--   * If the number of cities is 8 or fewer, a simpler direct approach, `tspShortestPath`, is used to find
--     the optimal path.
-- * If the graph is not strongly connected (i.e., some cities are unreachable), the function returns an
--   empty list, indicating no valid TSP solution.
--
-- ### Details of Implementation:
-- 1. **`cities_`**: Retrieves the list of all unique cities from the roadmap.
-- 2. **`size`**: Stores the total number of cities, calculated as the length of `cities_`.
-- 3. **`citiesToIds`**: Maps each city to a unique integer ID, which simplifies matrix indexing and bit manipulation.
--    - Uses `initializeCitiesToIDs`, a helper function that associates each city in `cities_` with an ID starting from 0.
-- 4. **`matrix`**: Represents distances between cities in a 2D array format, initialized using `initializeMatrix`.
--    - `initializeMatrix` converts the `roadmap` into a matrix where each entry `(i, j)` is the distance
--      between city `i` and city `j` (or infinity if no direct path exists).
-- 5. **`startCity`**: Sets the initial city to the first city (index 0).
-- 6. **`startMask`**: Initializes the visited state for each city. A list with binary values where only the `startCity`
--    is marked as visited (1), and all others are unvisited (0). This bit mask will be used for tracking visited cities.
--
-- The function then checks the connectivity of the roadmap:
--   * If `roadmap` is strongly connected, it proceeds with TSP calculations:
--     - For more than 8 cities: Calls `findOptimalPath` with bit masking to compute the optimal path.
--     - For 8 or fewer cities: Calls a simpler direct approach `tspShortestPath`.
--   * If the graph is disconnected, it returns an empty list as there is no valid path covering all cities.
travelSales :: RoadMap -> Path
travelSales roadmap
    | isStronglyConnected roadmap = if length cities_ > 8 
                                    then  convertPath (fst (findOptimalPath matrix startMask startCity [0] [] 0 1000000)) citiesToIds 
                                    else map fst (tspShortestPath roadmap)
    | otherwise              = []       -- If graph is not connected, return empty path.
    where
        cities_ = cities roadmap
        size = length cities_
        citiesToIds = initializeCitiesToIDs cities_ 0
        matrix = initializeMatrix size citiesToIds roadmap
        startCity = 0
        startMask = [if i == 0 then 1 else 0 | i <- [0..size - 1]]


--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

tspBruteForce :: RoadMap -> Path
tspBruteForce = undefined -- only for groups of 3 people; groups of 2 people: do not edit this function

-- Some graphs to test your work
gTest1 :: RoadMap
gTest1 = [("7","6",1),("8","2",2),("6","5",2),("0","1",4),("2","5",4),("8","6",6),("2","3",7),("7","8",7),("0","7",8),("1","2",8),("3","4",9),("5","4",10),("1","7",11),("3","5",14)]

gTest2 :: RoadMap
gTest2 = [("0","1",10),("0","2",15),("0","3",20),("1","2",35),("1","3",25),("2","3",30)]

gTest3 :: RoadMap -- unconnected graph
gTest3 = [("0","1",4),("2","3",2)]

gTest4 :: RoadMap
gTest4 = [("A", "B", 10), ("A", "C", 15), ("A", "D", 20), ("A", "E", 25), ("A", "F", 30),
          ("B", "C", 35), ("B", "D", 40), ("B", "E", 45), ("B", "F", 50),
          ("C", "D", 55), ("C", "E", 60), ("C", "F", 65),
          ("D", "E", 70), ("D", "F", 75),
          ("E", "F", 80)]

roadMap :: RoadMap
roadMap = [("City1", "City2", 10), ("City1", "City3", 15), ("City1", "City4", 20),
           ("City1", "City5", 10), ("City1", "City6", 25), ("City1", "City7", 30),
           ("City1", "City8", 35), ("City1", "City9", 40), ("City1", "City10", 45),
            ("City1", "City11", 50), ("City1", "City12", 55), ("City1", "City13", 60),
            ("City1", "City14", 65), ("City1", "City15", 70),

            ("City2", "City3", 5), ("City2", "City4", 10), ("City2", "City5", 20),
            ("City2", "City6", 15), ("City2", "City7", 25), ("City2", "City8", 30),
            ("City2", "City9", 35), ("City2", "City10", 40), ("City2", "City11", 45),
            ("City2", "City12", 50), ("City2", "City13", 55), ("City2", "City14", 60),
            ("City2", "City15", 65),

            ("City3", "City4", 10), ("City3", "City5", 15), ("City3", "City6", 20),
            ("City3", "City7", 10), ("City3", "City8", 25), ("City3", "City9", 30),
            ("City3", "City10", 35), ("City3", "City11", 40), ("City3", "City12", 45),
            ("City3", "City13", 50), ("City3", "City14", 55), ("City3", "City15", 60),

            ("City4", "City5", 5), ("City4", "City6", 15), ("City4", "City7", 10),
            ("City4", "City8", 20), ("City4", "City9", 25), ("City4", "City10", 30),
            ("City4", "City11", 35), ("City4", "City12", 40), ("City4", "City13", 45),
            ("City4", "City14", 50), ("City4", "City15", 55),

            ("City5", "City6", 10), ("City5", "City7", 20), ("City5", "City8", 15),
            ("City5", "City9", 25), ("City5", "City10", 20), ("City5", "City11", 30),
            ("City5", "City12", 35), ("City5", "City13", 40), ("City5", "City14", 45),
            ("City5", "City15", 50),

            ("City6", "City7", 5), ("City6", "City8", 10), ("City6", "City9", 20),
            ("City6", "City10", 25), ("City6", "City11", 30), ("City6", "City12", 25),
            ("City6", "City13", 35), ("City6", "City14", 40), ("City6", "City15", 45),

            ("City7", "City8", 15), ("City7", "City9", 10), ("City7", "City10", 20),
            ("City7", "City11", 25), ("City7", "City12", 30), ("City7", "City13", 35),
            ("City7", "City14", 30), ("City7", "City15", 40),

            ("City8", "City9", 5), ("City8", "City10", 10), ("City8", "City11", 15),
            ("City8", "City12", 20), ("City8", "City13", 25), ("City8", "City14", 30),
            ("City8", "City15", 35),

            ("City9", "City10", 10), ("City9", "City11", 15), ("City9", "City12", 20),
            ("City9", "City13", 25), ("City9", "City14", 20), ("City9", "City15", 30),

            ("City10", "City11", 5), ("City10", "City12", 10), ("City10", "City13", 15),
            ("City10", "City14", 20), ("City10", "City15", 25),

            ("City11", "City12", 15), ("City11", "City13", 10), ("City11", "City14", 20),
            ("City11", "City15", 30),

            ("City12", "City13", 5), ("City12", "City14", 10), ("City12", "City15", 15),

            ("City13", "City14", 20), ("City13", "City15", 25),

            ("City14", "City15", 10)]

roadMap' :: RoadMap
roadMap' = [
    ("City1", "City2", 29), ("City1", "City3", 20), ("City1", "City4", 21),
    ("City1", "City5", 16), ("City1", "City6", 31), ("City1", "City7", 100),
    ("City1", "City8", 12), ("City1", "City9", 4), ("City1", "City10", 31),

    ("City2", "City3", 15), ("City2", "City4", 29), ("City2", "City5", 28),
    ("City2", "City6", 40), ("City2", "City7", 72), ("City2", "City8", 21),
    ("City2", "City9", 29), ("City2", "City10", 41),

    ("City3", "City4", 17), ("City3", "City5", 27), ("City3", "City6", 42),
    ("City3", "City7", 58), ("City3", "City8", 25), ("City3", "City9", 23),
    ("City3", "City10", 37),

    ("City4", "City5", 30), ("City4", "City6", 55), ("City4", "City7", 65),
    ("City4", "City8", 19), ("City4", "City9", 34), ("City4", "City10", 26),

    ("City5", "City6", 40), ("City5", "City7", 60), ("City5", "City8", 22),
    ("City5", "City9", 31), ("City5", "City10", 25),

    ("City6", "City7", 10), ("City6", "City8", 20), ("City6", "City9", 30),
    ("City6", "City10", 15),

    ("City7", "City8", 80), ("City7", "City9", 70), ("City7", "City10", 50),

    ("City8", "City9", 30), ("City8", "City10", 25),

    ("City9", "City10", 35)]


main :: IO ()
main = do
    return()

--call travelSales with BF when n! < 2^n * n^2
--n = 7