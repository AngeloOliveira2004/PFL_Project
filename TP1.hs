{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Replace case with fromMaybe" #-}
import qualified Data.List
import qualified Data.Array
import qualified Data.Bits
-- PFL 2024/2025 Practical assignment 1

-- Uncomment the some/all of the first three lines to import the modules, do not change the code of these lines.

type City = String
type Path = [City]
type Distance = Int

type RoadMap = [(City,City,Distance)]

-- Global variable to represent infinite distance
infinite :: Int
infinite = 1000000

-- Remove duplicates from a list
nub :: Eq a => [a] -> [a]
nub [] = [] -- If the list is empty, return an empty list
nub (x:xs) = x : nub (filter (/= x) xs) -- Add the first element to the list and call nub recursively with the rest of the list, filtering out the first element

--4.1 - Return all the cities in the road map
cities :: RoadMap -> [City]
cities roadMap = nub [city | (city1 , city2 , _ ) <- roadMap , city <- [city1 , city2]] -- Add all the cities to a list and remove duplicates
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

-- Stores a pair (id, city) to map a id to the corresponding city
type Dict = [(Int, City)]

-- Maps a list of cities to unique integer IDs, starting from a given ID
mapCities :: [City] -> Int -> Dict
mapCities cities startId = zip [startId..] cities

-- Retrieves the city name corresponding to a given integer ID from the dictionary
getCityFromDict :: Dict -> Int -> City
getCityFromDict dict id = case lookup id dict of
    Just city -> city
    Nothing -> ""

-- Converts a list of city IDs to a list of city names using a dictionary
convertPath :: [Int] -> Dict -> Path
convertPath [] _ = []
convertPath (cityID:rest) citiesDict = getCityFromDict citiesDict cityID : convertPath rest citiesDict

-- Represents the Road Map as a matrix
type Matrix = [[Int]]

-- Creates initial matrix of distances with infinite if there is no edge or with the edge weight otherwise. The matrix is symmetric
initMatrix :: Int -> Dict -> RoadMap -> Matrix
initMatrix size dict roadMap = 
    [[ if dist == -1 then infinite else dist
        | col <- [0..size - 1], let dist = snd (areAdjacentAux roadMap (getCityFromDict dict line) (getCityFromDict dict col))
    ] | line <- [0..size - 1]]

areAdjacentAux :: RoadMap -> City -> City -> (Bool, Int)
areAdjacentAux rm city1 city2 = case lookup (city1, city2) distances of
    Just d  -> (True, d)
    Nothing -> (False, -1)
    where
    distances = [((c1, c2), d) | (c1, c2, d) <- rm] ++ [((c2, c1), d) | (c1, c2, d) <- rm]
-- To represent mask, [0,1,0,1] is equivelent to 0b0101
type Mask = [Int]

-- Gets the bit (1 or 0) at a specific index from a mask (list of integers)
getBit :: Int -> Mask -> Int
getBit index mask = mask !! index

-- Sets the bit at a specific index in a mask to 1 (It means city at index is marked visited)
setBit :: Int -> Mask -> Mask
setBit index mask = take index mask ++ [1] ++ drop (index + 1) mask

-- Checks if all bits in the mask are set to 1 (final state, everything visited)
isMaskFinal :: Mask -> Bool
isMaskFinal = all (== 1)

-- Solves the Traveling Salesman Problem using a dynamic programming approach with bit masking.
-- Returns the best path (a list of city indices) and the minimum distance required to visit all cities and return to the start.
travelSales :: RoadMap -> Path
travelSales roadmap
    | isStronglyConnected roadmap = convertPath (fst (solveTSP matrix startMask startCity [0] [] 0 infinite)) citiesDict
    | otherwise              = []       -- If graph is not connected, return empty path.
    where
        cities_ = cities roadmap
        size = length cities_
        citiesDict = mapCities cities_ 0
        matrix = initMatrix size citiesDict roadmap
        startCity = 0
        startMask = [ if i == 0 then 1 else 0 | i <- [0..size - 1]]

-- Recursively solves the Traveling Salesman Problem, updating the best path and distance.
-- Takes the distance matrix, current bitmask (visited cities), current city, current path, best path found so far,
-- current distance, and best distance, and returns the optimal path and minimum distance.
solveTSP :: Matrix -> Mask -> Int -> [Int] -> [Int] -> Distance -> Distance -> ([Int], Distance)
solveTSP matrix currMask currCity currPath bestPath currDist bestDist
    | isMaskFinal currMask = 
        if returnToStart /= infinite             -- If there is a path back to the start city
        then if currDist + returnToStart < bestDist -- If the current path is shorter than the best path found so far
             then (currPath ++ [0], currDist + returnToStart) -- Update the best path and distance
             else (bestPath, bestDist) -- Otherwise, keep the best path and distance
        else (bestPath, bestDist) -- If there is no path back to the start city, keep the best path and distance
    | otherwise = foldl tryNeighbor (bestPath, bestDist) unvisitedNeighbors -- Try all unvisited neighbors and update the best path and distance
    where
        returnToStart = matrix !! currCity !! 0 -- Distance from current city to start city
        unvisitedNeighbors = [x | x <- [0..length currMask - 1], getBit x currMask == 0] -- Unvisited neighbors
        tryNeighbor (accPath, accDist) neighbor =  -- Try a neighbor and update the best path and distance
            solveTSP matrix (setBit neighbor currMask) neighbor -- Recursively call solveTSP with the neighbor as the current city
                    (currPath ++ [neighbor]) accPath -- Update the current path and best path
                    (currDist + matrix !! currCity !! neighbor) accDist -- Update the current distance and best distance


--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

-- 4.8 Dijkstra's algorithm

type MinHeap = [(City,Distance)]

type CityDistances = [(City,Distance)]

type CityParentNodes = [(City,[City])]

-- Finds all the shortest path from a source city to a target city using Dijkstra's algorithm.
-- Entry point for finding all shortest paths from startCity to endCity
shortestPathDijkstra :: RoadMap -> City -> City -> [Path]
shortestPathDijkstra roadMap startCity endCity = getAllShortestPaths cityParentNodes endCity
    where
        (initialDistances, initialHeap, initialPredecessors) = initializeDijkstra roadMap startCity
        (_, _, cityParentNodes) = dijkstra roadMap initialDistances initialHeap initialPredecessors

-- Sets up initial distances, heap, and predecessors for Dijkstra's algorithm
initializeDijkstra :: RoadMap -> City -> (CityDistances, MinHeap, CityParentNodes)
initializeDijkstra rm startCity = 
    let initialDistances = map (\city -> (city, if city == startCity then 0 else infinite)) (cities rm)
        initialHeap = [(startCity, 0)]
        initialPredecessors = [(startCity, [])]
    in (initialDistances, initialHeap, initialPredecessors)

-- Wrapper function to get all shortest paths by backtracking from endCity to startCity
getAllShortestPaths :: CityParentNodes -> City -> [Path]
getAllShortestPaths cityParentNodes endCity = findPathsFromCityParentNodes cityParentNodes endCity [endCity]


-- Recursively finds all possible paths from the target city to the source using the city predecessors.
findPathsFromCityParentNodes :: CityParentNodes -> City -> Path -> [Path]
findPathsFromCityParentNodes cityParentNodes targetCity currPath =
    case getPredecessors cityParentNodes targetCity of
        [] -> [currPath]
        preds -> concatMap (\pred -> findPathsFromCityParentNodes cityParentNodes pred (pred : currPath)) preds -- Recursively call findPathsFromCityParentNodes with the predecessor as the target city and the predecessor appended to the current path

-- The Dijkstra algorithm implementation to find the shortest path in a roadmap
dijkstra :: RoadMap -> CityDistances -> MinHeap -> CityParentNodes -> (CityDistances, MinHeap, CityParentNodes)
dijkstra rm cityDistances heap cityParentNodes
    | null heap = (cityDistances, heap, cityParentNodes)  -- If heap is empty, we’re done
    | otherwise = 
        let currentCity = extractMinCity heap
            updatedHeap = deleteCityFromHeap heap currentCity
            unvisitedNeighbors = filterUnvisitedNeighbors rm updatedHeap cityParentNodes currentCity
            (updatedDistances, newHeap, updatedPredecessors) = relaxNeighbors currentCity unvisitedNeighbors cityDistances updatedHeap cityParentNodes
        in dijkstra rm updatedDistances newHeap updatedPredecessors -- Recursively call dijkstra with the updated distances, heap, and predecessors


filterUnvisitedNeighbors :: RoadMap -> MinHeap -> CityParentNodes -> City -> [(City, Distance)]
filterUnvisitedNeighbors rm heap cityParentNodes currentCity =
    filter (\(city, _) -> not (cityHasPredecessors cityParentNodes city && getCityDistanceFromHeap heap city == infinite)) (adjacent rm currentCity)

relaxNeighbors :: City -> [(City, Distance)] -> CityDistances -> MinHeap -> CityParentNodes -> (CityDistances, MinHeap, CityParentNodes)
relaxNeighbors currentCity neighbors cityDistances heap cityParentNodes =
    foldl (\(distancesAcc, heapAcc, predecessorsAcc) (neighbor, edgeW) ->
               relaxNeighbor currentCity neighbor edgeW distancesAcc heapAcc predecessorsAcc
           ) (cityDistances, heap, cityParentNodes) neighbors

relaxNeighbor :: City -> City -> Distance -> CityDistances -> MinHeap -> CityParentNodes -> (CityDistances, MinHeap, CityParentNodes)
relaxNeighbor currentCity neighbor edgeWeight distances heap parents =
    let currentDist = getCityDistanceFromHeap heap neighbor
        newDist = getDistFromRelaxedCity distances currentCity + edgeWeight
    in case () of
        _ | currentDist == infinite ->  -- Case 2: Distance is infinity, so must be relaxed
              (updateCityDistance distances neighbor newDist, insertCityIntoHeap heap (neighbor, newDist), replaceCityParentNodes parents currentCity neighbor)
          | newDist < currentDist ->   -- Case 3: Found a shorter path
              (updateCityDistance distances neighbor newDist, updateCityDistanceInHeap heap neighbor newDist, replaceCityParentNodes parents currentCity neighbor)
          | newDist == currentDist ->  -- Case 4: Found an alternative path
              (distances, heap, appendCityPredecessor parents currentCity neighbor)
          | otherwise ->               -- Case 1 or default: No update needed
              (distances, heap, parents)

-- Checks if a city has any recorded predecessors.
cityHasPredecessors :: CityParentNodes -> City -> Bool
cityHasPredecessors cityParents city =
    any ((== city) . fst) cityParents

-- Replaces the predecessors of a city with a new predecessor.
replaceCityParentNodes :: CityParentNodes -> City -> City -> CityParentNodes
replaceCityParentNodes cityParents citySource cityDest = updateCityPredecessors cityParents cityDest [citySource]

-- Appends a new predecessor to the existing list for a city.
appendCityPredecessor :: CityParentNodes -> City -> City -> CityParentNodes
appendCityPredecessor cityParents citySource cityDest =
    let existingPreds = getPredecessors cityParents cityDest
    in updateCityPredecessors cityParents cityDest (citySource : existingPreds)

-- Retrieves the predecessors of a city.
getPredecessors :: CityParentNodes -> City -> [City]
getPredecessors cityParents city = 
    case lookup city cityParents of
        Just preds -> preds
        Nothing -> []  -- If city is not found, return an empty list.

-- Helper function to update the predecessors list for a specific city.
updateCityPredecessors :: CityParentNodes -> City -> [City] -> CityParentNodes
updateCityPredecessors [] city newPreds = [(city, newPreds)]
updateCityPredecessors ((c, preds):rest) city newPreds
    | c == city = (city, newPreds) : rest  -- Update the list for the matching city
    | otherwise = (c, preds) : updateCityPredecessors rest city newPreds  -- Recur on the rest

-- Finds the city with the smallest distance from the minHeap.
extractMinCity :: MinHeap -> City
extractMinCity minHeap = fst (foldl (\acc@(_, accDist) pair@(_, dist) -> if dist < accDist then pair else acc) ("",100000) minHeap)

-- Retrieves the distance of a city from the minHeap.
getCityDistanceFromHeap :: MinHeap -> City -> Distance
getCityDistanceFromHeap [] _ = infinite  -- If city is not found, return a large "infinity" value.
getCityDistanceFromHeap ((city, dist):xs) targetCity
    | city == targetCity = dist
    | otherwise = getCityDistanceFromHeap xs targetCity

-- Removes a city from the minHeap.
deleteCityFromHeap :: MinHeap -> City -> MinHeap
deleteCityFromHeap minHeap city =
    filter (\(c, _) -> c /= city) minHeap

-- Insert a city into the sorted minHeap
insertCityIntoHeap :: MinHeap -> (City, Distance) -> MinHeap
insertCityIntoHeap [] elem = [elem]
insertCityIntoHeap heap@(x@(c, d):xs) elem@(c', d')
    | d' < d    = elem : heap
    | otherwise = x : insertCityIntoHeap xs elem

-- Updates the distance of a city in the minHeap.
updateCityDistanceInHeap :: MinHeap -> City -> Distance -> MinHeap
updateCityDistanceInHeap heap city newDist =
    [(c, if c == city then newDist else d) | (c, d) <- heap]


-- Updates the distance of a city in the relaxed (visited) cities list.
updateCityDistance :: CityDistances -> City -> Distance -> CityDistances
updateCityDistance distances city newDist =
    [(c, if c == city then newDist else d) | (c, d) <- distances]


-- Retrieves the distance of a city from the relaxed cities list.
getDistFromRelaxedCity :: CityDistances -> City -> Distance
getDistFromRelaxedCity distances city = 
    maybe infinite snd (Data.List.find (\(c, _) -> c == city) distances)


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

main :: IO ()
main = do
    {-
  putStrLn "Testing gTest1:"
  print (cities gTest1)

  putStrLn "Testing gTest2:"
  print (cities gTest2)

  putStrLn "Testing gTest3:"
  print (cities gTest3)

  putStrLn "Testing areAdjacent:"
  print (areAdjacent gTest1 "7" "6")
  print (areAdjacent gTest1 "3" "8")

  putStrLn "Testing distance:"
  print (distance gTest1 "7" "6")
  print (distance gTest1 "3" "8")

  putStrLn "Testing adjacent:"
  print (adjacent gTest1 "7")
  print (adjacent gTest1 "3")

  putStrLn "Testing pathDistance:"
  print (pathDistance gTest1 ["7","6","5","4"])
  print (pathDistance gTest1 ["7","6","5","4","3","2","1","0"])

  putStrLn "Testing rome:"
  print (rome gTest1)
  print (rome gTest3)

  putStrLn "Testing isStronglyConnected:"
  print (isStronglyConnected gTest1)
  print (isStronglyConnected gTest3)
    

  putStrLn "Testing shortestPath:"
  print (shortestPath gTest1 "0" "10000")
    
  putStrLn "Testing gTest1:"
  print (cities gTest1)
-}
    -- print (isStronglyConnected gTest1)
    -- print (isStronglyConnected gTest2)
    -- print (isStronglyConnected gTest3)


    -- print (travelSales gTest2)

  --print (shortestPath gTest2 "0" "3")

{-  
  putStrLn "Testing gTest1:"
  print (travelSales gTest1)

  putStrLn "Testing gTest2:"
  print (travelSales gTest2)
-}
  putStrLn "Testing gTest1:"
  print (shortestPath gTest1 "0" "4")

  putStrLn "Testing gTest1:"
  print (shortestPathDijkstra gTest1 "0" "4")
