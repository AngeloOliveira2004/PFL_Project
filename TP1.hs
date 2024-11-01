import qualified Data.List
import qualified Data.Array
import qualified Data.Bits
import Data.Time.Clock (getCurrentTime, diffUTCTime, UTCTime)
import Control.Monad (forM_)
import Data.Time.Clock
-- PFL 2024/2025 Practical assignment 1

-- Uncomment the some/all of the first three lines to import the modules, do not change the code of these lines.

type City = String
type Path = [City]
type Distance = Int

type RoadMap = [(City,City,Distance)]

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

-- Represents the Road Map as a matrix
type Matrix = [[Int]]

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

-- Creates initial matrix of distances with 1000000 if there is no edge or with the edge weight otherwise. The matrix is symmetric
initMatrix :: Int -> Dict -> RoadMap -> Matrix
initMatrix size dict roadMap = 
    [[ if dist == -1 then 1000000 else dist
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
    | isStronglyConnected roadmap = convertPath (fst (solveTSP matrix startMask startCity [0] [] 0 1000000)) citiesDict
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
        if returnToStart /= 1000000             -- If there is a path back to the start city
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


-------------------------------------------------------------------------------------------------------------------------

-- Recursive DFS to collect all paths visiting each city exactly once, returning to the start
tspPaths :: RoadMap -> City -> [City] -> PathDistances -> [PathDistances]
tspPaths roadMap start unvisited visited
    | null unvisited = case lookup start (adjacentCityDistances roadMap (fst (last visited))) of
        Just returnDist -> [visited ++ [(start, returnDist)]]  -- Cycle complete
        Nothing -> []  -- No return path, incomplete cycle
    | otherwise =
        concat [ tspPaths roadMap start (filter (/= adj) unvisited) (visited ++ [(adj, dist)])
               | (adj, dist) <- adjacentCityDistances roadMap (fst (last visited)), adj `elem` unvisited ]

-- Solves TSP by finding the shortest cycle visiting all cities from a starting city
tspShortestPath :: RoadMap -> PathDistances
tspShortestPath roadMap = if null allCycles then [] else minimumBy compareDist allCycles
  where
    allCities = cities roadMap
    start = head allCities
    allCycles = tspPaths roadMap start (filter (/= start) allCities) [(start, 0)]
    compareDist p1 p2 = compare (totalDistPath p1) (totalDistPath p2)
    

-- Helper function to find the minimum by a specific criterion
minimumBy :: (a -> a -> Ordering) -> [a] -> a
minimumBy _ [] = error "minimumBy: empty list"
minimumBy cmp (x:xs) = foldl (\acc y -> if cmp y acc == LT then y else acc) x xs

roadMap :: RoadMap
roadMap = [("A", "B", 10), ("A", "C", 15), ("A", "D", 20), 
           ("B", "C", 35), ("B", "D", 25), ("C", "D", 30)]

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

-- Time wrapper for travelSales
timeTravelSales :: RoadMap -> IO (Path, NominalDiffTime)
timeTravelSales roadmap = do
    start <- getCurrentTime
    let result = travelSales roadmap
    end <- getCurrentTime
    let duration = diffUTCTime end start
    return (result, duration)

-- Timing wrapper for tspShortestPath
timeTspShortestPath :: RoadMap -> IO (PathDistances, NominalDiffTime)
timeTspShortestPath roadmap = do
    start <- getCurrentTime
    let result = tspShortestPath roadmap
    end <- getCurrentTime
    let duration = diffUTCTime end start
    return (result, duration)

-- Function to accumulate total time for multiple test cases
totalTime :: [IO (a, NominalDiffTime)] -> IO NominalDiffTime
totalTime tests = do
    times <- mapM (fmap snd) tests
    return (sum times)

-- Test all functions with all test cases
main :: IO ()
main = do
    putStrLn "Calculating total time for travelSales and tspShortestPath functions on all test cases..."

    let travelSalesTests = [timeTravelSales gTest1, timeTravelSales gTest2, timeTravelSales gTest3, timeTravelSales gTest4]
    totalTravelSalesTime <- totalTime travelSalesTests

    let tspShortestPathTests = [timeTspShortestPath gTest1, timeTspShortestPath gTest2, timeTspShortestPath gTest3, timeTspShortestPath gTest4]
    totalTspShortestPathTime <- totalTime tspShortestPathTests

    putStrLn $ "\nTotal time for travelSales: " ++ show totalTravelSalesTime
    putStrLn $ "Total time for tspShortestPath: " ++ show totalTspShortestPathTime

