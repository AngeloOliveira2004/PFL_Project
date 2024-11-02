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
-- Type definitions
type Dict = [(Int, City)]
type Matrix = [[Int]]
type Mask = [Int]

-- Constants
inf :: Int
inf = 1000000000000  -- or any large number you choose

-- Maps a list of cities to unique integer IDs, starting from a given ID
initializeDict :: [City] -> Int -> Dict
initializeDict cities startId = zip [startId..] cities

-- Retrieves the city name corresponding to a given integer ID from the dictionary
getCityFromDict :: Dict -> Int -> City
getCityFromDict dict cityId = maybe "" id (lookup cityId dict)

-- Converts a list of city IDs to a list of city names using a dictionary
convertPath :: [Int] -> Dict -> Path
convertPath ids citiesDict = map (getCityFromDict citiesDict) ids

-- Retrieves the distance between two cities from the road map
getDistanceBetweenCities :: RoadMap -> City -> City -> Distance
getDistanceBetweenCities roadMap city1 city2 =
    maybe inf snd (findDistance roadMap city1 city2)

-- Finds the distance between two cities, searching both directions
findDistance :: RoadMap -> City -> City -> Maybe (City, Distance)
findDistance roadMap city1 city2 = case distance roadMap city1 city2 of
    Just dist -> Just (city2, dist)
    Nothing -> case distance roadMap city2 city1 of
        Just dist -> Just (city2, dist)
        Nothing -> Nothing

-- Initializes the distance matrix with appropriate values
initializeMatrix :: Int -> Dict -> RoadMap -> Matrix
initializeMatrix size dict roadMap = 
    [ [ getDistanceBetweenCities roadMap (getCityFromDict dict row) (getCityFromDict dict col)
      | col <- [0..size - 1]
      ]
    | row <- [0..size - 1]
    ]

-- Gets the bit (1 or 0) at a specific index from a mask
getBit :: Int -> Mask -> Int
getBit index mask = mask !! index

-- Sets the bit at a specific index in a mask to 1
setBit :: Int -> Mask -> Mask
setBit index mask = take index mask ++ [1] ++ drop (index + 1) mask

-- Checks if all bits in the mask are set to 1 (final state, everything visited)
isMaskFinal :: Mask -> Bool
isMaskFinal = all (== 1)

-- Solves the Traveling Salesman Problem recursively, updating the best path and distance
solveTSP :: Matrix -> Mask -> Int -> [Int] -> [Int] -> Distance -> Distance -> ([Int], Distance)
solveTSP matrix currMask currCity currPath bestPath currDist bestDist
    | isMaskFinal currMask = updateBestPath currCity currPath currDist matrix bestPath bestDist
    | otherwise = foldl (tryNeighbor matrix currMask currCity currPath currDist) (bestPath, bestDist) unvisitedNeighbors
    where
        returnToStart = head (matrix !! currCity)
        unvisitedNeighbors = filter (\x -> getBit x currMask == 0) [0..length currMask - 1]

-- Update the best path if the current one is better
updateBestPath :: Int -> [Int] -> Distance -> Matrix -> [Int] -> Distance -> ([Int], Distance)
updateBestPath currCity currPath currDist matrix bestPath bestDist
    | returnToStart /= inf && currDist + returnToStart < bestDist =
        (currPath ++ [0], currDist + returnToStart)
    | otherwise = (bestPath, bestDist)
    where
        returnToStart = head (matrix !! currCity)

-- Try visiting a neighbor city and updating the best path and distance
tryNeighbor :: Matrix -> Mask -> Int -> [Int] -> Distance -> ([Int], Distance) -> Int -> ([Int], Distance)
tryNeighbor matrix currMask currCity currPath currDist (bestPath, bestDist) neighbor =
    let nextMask = setBit neighbor currMask
        nextPath = currPath ++ [neighbor]
        nextDist = currDist + matrix !! currCity !! neighbor
    in solveTSP matrix nextMask neighbor nextPath bestPath nextDist bestDist

--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

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
minimumBy cmp (x:xs) = foldl (\acc y -> if cmp y acc == LT then y else acc) x xs

--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

-- Solves the Traveling Salesman Problem using a dynamic programming approach with bit masking.
-- Returns the best path (a list of city indices) and the minimum distance required to visit all cities and return to the start.
travelSales :: RoadMap -> Path
travelSales roadmap
    | isStronglyConnected roadmap = if length cities_ > 8 
                                    then  convertPath (fst (solveTSP matrix startMask startCity [0] [] 0 1000000)) citiesDict 
                                    else map fst (tspShortestPath roadmap)
    | otherwise              = []       -- If graph is not connected, return empty path.
    where
        cities_ = cities roadmap
        size = length cities_
        citiesDict = initializeDict cities_ 0
        matrix = initializeMatrix size citiesDict roadmap
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