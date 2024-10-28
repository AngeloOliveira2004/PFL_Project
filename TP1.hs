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

type DistanceToCity = (City,Distance)
type PathWithDistances = [DistanceToCity]

-- Find adjacent cities with their distances
adjacentWithDistances :: RoadMap -> City -> [(City, Distance)]
adjacentWithDistances roadMap city = [(if c1 == city then c2 else c1, dist) | (c1, c2, dist) <- roadMap, c1 == city || c2 == city]

-- Modified DFS function to find all paths with distances from source to destination
allPathsWithDistances :: RoadMap -> City -> City -> PathWithDistances -> [PathWithDistances]
allPathsWithDistances roadMap source destination visited
    -- If source equals destination, a path is found
    |  source == destination = [visited ++ [(destination, 0)]]  -- Append destination with 0 as the last distance
    -- Otherwise, explore unvisited adjacent cities
    | otherwise = concat
        [ allPathsWithDistances roadMap adj destination (visited ++ [(source, dist)])
        | (adj, dist) <- adjacentWithDistances roadMap source, adj `notElem` map fst visited]

-- Helper function to find all paths with distances from source to destination (user-facing function)
findAllPathsWithDistances :: RoadMap -> City -> City -> [PathWithDistances]
findAllPathsWithDistances roadMap source destination = allPathsWithDistances roadMap source destination [] -- Call the modified DFS function with an empty list as the visited list

totalDistance :: PathWithDistances -> Distance
totalDistance path = sum [dist | (_, dist) <- path]

-- Shortest path function
shortestPath :: RoadMap -> City -> City -> [Path]
shortestPath roadMap source destination
    | null paths = []  -- No path found
    | otherwise = [map fst path | path <- paths, totalDistance path == minimumDist]  -- Return all paths with the minimum distance
    where
        paths = findAllPathsWithDistances roadMap source destination -- Find all paths with distances
        minimumDist = minimum [totalDistance path | path <- paths] -- Find the minimum distance

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

    print (isStronglyConnected gTest1)
    print (isStronglyConnected gTest2)
    print (isStronglyConnected gTest3)


    print (travelSales gTest2)
-}
  --print (shortestPath gTest2 "0" "3")
  putStrLn "Testing gTest1:"
  print (travelSales gTest1)

  putStrLn "Testing gTest2:"
  print (travelSales gTest2)

