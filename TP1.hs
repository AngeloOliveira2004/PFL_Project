import qualified Data.List
import qualified Data.Array
import qualified Data.Bits
-- PFL 2024/2025 Practical assignment 1

-- Uncomment the some/all of the first three lines to import the modules, do not change the code of these lines.

type City = String
type Path = [City]
type Distance = Int

type RoadMap = [(City,City,Distance)]

--4.1 - Return all the cities in the road map

removeDup:: (Eq a) => [a] -> [a]
removeDup= removeDupAux (==)

removeDupAux :: (a -> a -> Bool) -> [a] -> [a]
removeDupAux eq [] = []
removeDupAux eq (x:xs) = x : removeDupAux eq (filter (not . eq x) xs)

cities :: RoadMap -> [City]
cities roadMap = removeDup [city | (city1 , city2 , _ ) <- roadMap , city <- [city1 , city2]]
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

--4.2 - Check if two cities are adjacent

areAdjacent :: RoadMap -> City -> City -> Bool
areAdjacent roadMap city1 city2 =
    any (\(c1 , c2 , _) -> (c1 == city1 && c2 == city2) || (c1 == city2 && c2 == city1)) roadMap

--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

--4.3 - Return the distance between two cities

distance :: RoadMap -> City -> City -> Maybe Distance
distance roadMap city1 city2
        | null city1 = Nothing
        | null city2 = Nothing
        | city1 == city2 = Just 0
        | otherwise = case [d | (c1 , c2 , d) <- roadMap , (c1 == city1 && c2 == city2) || (c1 == city2 && c2 == city1)] of
            [] -> Nothing
            (d:_) -> Just d

--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

 --4.4 - Return the list of cities adjacent to a given city

adjacent :: RoadMap -> City -> [(City,Distance)]
adjacent roadMap city 
        | null city = []
        | otherwise = [(if c1 == city then c2 else c1, d) | (c1, c2, d) <- roadMap, c1 == city || c2 == city]

--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

--4.5 - Return the distance of a path
pathDistance :: RoadMap -> Path -> Maybe Distance
pathDistance roadMap [] = Just 0 -- Sem cidades no caminho, logo a distância é 0
pathDistance roadMap [_] = Just 0 -- Só uma cidade no caminho, logo a distância é 0
pathDistance roadMap (c1:c2:xs) =
    case distance roadMap c1 c2 of
        Nothing -> Nothing -- Não existe caminho entre as duas cidades
        Just distance -> case pathDistance roadMap (c2:xs) of
            Nothing -> Nothing -- Não existe caminho entre as cidades seguintes
            Just restDistance -> Just (distance + restDistance) -- Existe caminho entre as cidades e as seguintes , então temos de somar o valor de c1 a c2 , com o valor de c2 até ao fim do caminho que conseguimos através da recursão

--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

--4.6 - Return the names of the cities with the highest number of roads connecting to them

rome :: RoadMap -> [City]
rome roadMap = let
    citiesVar = cities roadMap -- Todas as cidades existententes no roadMap , são eliminados duplicados através da função removeDup
    in
        let
            adjacentCitiesWD = [(city , length (adjacent roadMap city)) | city <- citiesVar]
            maxAdjacents = maximum (map snd adjacentCitiesWD) -- Vai buscar o numero máximo de cidades adjacentes que uma cidade tem, que se encontra no segundo parametro de cada tuple
        in
            [city | (city , adj) <- adjacentCitiesWD , adj == maxAdjacents] -- Vai buscar todos as cidades que têm o número máximo de cidades adjacentes
            
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

--4.7 - Check if the road map is strongly connected

isStronglyConnected :: RoadMap -> Bool
isStronglyConnected roadMap = let
    citiesVar = cities roadMap -- Todas as cidades existententes no roadMap , são eliminados duplicados através da função removeDup
    startCity = head citiesVar -- Vai buscar a primeira cidade
    in
        length (dfs roadMap startCity []) == length citiesVar -- Para todas as cidades , verifica se existe um caminho entre elas , se existir para todas as cidades então o grafo é fortemente conectado

dfs :: RoadMap -> City -> [City] -> [City]
dfs roadMap city visited
    | city `elem` visited = visited -- Se já estiver visitado , então não é necessário visitar novamente, logo retorna-se a lista de cidades visitadas
    | otherwise = -- Se não estiver visitado
        let
            -- Vai buscar todas as cidades adjacentes
            adjacentCities = adjacent roadMap city
            -- Elimina cidades que ainda não foram visitadas
            notVisited = [c | (c, _) <- adjacentCities, c `notElem` visited]
        in
            -- Adiciona a cidade atual à lista de cidades visitadas e chama a função recursivamente para as cidades adjacentes
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
findAllPathsWithDistances roadMap source destination = allPathsWithDistances roadMap source destination []

totalDistance :: PathWithDistances -> Distance
totalDistance path = sum [dist | (_, dist) <- path]

-- Shortest path function
shortestPath :: RoadMap -> City -> City -> [Path]
shortestPath roadMap source destination
    | null paths = []  -- No path found
    | otherwise = [map fst path | path <- paths, totalDistance path == minimumDist]  -- Return all paths with the minimum distance
    where
        paths = findAllPathsWithDistances roadMap source destination
        minimumDist = minimum [totalDistance path | path <- paths]      

--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

allPathsWithDistances' :: RoadMap -> City -> City -> Int -> PathWithDistances -> [PathWithDistances]
allPathsWithDistances' roadMap source destination totalCities visited
    -- If source equals destination and all cities are visited, a Hamiltonian path is found
    | source == destination && length visited + 1 == totalCities = [visited ++ [(destination, 0)]]
    -- Otherwise, explore unvisited adjacent cities
    | otherwise = concat 
        [ allPathsWithDistances' roadMap adj destination totalCities (visited ++ [(source, dist)]) 
        | (adj, dist) <- adjacentWithDistances roadMap source, adj `notElem` map fst visited]

findHamiltonianPaths :: RoadMap -> City -> City -> [PathWithDistances]
findHamiltonianPaths roadMap source destination = allPathsWithDistances' roadMap source destination numCities []
    where
    -- Calculate the total number of unique cities
    numCities = length $ Data.List.nub $ concat [[c1, c2] | (c1, c2, _) <- roadMap]

travelSales :: RoadMap -> Path
travelSales roadMap 
    | null paths = []  -- No path found
    | otherwise = head [map fst path | path <- paths, totalDistance path == minimumDist]
    where
        paths = findHamiltonianPaths roadMap startCity startCity
        startCity = head $ cities roadMap
        minimumDist = minimum [totalDistance path | path <- paths]

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
  print (shortestPath gTest1 "0" "4")
  --print (shortestPath gTest2 "0" "3")

  putStrLn "Testing travelSales:"
  print (travelSales gTest1)
  print (travelSales gTest2)
