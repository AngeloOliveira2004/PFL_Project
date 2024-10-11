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

--4.8 - Return the shortest path between two cities
{-
vector<int> shortestPath(vector<vector<int>>& edges, int N,int M, int src){
        vector<int>adj[N];
        for(auto it:edges){
            int u=it[0];
            int v=it[1];
            adj[u].push_back(v);
            adj[v].push_back(u);
        }
        vector<int>dist(N,-1);
        dist[src]=0;
        queue<pair<int,int>>q;
        q.push({src,0});
        while(!q.empty()){
            int node=q.front().first;
            int d=q.front().second;
            q.pop();
            for(auto it:adj[node]){
                if(dist[it]==-1){
                    dist[it]=d+1;
                    q.push({it,d+1});
                }
            }
        }
        return dist;
    }s
-}

--We can use the same logic as the code above, but instead of using a list and always pick the head
type DistanceToCity = (City,Distance)

initialize :: [City] -> City -> [(City, Distance)]
initialize cities source = [(city, if city == source then 0 else -1) | city <- cities]

-- Update the distance in the distance list
distancesUpdate :: [DistanceToCity] -> City -> Int -> [DistanceToCity]
distancesUpdate [] _ _ = []
distancesUpdate ((city, distance):xs) cityToUpdate newDistance
    | city == cityToUpdate = (city, newDistance) : distancesUpdate xs cityToUpdate newDistance
    | otherwise = (city, distance) : distancesUpdate xs cityToUpdate newDistance

-- Get adjacent cities with their distances
adjacentCitiesDistance :: RoadMap -> City -> [DistanceToCity]
adjacentCitiesDistance roadMap city = [(if c1 == city then c2 else c1, dist) | (c1, c2, dist) <- roadMap, c1 == city || c2 == city]

-- Get the city with the minimum distance that hasn't been visited
minDistanceCity :: [DistanceToCity] -> [City] -> Maybe City
minDistanceCity distances visited = 
    let unvisitedDistances = filter (\(city, dist) -> dist /= -1 && city `notElem` visited) distances
    in if null unvisitedDistances
       then Nothing
       else Just (fst (Data.List.minimumBy (\(_, d1) (_, d2) -> compare d1 d2) unvisitedDistances))

       
fromMaybe :: a -> Maybe a -> a
fromMaybe x Nothing = x
fromMaybe _ (Just x) = x

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing _ = False
       
-- Dijkstra's algorithm using only lists
-- Dijkstra's algorithm using only lists
dijkstra :: RoadMap -> [City] -> City -> [(City, Distance)] -> [(City, Maybe City)] -> [(City, Distance)]
dijkstra roadMap cities source distances predecessors = dijkstra' distances [] predecessors
  where
    dijkstra' dist visited preds
      | length visited == length cities = dist
      | otherwise =
          let currentCity = fromMaybe "" (minDistanceCity dist visited)
              currentDist = fromMaybe (-1) (lookup currentCity dist)
              adjCities = adjacentCitiesDistance roadMap currentCity
              updatedDist = foldl (updateDistances currentCity currentDist) dist adjCities
              newVisited = currentCity : visited
          in dijkstra' updatedDist newVisited (updatePredecessors currentCity currentDist preds adjCities)

    updateDistances currCity currDist distances (adjCity, adjDist) =
      let totalDist = currDist + adjDist
          oldDist = fromMaybe maxBound (lookup adjCity distances)
      in if oldDist == -1 || totalDist < oldDist
         then distancesUpdate distances adjCity totalDist
         else distances

    -- Update the predecessor list to reflect (City, Maybe City)
    updatePredecessors :: City -> Int -> [(City, Maybe City)] -> [DistanceToCity] -> [(City, Maybe City)]
    updatePredecessors currCity currDist preds adjCities =
      [if adjCity == targetCity && (isNothing oldPred || currDist + adjDist < fromMaybe maxBound (lookup targetCity distances))
       then (targetCity, Just currCity)  -- Use City as the predecessor
       else (targetCity, oldPred)
       | (targetCity, oldPred) <- preds,
         (adjCity, adjDist) <- adjCities]

-- Reconstruct the shortest path from the source to the destination
reconstructPath :: [(City, Maybe City)] -> City -> City -> Path
reconstructPath preds src dest = reverse $ Data.List.unfoldr step (Just dest)
  where
    step Nothing = Nothing
    step (Just city)
      | city == src = Just (city, Nothing)
      | otherwise = case lookup city preds of
                      Just (Just predCity) -> Just (city, Just predCity)
                      _ -> Nothing

-- Shortest path function
shortestPath :: RoadMap -> City -> City -> Path
shortestPath roadMap source destination
  | source == destination = [source]
  | otherwise =
      let cities = Data.List.nub (concat [[c1, c2] | (c1, c2, _) <- roadMap])
          distances = initialize cities source
          predecessors = [(city, Nothing :: Maybe City) | city <- cities]  -- Initialize with Maybe City
          finalDistances = dijkstra roadMap cities source distances predecessors
      in reconstructPath predecessors source destination

      --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

travelSales :: RoadMap -> Path
travelSales = undefined

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
  print (shortestPath gTest2 "0" "3")
