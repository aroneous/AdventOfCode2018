import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Char as Char

-- Reverse a string of digits and parse to an integer
parseDigits digits = read (reverse digits) :: Integer

-- Find integers in a string with arbitrary non-digit separators, and return an array of them (in reverse order)
processNumber :: String -> String -> [Integer] -> [Integer]
processNumber [] [] res = res
processNumber [] digits res = let newNum = parseDigits digits in newNum : res
processNumber (x : xs) digits res = if Char.isDigit x
    then processNumber xs (x : digits) res
    else case digits of
        [] -> processNumber xs digits res
        _ -> let newNum = parseDigits digits in processNumber xs [] (newNum : res)

-- Return array of integers corresponding to integer values in the provided string
findNumbers :: String -> [Integer]
findNumbers s = reverse (processNumber s [] [])

recordRow :: Integer -> Integer -> Integer -> Integer -> Map.Map (Integer, Integer) (Set.Set Integer) -> Map.Map (Integer, Integer) (Set.Set Integer)
recordRow x y 0 id board = board
recordRow x y w id board = recordRow (x + 1) y (w - 1) id (Map.insertWith Set.union (x, y) (Set.singleton id) board)

-- Record an entry into the map.
-- Map.Map is from (x, y) coordinate to number of entries present there
recordEntry :: Integer -> Integer -> Integer -> Integer -> Integer -> Map.Map (Integer, Integer) (Set.Set Integer) -> Map.Map (Integer, Integer) (Set.Set Integer)
recordEntry x y w 0 id board = board
recordEntry x y w h id board = recordEntry x (y + 1) w (h - 1) id (recordRow x y w id board)

countMultiples :: Map.Map (Integer, Integer) Integer -> Integer
countMultiples board = Map.foldl (\count a -> if a > 1 then count + 1 else count) 0 board

getOrDefault :: Ord k => k -> a -> Map.Map k a -> a
getOrDefault key def m = case Map.lookup key m of
    Just val -> val
    Nothing -> def

mapSets :: Ord a => Map.Map a [(Set.Set a)] -> Set.Set a -> Map.Map a [(Set.Set a)]
mapSets m s = Set.foldl (\m' id -> let existing = getOrDefault id [] m' in Map.insert id (s:existing) m') m s

invertBoard board = Map.foldl mapSets Map.empty board

onlySingletonSets :: [Set.Set a] -> Bool
onlySingletonSets [] = True
onlySingletonSets (x:xs) = Set.size x == 1 && onlySingletonSets xs

findNonOverlapping :: Map.Map a [Set.Set a] -> Map.Map a [Set.Set a]
findNonOverlapping m = Map.filter onlySingletonSets m

recordEntry' board entry = let [id,x,y,w,h] = entry in recordEntry x y w h id board

main = do
    contents <- getContents
    let l = lines contents
        entries = map findNumbers l
        board = foldl recordEntry' Map.empty entries
        inverted = invertBoard board
        nonOverlapping = findNonOverlapping inverted
--        uniques = Map.filter (\a -> Set.size a == 1) board
--        uniqueMapEntry = head (Map.toList uniques)
--        uniqueId = head (Set.toList (snd uniqueMapEntry))
--    print uniqueId
--    print (take 2 (Map.toList inverted))
--    print nonOverlapping
    print (fst (head (Map.toList nonOverlapping)))
