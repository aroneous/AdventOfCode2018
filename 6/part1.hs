import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as List
import Data.Char
import Data.Ord

parseDigits digits = read (reverse digits) :: Int

getOrDefault :: Ord k => k -> a -> Map.Map k a -> a
getOrDefault key def m = case Map.lookup key m of
    Just val -> val
    Nothing -> def

getCell :: Ord k => k -> Map.Map k Cell -> Cell
getCell k = getOrDefault k Unknown

data Cell = Unknown | Closest Int | Tied deriving Show

parseNums :: String -> String -> [Int] -> [Int]
parseNums [] [] nums = reverse nums
parseNums [] digits nums = reverse ((parseDigits digits):nums)
parseNums (x:xs) [] nums = if isDigit x
    then parseNums xs [x] nums
    else parseNums xs [] nums
parseNums (x:xs) digits nums = if isDigit x
    then parseNums xs (x:digits) nums
    else parseNums xs [] ((parseDigits digits):nums)

parseEntry :: String -> (Int, Int)
parseEntry s = let [x,y] = parseNums s [] [] in (x, y)
--parseEntry :: String -> [Int]
--parseEntry s = parseNums s [] []

buildBoard :: Map.Map (Int, Int) Cell -> Int -> (Int, Int) -> Map.Map (Int, Int) Cell
buildBoard board idx coord = Map.insert coord (Closest idx) board

isClosestCell :: Cell -> Bool
isClosestCell cell = case cell of
    Closest _ -> True
    _ -> False

isUnknownCell :: Cell -> Bool
isUnknownCell cell = case cell of
    Unknown -> True
    _ -> False

knownNeighbors :: Map.Map (Int, Int) Cell -> (Int, Int) -> [Int]
knownNeighbors board (x,y) =
    let neighbors = [ getCell (x + 1, y) board, getCell (x - 1, y) board, getCell (x, y - 1) board, getCell (x, y + 1) board ]
        closests = filter isClosestCell neighbors
        ids = List.foldl (\s (Closest id) -> Set.insert id s) Set.empty closests in
    Set.toList ids

processCell :: Map.Map (Int, Int) Cell -> (Int, Int) -> Cell
processCell board coord = let curr = getCell coord board in
    if isUnknownCell curr
    then let kn = knownNeighbors board coord in 
        case kn of
            [x] -> Closest x
            (x:y:xs) -> Tied
            _ -> Unknown
    else curr

updateCell :: Ord k => k -> Cell -> Map.Map k Cell -> Map.Map k Cell
updateCell coord Unknown board = board
updateCell coord cell board = Map.insert coord cell board

processRow :: Map.Map (Int, Int) Cell -> Map.Map (Int, Int) Cell -> Int -> [Int] -> Map.Map (Int, Int) Cell
processRow _ board' _ [] = board'
processRow board board' y (x:xs) = processRow board (updateCell (x, y) (processCell board (x, y)) board') y xs

-- board -> min -> max
evalBoard :: Map.Map (Int, Int) Cell -> (Int, Int) -> (Int, Int) -> Map.Map (Int, Int) Cell
evalBoard board (minx, miny) (maxx, maxy) = List.foldl (\b y -> processRow board b y [minx..maxx]) board [miny..maxy]

unknownCells :: Map.Map (Int, Int) Cell -> (Int, Int) -> (Int, Int) -> [(Int, Int)]
unknownCells board (minx, miny) (maxx, maxy) =
    List.filter (\k -> isUnknownCell $ getCell k board) [(x,y) | x <- [minx..maxx], y <- [miny..maxy]]

boardComplete :: Map.Map (Int, Int) Cell -> (Int, Int) -> (Int, Int) -> Bool
boardComplete board min max = List.null $ unknownCells board min max

numUnknown board min max = List.length $ unknownCells board min max

iterateBoard :: Map.Map (Int, Int) Cell -> (Int, Int) -> (Int, Int) -> Map.Map (Int, Int) Cell
iterateBoard board min max = let board' = evalBoard board min max in
    if boardComplete board' min max
        then board'
        else iterateBoard board' min max

pad :: String -> String
pad [] = "  "
pad [x] = " " ++ [x]
--pad [x,y] = " " ++ [x,y]
pad x = x

printCell :: Cell -> String
printCell Unknown = "  "
printCell Tied = " ."
printCell (Closest id) = pad $ show id

printRow :: Map.Map (Int, Int) Cell -> Int -> [Int] -> String
printRow board y xs = let cells = map (\x -> printCell $ getCell (x,y) board) xs in
    List.intercalate " " cells

printBoard :: Map.Map (Int, Int) Cell -> (Int, Int) -> (Int, Int) -> String
printBoard board (minx, miny) (maxx, maxy) = let rows = map (\y -> printRow board y [minx..maxx]) [miny..maxy] in
    List.intercalate "\n" rows

getIdsIn :: Map.Map (Int, Int) Cell -> [(Int, Int)] -> Set.Set Int
getIdsIn board coords = List.foldl (\s coord -> case getCell coord board of
    Closest id -> Set.insert id s
    _ -> s) Set.empty coords

getIdsOnEdges :: Map.Map (Int, Int) Cell -> (Int, Int) -> (Int, Int) -> Set.Set Int
getIdsOnEdges board (minx,miny) (maxx,maxy) = Set.union
    (getIdsIn board [(x,y) | x <- [minx..maxx], y <- [miny,maxy]])
    (getIdsIn board [(x,y) | x <- [minx,maxx], y <- [miny..maxy]])

getCounts :: Map.Map (Int, Int) Cell -> Map.Map Int Int
getCounts = Map.foldl (\cm cell -> case cell of
    Closest id -> Map.insertWith (+) id 1 cm
    _ -> cm) Map.empty

findBiggest :: Map.Map (Int, Int) Cell -> (Int, Int) -> (Int, Int) -> Int
findBiggest board min max =
    let counts = getCounts board
        edgeIds = getIdsOnEdges board min max
        coreCounts = Map.filterWithKey (\id _ -> not $ Set.member id edgeIds) counts
        (maxId, count) = List.maximumBy (comparing snd) (Map.toList coreCounts) in
    count

main = do
    contents <- getContents
    let l = lines contents
        coords = map parseEntry l
        (minx,_) = List.minimumBy (comparing fst) coords
        (_,miny) = List.minimumBy (comparing snd) coords
        (maxx,_) = List.maximumBy (comparing fst) coords
        (_,maxy) = List.maximumBy (comparing snd) coords
        min = (minx, miny)
        max = (maxx, maxy)
        board = List.foldl (\board (idx, coord) -> buildBoard board idx coord) Map.empty $ zip [0..] coords
        (x, y) = head coords
        neibs = knownNeighbors board (x, y+1)
        bill = processCell board (x, y+1)
        tim = processRow board board (y+1) [(x-2)..(x+2)]
        filled = iterateBoard board min max
        biggest = findBiggest filled min max
--    putStr $ printBoard board min max
--    putStr $ printBoard filled min max
--    print filled
--    print $ numUnknown board min max
--    print $ numUnknown filled min max
    print $ biggest
