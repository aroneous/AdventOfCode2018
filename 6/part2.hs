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

totalDistance :: Map.Map (Int, Int) Cell -> (Int, Int) -> Int
totalDistance board (x, y) = Map.foldlWithKey (\sum (x', y') _ -> sum + (abs (x - x')) + (abs (y - y'))) 0 board

pointsInRange :: Map.Map (Int, Int) Cell -> (Int, Int) -> (Int, Int) -> [(Int, Int)]
pointsInRange board (minx, miny) (maxx, maxy) = filter (\coord -> totalDistance board coord < 10000)
    [(x, y) | x <- [minx..maxx], y <- [miny..maxy]]

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
        numInRange = length $ pointsInRange board min max
    print $ numInRange
