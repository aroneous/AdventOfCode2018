import qualified Data.Map as Map
import Data.Map (Map, insert, fromList, (!))
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.List as List
import qualified Data.List.Split as Split
import qualified Data.ByteString as B
import qualified Data.Sequence as S
import Data.Sequence (Seq, index)
import Data.Foldable (toList)
import Data.Monoid (mappend)
import Data.Maybe (fromJust, isJust)
import Data.Bits ((.|.),(.&.))
import Data.Char
import Data.Ord
import Debug.Trace (trace)
import Text.Regex.TDFA

-- What states can a cell be in?
data CellState = Open | Trees | Lumberyard deriving (Show, Eq)

type Grid = Map Coord CellState

type Coord = (Int, Int)

down :: Coord -> Coord
down (x, y) = (x, y+1)

left :: Coord -> Coord
left (x, y) = (x-1, y)

right :: Coord -> Coord
right (x, y) = (x+1, y)

neighbors :: Grid -> Coord -> [CellState]
neighbors grid (x, y) =
    let neighCoords = [(x',y') | x' <- [x-1..x+1], y' <- [y-1..y+1], (x',y') /= (x,y)]
        neighborMaybes = map (\c -> Map.lookup c grid) neighCoords
    in map fromJust $ filter isJust neighborMaybes

nextCellState :: Grid -> Coord -> CellState
nextCellState grid coord =
    let ns = neighbors grid coord
        cs = grid ! coord
    in case cs of
        Open -> if length (filter (==Trees) ns) >= 3 then Trees else Open
        Trees -> if length (filter (==Lumberyard) ns) >= 3 then Lumberyard else Trees
        Lumberyard ->
            let numLumb = length (filter (==Lumberyard) ns)
                numTrees = length (filter (==Trees) ns)
            in if numLumb >= 1 && numTrees >= 1 then Lumberyard else Open

nextState :: Grid -> Grid
nextState grid =
    Map.mapWithKey (\coord _ -> nextCellState grid coord) grid

printBoard :: Grid -> Coord -> Coord -> String
printBoard grid (minx,miny) (maxx,maxy) =
    let rows = map (\y -> map (\x -> case grid ! (x,y) of
            Open -> '.'
            Trees -> '|'
            Lumberyard -> '#') [minx..maxx]) [miny..maxy]
    in List.intercalate "\n" rows

cellValue :: Char -> CellState
cellValue c = case c of
    '.' -> Open
    '|' -> Trees
    '#' -> Lumberyard

parseBoard :: [String] -> Grid
parseBoard lines =
    List.foldl (\grid (y, line) ->
        List.foldl (\grid (x, c) -> Map.insert (x,y) (cellValue c) grid)
            grid (zip [1..] (List.filter (/='\n') line))) Map.empty (zip [1..] lines)

getCounts :: Grid -> (Int, Int)
getCounts grid =
    let numLumb = Map.size (Map.filter (==Lumberyard) grid)
        numTrees = Map.size (Map.filter (==Trees) grid)
    in (numLumb, numTrees)

getAnswer :: Grid -> Int
getAnswer grid =
    let numLumb = Map.size (Map.filter (==Lumberyard) grid)
        numTrees = Map.size (Map.filter (==Trees) grid)
    in numLumb * numTrees

main = do
    contents <- getContents
    let l = lines contents
        grid = parseBoard l
        (minCoord, _) = Map.findMin grid
        (maxCoord, _) = Map.findMax grid
        --finalState = (iterate nextState grid) !! 10
        its = iterate nextState grid
        --board = printBoard finalState minCoord maxCoord
        --answer = getAnswer finalState
    --print [minCoord, maxCoord]
    --putStrLn board
    --sequence $ take 10 (map (\state -> putStrLn (printBoard state minCoord maxCoord)) (drop 100 its))
    sequence $ take 1000 (map (\(idx, it) -> putStrLn ((show idx) ++ ": " ++ (show $ getCounts it))) (zip [0..] its))

-- Inspection of the data revealed a cycle of period 28 in the number of trees and lumberyards. 1,000,000,000 mod 28 is 20.
-- So is 1000 % 28 - so the answer is the same as after 1000 cycles: 358 * 651 = 233058
