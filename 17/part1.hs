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
import Data.Maybe (fromJust)
import Data.Bits ((.|.),(.&.))
import Data.Char
import Data.Ord
import Debug.Trace (trace)
import Text.Regex.TDFA

data Dir = X | Y deriving (Show, Eq)

-- What states can a cell be in?
data CellState = Sand | Clay | Pool | Drip deriving (Show, Eq)

-- Status of processing of a given row [in a particular direction]: did it run into a wall and pool, or drip down?
data TraversalState =
      Pooling  -- Hit a clay (or pooling?) wall, so water could pool to a higher level
    | Dripping -- Found a hole beneath to let water drip down
    deriving (Show, Eq)

-- Status of row [below]: did it resut in pooling, or did it drip away?
data RowState = Pooled | Terminated

type Coord = (Int, Int)

type Grid = Map Coord CellState

traverseLeft :: Grid -> (Int, Int) -> (Grid, TraversalState)
traverseLeft grid (x, y) =

parseLine :: String -> [Coord]
parseLine line =
    let matches :: [[String]]
        matches = line =~ "(x|y)=([[:digit:]]+), (x|y)=([[:digit:]]+)\\.\\.([[:digit:]]+)"
        match = matches !! 0
        rangeDirStr = match !! 3
        rangeDir = if rangeDirStr == "x" then X else Y
        nonRange = read (match !! 2) :: Int
        rangeLow = read (match !! 4) :: Int
        rangeHigh = read (match !! 5) :: Int
    in if rangeDir == X
        then [(x, nonRange) | x <- [rangeLow..rangeHigh]]
        else [(nonRange, y) | y <- [rangeLow..rangeHigh]]

printBoard :: Set Coord -> Coord -> Coord -> String
printBoard claySet (minx,miny) (maxx,maxy) =
    let rows = map (\y -> map (\x -> if Set.member (x,y) claySet then '#' else '.') [minx..maxx]) [miny..maxy]
    in List.intercalate "\n" rows

main = do
    contents <- getContents
    let l = lines contents
        coords = l >>= parseLine
        claySet = Set.fromList coords
        grid = Set.foldl (\m c -> Map.insert c Clay m) claySet -- Build our initial grid, which is clay in the enumerated positions
        (minx, _) = List.minimumBy (comparing fst) coords
        (maxx, _) = List.maximumBy (comparing fst) coords
        (_, miny) = List.minimumBy (comparing snd) coords
        (_, maxy) = List.maximumBy (comparing snd) coords
        board = printBoard claySet (minx,miny) (maxx,maxy)
    --print $ take 100 coords
    print [(minx, miny),(maxx, maxy)]
    putStrLn board
