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

type Drips = Set Coord

getCell :: Grid -> Coord -> CellState
getCell grid coord =
    case Map.lookup coord grid of
        Just cell -> cell
        Nothing -> Sand

down :: Coord -> Coord
down (x, y) = (x, y+1)

left :: Coord -> Coord
left (x, y) = (x-1, y)

right :: Coord -> Coord
right (x, y) = (x+1, y)

isSolid :: CellState -> Bool
isSolid cell = cell == Clay || cell == Pool

isPorous :: CellState -> Bool
isPorous cell = cell == Sand || cell == Drip

leftIsWall :: Grid -> Coord -> Bool
leftIsWall grid coord = isSolid $ getCell grid (left coord)

rightIsWall :: Grid -> Coord -> Bool
rightIsWall grid coord = isSolid $ getCell grid (right coord)

downIsPorous :: Grid -> Coord -> Bool
downIsPorous grid coord = isPorous $ getCell grid (down coord)

-- Move left and find either a wall or a hole beneath. Return the column where that occurs and which case it is
traverseLeft :: Grid -> Coord -> (Coord, TraversalState)
traverseLeft grid (x, y) =
    let edge = head $ dropWhile (\coord -> not (leftIsWall grid coord || downIsPorous grid coord)) [(x', y) | x' <- [x, x-1..]]
        ts = if leftIsWall grid edge then Pooling else Dripping
    in (edge, ts)

-- Move right and find either a wall or a hole beneath. Return the column where that occurs and which case it is
traverseRight :: Grid -> Coord -> (Coord, TraversalState)
traverseRight grid (x, y) =
    let edge = head $ dropWhile (\coord -> not (rightIsWall grid coord || downIsPorous grid coord)) [(x', y) | x' <- [x..]]
        ts = if rightIsWall grid edge then Pooling else Dripping
    in (edge, ts)

fillLevel :: Grid -> Drips -> Coord -> (Grid, Drips, TraversalState)
fillLevel grid drips coord =
    let (x, y) = coord
        ((xl, _), ls) = traverseLeft grid coord
        ((xr, _), rs) = traverseRight grid coord
        levelState = if ls == Pooling && rs == Pooling then Pooling else Dripping
        cellState = if levelState == Pooling then Pool else Drip
        grid' = List.foldl (\g c -> Map.insert c cellState g) grid [(x', y) | x' <- [xl..xr]]
        drips' = if ls == Dripping then Set.insert (xl, y+1) drips else drips
        drips'' = if rs == Dripping then Set.insert (xr, y+1) drips' else drips'
    in (grid', drips'', levelState)

processDrip :: Int -> Grid -> Drips -> Coord -> (Grid, Drips, TraversalState)
processDrip maxy grid drips drip =
    if (snd drip) > maxy
    then (grid, drips, Dripping)
    else
        if downIsPorous grid drip
        then
            let (grid', drips', downState) = processDrip maxy grid drips (down drip)
            in if downState == Pooling
                then fillLevel grid' drips' drip
                else
                    let grid'' = Map.insert drip Drip grid'
                    in (grid'', drips', Dripping)
        else fillLevel grid drips drip

processDrips :: Int -> Grid -> Drips -> (Grid, Drips)
processDrips maxy grid drips =
    case Set.lookupMin drips of
        Nothing -> (grid, drips)
        Just drip ->
            let drips' = Set.deleteMin drips
                (grid', drips'', _) = processDrip maxy grid drips' drip
            in trace (show drips'') $ processDrips maxy grid' drips''
            --in (grid', drips'')

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

--printBoard :: Set Coord -> Coord -> Coord -> String
--printBoard claySet (minx,miny) (maxx,maxy) =
    --let rows = map (\y -> map (\x -> if Set.member (x,y) claySet then '#' else '.') [minx..maxx]) [miny..maxy]
    --in List.intercalate "\n" rows

printBoard :: Grid -> Coord -> Coord -> String
printBoard grid (minx,miny) (maxx,maxy) =
    let rows = map (\y -> map (\x -> case getCell grid (x,y) of
            Clay -> '#'
            Sand -> '.'
            Pool -> '~'
            Drip -> '|') [minx..maxx]) [miny..maxy]
    in List.intercalate "\n" rows

main = do
    contents <- getContents
    let l = lines contents
        coords = l >>= parseLine
        claySet = Set.fromList coords
        grid = Set.foldl (\m c -> Map.insert c Clay m) Map.empty claySet -- Build our initial grid, which is clay in the enumerated positions
        (minx, _) = List.minimumBy (comparing fst) coords
        (maxx, _) = List.maximumBy (comparing fst) coords
        (_, miny) = List.minimumBy (comparing snd) coords
        (_, maxy) = List.maximumBy (comparing snd) coords
        initialDrip = (500, miny)
        (finalGrid, _) = processDrips maxy grid (Set.singleton initialDrip)
        answer = Map.size $ Map.filter (\c -> c == Pool || c == Drip) finalGrid
        --board = printBoard claySet (minx,miny) (maxx,maxy)
        board = printBoard finalGrid (minx,miny) (maxx,maxy)
    --print $ take 100 coords
    print [(minx, miny),(maxx, maxy)]
    putStrLn board
    print answer
