import qualified Data.Map as Map
import Data.Map (Map, insert, fromList, (!))
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.List as List
import qualified Data.List.Split as Split
import qualified Data.ByteString as B
import Data.Monoid (mappend)
import Data.Char
import Data.Ord
import Data.Word
import Codec.BMP
import Debug.Trace (trace)

data Race = Elf | Goblin deriving (Show, Eq)

data Beast = Beast {
    race :: Race,
    hp :: Int
} deriving Show

data Cell = Empty | Occupied Beast deriving Show

data Direction = N | S | E | W deriving Show

type Coord = (Int, Int)

type Grid = Map Coord Cell

startHp = 200
attackPower = 3

startingElf = Occupied (Beast Elf startHp)
startingGoblin = Occupied (Beast Goblin startHp)

newCoord :: Coord -> Direction -> Coord
newCoord (y, x) dir =
    case dir of
        E -> (y, x+1)
        W -> (y, x-1)
        N -> (y-1, x)
        S -> (y+1, x)

parseCell :: Char -> Maybe Cell
parseCell c =
    case c of
        '#' -> Nothing
        '.' -> Just Empty
        'E' -> Just startingElf
        'G' -> Just startingGoblin
        '\n' -> Nothing

data CellStatus = Match | Valid | Invalid deriving Show

data SearchState = SearchState {
    seen :: Map Coord Int, -- Cells seen during the previous iterations of this search, and distance from start point
    frontier :: Set Coord, -- Cells newly visited on latest iteration
    found :: Set Coord -- Cells containing target race
} deriving Show

neighbors :: Coord -> [Coord]
neighbors (y,x) = [(y-1, x), (y+1, x), (y,x-1), (y,x+1)]

cellSearchAction :: Grid -> Race -> Coord -> CellStatus
cellSearchAction grid race coord = 
    case Map.lookup coord grid of
        Nothing -> Invalid
        Just Empty -> Valid
        Just (Occupied (Beast r _)) -> if r == race then Match else Invalid

-- Takes grid, target race, map of coords visited during this search to distance from start point, coord to search from
-- Returns (newly-visited valid cells, and coordinates of matching cells)
searchFromCell :: Grid -> Race -> Map Coord Int -> Coord -> (Set Coord, Set Coord)
searchFromCell grid race seen coord =
    --let distance = (seen ! coord) + 1 -- distance of new finds from start point == distance of this point + 1
        -- Find neighbors of this cell that haven't already been visited
    let newNeighbors = List.filter (\c -> not $ Map.member c seen) $ neighbors coord
        -- For each neighbor, see if it's a valid next step in the path or a target element
        result = List.foldl
            (\(newSeen, found) coord ->
                case cellSearchAction grid race coord of
                    Invalid -> (newSeen, found)
                    Valid -> (Set.insert coord newSeen, found)
                    Match -> (Set.insert coord newSeen, Set.insert coord found))
            (Set.empty, Set.empty) newNeighbors
    in result

-- Perform an interation of a search, expanding outward by one step where possible
doSearchIteration :: Grid -> Race -> SearchState -> SearchState
doSearchIteration grid race (SearchState seen frontier found) =
        -- Evaluate all cells on the frontier
    let (allSeen, newlySeen, newFound) = (Set.foldl (\(allSeen, newlySeen, newFound) coord ->
            let newDist = (allSeen ! coord) + 1 -- Distance of newly-found cells is distance of this cell + 1
                (hereSeen, hereFound) = searchFromCell grid race allSeen coord -- Search this cells neighbors
                allSeen' = Set.foldl (\as c -> Map.insert c newDist as) allSeen hereSeen -- Add newly-visited cells to master 'seen' map
                newlySeen' = Set.union newlySeen hereSeen -- Add newly-visited cells to 'frontier' set
                newFound' = Set.union newFound hereFound -- Add newly-found cells to 'found' set
            in (allSeen', newlySeen', newFound')) (seen, Set.empty, found) frontier)
    in SearchState allSeen newlySeen newFound -- Return updated master 'seen' map, the new frontier, and set of found cells

findClosest :: Grid -> Race -> Coord -> Maybe Coord
findClosest grid race start =
    let startState = SearchState (Map.singleton start 0) (Set.singleton start) Set.empty
        SearchState _ _ found = until (\(SearchState _ newlySeen found) -> not (Set.null found) || Set.null newlySeen)
            (doSearchIteration grid race) startState
    in if not (Set.null found) then Just (head (Set.toList found)) else Nothing

-- Coordinate should be in (y, x) form to take advantage of natural ordering to result in top-to-bottom, left-to-right
addCellEntry :: Grid -> Char -> Coord -> Grid
addCellEntry grid c coord =
    case parseCell c of
        Nothing -> grid
        Just cell -> insert coord cell grid

parseRow :: Grid -> Int -> String -> Grid
parseRow grid y s = List.foldl (\grid' (x, c) -> addCellEntry grid' c (y, x)) grid (zip [0..] s)

(!-) m k = case Map.lookup k m of
    Just a -> a
    Nothing -> error $ "Not found: " ++ (show k)

printCell :: Maybe Cell -> Char
printCell mc =
    case mc of
        Nothing -> '#'
        Just Empty -> '.'
        Just (Occupied (Beast Elf _)) -> 'E'
        Just (Occupied (Beast Goblin _)) -> 'G'

printRow :: Grid -> Int -> [Int] -> String
printRow grid y xs =
    map (printCell.(\x -> Map.lookup (y,x) grid)) xs

printGrid :: Grid -> Coord -> String
printGrid grid (height, width) =
    let rows = map (\y -> printRow grid y [0..width]) [0..height]
    in List.intercalate "\n" rows

main = do
    contents <- getContents
    let l = lines contents
        height = length l
        width = length (l !! 0) - 1 -- Ignore newline
        grid = List.foldl (\grid (y, row) -> parseRow grid y row) Map.empty (zip [0..] l)
        beasts = Map.filter (\cell -> case cell of
                Occupied _ -> True
                _ -> False) grid
        beastList = Map.toList beasts
        (coord, _) = head beastList
        (coord2, _) = beastList !! 1
        closest = findClosest grid Elf coord
        closest2 = findClosest grid Elf coord2
    putStrLn $ printGrid grid (height, width)
    print beasts
    print closest
    print closest2
