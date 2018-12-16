import qualified Data.Map as Map
import Data.Map (Map, insert, fromList, (!))
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.List as List
import qualified Data.List.Split as Split
import qualified Data.ByteString as B
import Data.Monoid (mappend)
import Data.Maybe (fromJust, isJust)
import Data.Char
import Data.Ord
import Debug.Trace (trace)

data Race = Elf | Goblin deriving (Show, Eq)

data Beast = Beast {
    race :: Race,
    hp :: Int,
    attack :: Int
} deriving Show

data Cell = Empty | Occupied Beast deriving Show

data Direction = N | S | E | W deriving Show

type Coord = (Int, Int)

type Grid = Map Coord Cell

startHp = 200
goblinAttackPower = 3
elfAttackPower = 4

startingElf = Occupied (Beast Elf startHp elfAttackPower)
startingGoblin = Occupied (Beast Goblin startHp goblinAttackPower)

enemy Elf = Goblin
enemy Goblin = Elf

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
    found :: Set Coord -- Reached target coordinates
} deriving Show

neighbors :: Coord -> [Coord]
neighbors (y,x) = [(y-1, x), (y+1, x), (y,x-1), (y,x+1)]

cellSearchAction :: Grid -> Set Coord -> Coord -> CellStatus
cellSearchAction grid targets coord = 
    if Set.member coord targets
    then Match
    else case Map.lookup coord grid of
        Just Empty -> Valid
        _ -> Invalid

-- Takes grid, target coordinates, map of coords visited during this search to distance from start point, coord to search from
-- Returns (newly-visited valid cells, and coordinates of matching cells)
searchFromCell :: Grid -> Set Coord -> Map Coord Int -> Coord -> (Set Coord, Set Coord)
searchFromCell grid targets seen coord =
    --let distance = (seen ! coord) + 1 -- distance of new finds from start point == distance of this point + 1
        -- Find neighbors of this cell that haven't already been visited
    let newNeighbors = List.filter (\c -> not $ Map.member c seen) $ neighbors coord
        -- For each neighbor, see if it's a valid next step in the path or a target element
        result = List.foldl
            (\(newSeen, found) coord ->
                case cellSearchAction grid targets coord of
                    Invalid -> (newSeen, found)
                    Valid -> (Set.insert coord newSeen, found)
                    Match -> (Set.insert coord newSeen, Set.insert coord found))
            (Set.empty, Set.empty) newNeighbors
    in result

-- Perform an interation of a search, expanding outward by one step where possible
doSearchIteration :: Grid -> Set Coord -> SearchState -> SearchState
doSearchIteration grid targets (SearchState seen frontier found) =
        -- Evaluate all cells on the frontier
    let (allSeen, newlySeen, newFound) = (Set.foldl (\(allSeen, newlySeen, newFound) coord ->
            let newDist = (allSeen ! coord) + 1 -- Distance of newly-found cells is distance of this cell + 1
                (hereSeen, hereFound) = searchFromCell grid targets allSeen coord -- Search this cells neighbors
                allSeen' = Set.foldl (\as c -> Map.insert c newDist as) allSeen hereSeen -- Add newly-visited cells to master 'seen' map
                newlySeen' = Set.union newlySeen hereSeen -- Add newly-visited cells to 'frontier' set
                newFound' = Set.union newFound hereFound -- Add newly-found cells to 'found' set
            in (allSeen', newlySeen', newFound')) (seen, Set.empty, found) frontier)
    in SearchState allSeen newlySeen newFound -- Return updated master 'seen' map, the new frontier, and set of found cells

findClosest :: Grid -> Set Coord -> Coord -> Maybe Coord
findClosest grid targets start =
    let startState = SearchState (Map.singleton start 0) (Set.singleton start) Set.empty
        SearchState _ _ found = until (\(SearchState _ newlySeen found) -> not (Set.null found) || Set.null newlySeen)
            (doSearchIteration grid targets) startState
    in if not (Set.null found) then Just (head (Set.toList found)) else Nothing

-- Get distance map with 'to' as the target, expanded to reaxch 'from'. A path is assumed to exist.
distMap :: Grid -> Coord -> Coord -> Map Coord Int
distMap grid from to =
    let startState = SearchState (Map.singleton to 0) (Set.singleton to) Set.empty
        SearchState seen _ found = until (\(SearchState _ newlySeen found) -> not (Set.null found) || Set.null newlySeen)
            (doSearchIteration grid (Set.singleton from)) startState
    in if not (Set.null found) then seen else error "No path found"

-- Get coordinates of all beasts of the given race
getBeasts :: Grid -> Race -> [Coord]
getBeasts grid race =
    let beastMap = Map.filter (\cell -> case cell of
            Occupied (Beast r _ _) -> race == r
            _ -> False) grid
    in map fst $ Map.toList beastMap

getBeastMap :: Grid -> Map Coord Beast
getBeastMap grid =
    let beastCells = Map.filter (\cell -> case cell of
            Occupied (Beast _ _ _) -> True
            _ -> False) grid
    in Map.map (\cell -> case cell of
            Occupied b -> b) beastCells

-- Get all spaces that are in range of beasts of the specified race. These are orthogonally-adjacent spaces
-- that are available
inRangeSpaces :: Grid -> Race -> [Coord]
inRangeSpaces grid race =
    let beasts = getBeasts grid race
        possible = beasts >>= neighbors
        inRange = filter (\coord -> case Map.lookup coord grid of
                Just Empty -> True
                _ -> False) possible
    in inRange

-- Find adjacent cells containing a beast of the specified race
targetsInRange :: Grid -> Race -> Coord -> [Coord]
targetsInRange grid targetRace coord = filter (\c -> case Map.lookup c grid of
        Just (Occupied (Beast r _ _)) -> r == targetRace
        _ -> False) $ neighbors coord

preferredTarget :: Grid -> Race -> Coord -> Maybe Coord
preferredTarget grid targetRace coord =
    let targets = List.foldl (\l c -> case Map.lookup c grid of
            Just (Occupied (Beast r hp _)) -> if r == targetRace then ((hp, c):l) else l
            _ -> l) [] $ neighbors coord
        sorted = List.sort targets
    in if List.null sorted then Nothing else Just (snd (head sorted))

doMove :: Grid -> Coord -> Coord -> Coord
doMove grid pos target =
    let dm = distMap grid pos target
        neigh = Set.fromList (neighbors pos)
        possibleMoves = Map.filterWithKey (\c _ -> Set.member c neigh) dm
        (nextCoord, _) = Map.findMin possibleMoves
    in nextCoord

-- Move and possibly attack
-- Return updated grid and maybe coordinates of a killed enemy
doMoveAndAttack :: Grid -> Coord -> Race -> Int -> (Grid, Maybe Coord, Bool)
doMoveAndAttack grid coord targetRace attackPower =
    case findClosest grid (Set.fromList $ getBeasts grid targetRace) coord of
        Just target ->
            let nextCoord = doMove grid coord target
                beast = grid ! coord
                grid' = Map.insert coord Empty grid
                grid'' = Map.insert nextCoord beast grid'
                maybeTarget = preferredTarget grid'' targetRace nextCoord
            in case maybeTarget of
                Nothing -> (grid'', Nothing, False)
                Just target -> doAttack grid'' target attackPower
        Nothing -> (grid, Nothing, False) -- Nothing we can reach - just stay where we are

-- Attack in-range target
-- Return updated grid, maybe coordinates of a killed enemy, and whether an elf died
doAttack :: Grid -> Coord -> Int -> (Grid, Maybe Coord, Bool)
doAttack grid target attackPower =
    let Occupied (Beast r hp enemyAttack) = grid ! target
        newHp = hp - attackPower
        killed = if newHp <= 0 then Just target else Nothing
        elfDied = isJust killed && r == Elf
        grid' = if newHp <= 0
            then Map.insert target Empty grid
            else Map.insert target (Occupied (Beast r newHp enemyAttack)) grid
    in (grid', killed, elfDied)

-- Return updated grid, maybe coordinates of a killed enemy, and whether an elf died
doTurnForUnit :: Grid -> Coord -> (Grid, Maybe Coord, Bool)
doTurnForUnit grid coord =
    let Occupied (Beast race hp attackPower) = grid ! coord -- Get details of cell occupant (assumed to exist)
        targetRace = enemy race
        maybeTarget = preferredTarget grid targetRace coord
        result = case maybeTarget of
            Nothing -> doMoveAndAttack grid coord targetRace attackPower
            Just target -> doAttack grid target attackPower
    in result

doTurn :: Grid -> (Grid, Bool, Bool)
doTurn grid =
    let beastMap = getBeastMap grid -- The beasts we have to process a turn for (unless they die before their turn)
        (nextGrid, killed, done, elfDied) = Map.foldlWithKey (\(grid', killed, done, elfDied) coord beast ->
                if done
                then (grid', killed, True, elfDied) -- Combat already done - stop
                else if Set.member coord killed
                    then (grid', killed, False, elfDied) -- Unit killed this turn - skip
                    else if List.null (getBeasts grid' (enemy (race beast)))
                        then (grid', killed, True, False) -- Combat done! No enemies
                        else
                            let (grid'', maybeKilled, elfDied) = doTurnForUnit grid' coord
                                killed' = case maybeKilled of
                                        Just killedCoord -> Set.insert killedCoord killed
                                        Nothing -> killed
                            in (grid'', killed', elfDied, elfDied)) (grid, Set.empty, False, False) beastMap
    in (nextGrid, done, elfDied)

-- Return final grid, number of full rounds, and whether an elf died
playGame :: Grid -> Coord -> (Grid, Int, Bool)
playGame grid hw =
    --let (grid', done) = trace "doing turn" (doTurn grid)
    let (grid', done, elfDied) = doTurn grid
    in
        if done
        then (grid', 0, elfDied)
        else
            let (grid'', rounds, elfDied') = playGame grid' hw
            in (grid'', rounds + 1, elfDied')

setElfAttack :: Grid -> Int -> Grid
setElfAttack grid attack =
    Map.map (\cell -> case cell of
        Occupied (Beast Elf hp _) -> Occupied (Beast Elf hp attack)
        _ -> cell) grid

findMinAttackPower :: Grid -> Coord -> (Grid, Int)
findMinAttackPower grid hw =
    let (grid', rounds, _) = head (dropWhile (\(_, _, elfDied) -> elfDied) $
            map (\attack -> let grid' = setElfAttack grid attack in playGame grid' hw) [4..])
    in (grid', rounds)

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
        Just (Occupied (Beast Elf _ _)) -> 'E'
        Just (Occupied (Beast Goblin _ _)) -> 'G'

printRow :: Grid -> Int -> [Int] -> Map Coord Char -> String
printRow grid y xs annotations =
    map (\x -> case Map.lookup (y,x) annotations of
            Just char -> char
            Nothing -> printCell (Map.lookup (y,x) grid)) xs

printGridAnnotated :: Grid -> Coord -> Map Coord Char -> String
printGridAnnotated grid (height, width) annotations =
    let rows = map (\y -> printRow grid y [0..width] annotations) [0..height]
    in List.intercalate "\n" rows

printGrid :: Grid -> Coord -> String
printGrid grid hw = printGridAnnotated grid hw Map.empty

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
        inRange = inRangeSpaces grid Elf
        inRangeSet = Set.fromList inRange
        closest = findClosest grid inRangeSet coord
        closest2 = findClosest grid inRangeSet coord2
        toCoord = fromJust closest
        fromCoord = coord
        dists = distMap grid fromCoord toCoord
        distChars = Map.map (\d -> head $ show (d `mod` 10)) dists
        --(grid', numRound, elfDied) = playGame grid (height, width)
        (grid', numRound ) = findMinAttackPower grid (height, width)
        remainingHp = Map.foldl (\sum beast -> sum + hp beast) 0 (getBeastMap grid')
        answer = numRound * remainingHp
    putStrLn $ printGrid grid (height, width)
    --putStrLn $ printGridAnnotated grid (height, width) distChars
    --putStrLn $ printGridAnnotated grid (height, width) (Map.fromList (zip (Set.toList inRangeSet) (repeat '?')))
    --print beasts
    --print closest
    --print closest2
    --print inRange
    putStrLn $ printGrid grid' (height, width)
    putStr "Num rounds: "
    print numRound
    putStr "Remaining hit points: "
    print remainingHp
    putStr "Answer: "
    print answer
    -- putStrLn $ "Elf died: " ++ (show elfDied)
