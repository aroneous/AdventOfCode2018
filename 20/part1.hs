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

-- Coordinates of a room
type Coord = (Int, Int)

-- Pair of adjacent coordinates, representing a doorway between two rooms. 'Smaller' coordinates should go first
type Doorway = (Coord, Coord)

-- Full set of doorways defining the grid
type Doors = Set Doorway

data Dir = N | S | E | W deriving (Show, Read, Eq, Ord)

-- A step list is a simple list of directions
type StepList = [Dir]

-- A pattern element is a list of steps or a sub-pattern (which is an 'or' group of patterns)
data PatternElement = Steps StepList | Subpattern [Pattern] deriving Show

-- A pattern is a list of pattern elements
type Pattern = [PatternElement]

-- Parsing input and building tree structure
--
parseStepList :: String -> (StepList, String)
parseStepList (p:ps)
    | p `elem` ['N','S','E','W'] =
        let d = (read [p] :: Dir)
            (ds, rest) = parseStepList ps
        in (d:ds, rest)
    | otherwise = ([], p:ps)

-- Pattern is a sequence of StepLists and subpatterns - sequential
-- Subpattern is a sequence of |-separated Patterns - alternatives
-- Parsing subpattern aggregates a list of |-separated patterns

parseSubpattern :: String -> ([Pattern], String)
parseSubpattern sp =
    let (pat, (r:rs)) = parsePattern sp
    in case r of
        '|' -> -- Another pattern follows within the group
            let (pats, rest) = parseSubpattern rs
            in (pat:pats, rest)
        ')' -> -- Ends subpattern
            ([pat], rs)

parsePattern :: String -> (Pattern, String)
parsePattern (p:ps)
    | p == '(' =
        let (subpats, rest) = parseSubpattern ps
            pe = Subpattern subpats
            (pes, rest') = parsePattern rest
        in (pe:pes, rest')
    | p `elem` "NSEW" =
        let (sl, rest) = parseStepList (p:ps)
            pe = Steps sl
            (pes, rest') = parsePattern rest
        in (pe:pes, rest')
    | p `elem` "|)$" = ([],(p:ps)) -- Terminating condition of a Pattern - new alternative, end of subgroup, end of input
    | otherwise = error $ "Unexpected input in parsePattern: " ++ (take 5 (p:ps)) ++ "..."

-- Iterating direction list

makeDoor :: Coord -> Coord -> Doorway
makeDoor a b = if a < b then (a, b) else (b, a)

addDoorway :: Coord -> Dir -> (Coord, Doorway)
addDoorway (sx, sy) dir =
    let end = case dir of
            N -> (sx, sy-1)
            S -> (sx, sy+1)
            E -> (sx+1, sy)
            W -> (sx-1, sy)
        door = makeDoor (sx, sy) end
    in (end, door)

executeStepList :: StepList -> Coord -> (Coord, Doors)
executeStepList steps start = List.foldl (\(s, ds) dir ->
        let (end, door) = addDoorway s dir
        in (end, Set.insert door ds)) (start, Set.empty) steps

iteratePattern :: Set Coord -> Pattern -> (Set Coord, Doors)
iteratePattern starts [] = (starts, Set.empty)
iteratePattern starts (Steps pe:pes) =
    let (ends, doors) = Set.foldl (\(ends, doors) start ->
            let (e, ds) = executeStepList pe start
            in (Set.insert e ends, Set.union doors ds))
                (Set.empty, Set.empty) starts
        (ends', doors') = iteratePattern ends pes
    in (ends', Set.union doors doors')
iteratePattern starts (Subpattern pe:pes) =
    -- pe is [Pattern]. Recurse across them
    let (ends, doors) = List.foldl (\(ends, doors) sp ->
            let (es, ds) = iteratePattern starts sp
            in (Set.union ends es, Set.union doors ds))
                (Set.empty, Set.empty) pe
        (ends', doors') = iteratePattern ends pes
    in (ends', Set.union doors doors')

-- Printing board

doorsFrom :: Doors -> Coord -> (String, String)
doorsFrom d (x, y) =
    let across = if Set.member ((x,y), (x+1,y)) d then '|' else '#'
        down = if Set.member ((x,y), (x,y+1)) d then '-' else '#'
        room = if (x, y) == (0, 0) then 'X' else '.'
    in ([room,across], [down, '#'])
    --in ([across,room], ['#',down]) -- building in reverse

printBoard :: Doors -> String
printBoard doors =
    let ((minx, miny), (maxx, maxy)) = Set.foldl (\((minx, miny), (maxx, maxy)) ((x1, y1), (x2, y2)) ->
            ((min minx x1, min miny y1), (max maxx x2, max maxy y2))) ((0, 0), (0, 0)) doors
        (racrosses, rdowns) = List.foldl (\(acrosses, downs) y ->
                let (across, down) = List.foldl (\(across, down) x ->
                        let (a, d) = doorsFrom doors (x,y)
                        in (across ++ a, down ++ d)) ("#","#") [minx..maxx]
                in (across:acrosses, down:downs)) ([],[]) [miny-1..maxy]
        --acrosses = map reverse $ reverse racrosses
        --downs = map reverse $ reverse rdowns
        acrosses = reverse racrosses
        downs = reverse rdowns
        --acrosses' = map (List.intersperse '.') acrosses
        --downs' = map (List.intersperse '#') downs
        --acrosses'' = map tail acrosses'
        --downs'' = tail downs'
        --downs'' = map (tail.init) downs'
        board = zipWith (\a b -> [a, b]) acrosses downs >>= (\a -> a)
        board' = List.intercalate "\n" $ tail board
    in board'

-- Traversing board to find distance

neighbors :: Doors -> Coord -> Set Coord
neighbors doors (x,y) =
    let doorways = filter (\c -> Set.member c doors) [((x-1,y),(x,y)),((x,y-1),(x,y)),((x,y),(x+1,y)),((x,y),(x,y+1))]
        coords = Set.fromList $ doorways >>= (\(a,b) -> [a,b])
    in Set.delete (x,y) coords

-- Takes set of all seen coordinates, starting points to look outward from, and returns newly-found points
findUnseenNeighbors :: Doors -> Set Coord -> Set Coord -> Set Coord
findUnseenNeighbors doors seen starts =
    let allNeighbors = Set.foldl Set.union Set.empty $ Set.map (neighbors doors) starts
        unseenNeighbors = Set.filter (\c -> not $ Set.member c seen) allNeighbors
    in unseenNeighbors

findFarthest :: Doors -> Set Coord -> Set Coord -> Int
findFarthest doors seen starts
    | Set.null starts = -1
    | otherwise = 
        let freshMeat = findUnseenNeighbors doors seen starts
            seen' = Set.union seen freshMeat
        in 1 + (findFarthest doors seen' freshMeat)

main = do
    contents <- getContents
    let (patt, _) = parsePattern $ tail contents
        (ends, doors) = iteratePattern (Set.singleton (0,0)) patt
        farthest = findFarthest doors (Set.singleton (0,0)) (Set.singleton (0,0))
    --print doors
    putStrLn $ printBoard doors
    print farthest
