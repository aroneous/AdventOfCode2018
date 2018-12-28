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

data Tool = Neither | Torch | Climbing deriving (Show, Eq, Ord)

type Coord = (Int, Int)

type DistMap = Map Coord (Tool, Int)

erosionLevel :: Map Coord Int -> Coord -> (Map Coord Int, Int)
erosionLevel board coord =
    case Map.lookup coord board of
        Just gi -> (board, (gi + depth) `mod` 20183)
        Nothing ->
            let board' = geologicIndex board coord
            in erosionLevel board' coord

geologicIndex :: Map Coord Int -> Coord -> Map Coord Int
geologicIndex board coord
    | Map.member coord board = board
    | coord == (0,0) = Map.insert coord 0 board
    | coord == target = Map.insert coord 0 board
    | y == 0 = Map.insert coord (x * 16807) board
    | x == 0 = Map.insert coord (y * 48271) board
    | otherwise =
        let (board', el1) = erosionLevel board (x-1, y)
            (board'', el2) = erosionLevel board' (x, y-1)
        in Map.insert coord (el1 * el2) board''
    where (x,y) = coord

buildBoard :: Map Coord Int
buildBoard = List.foldl (\board coord -> geologicIndex board coord) Map.empty [(x,y) | y <- [0..snd target], x <- [0..fst target]]

cellType :: Map Coord Int -> Coord -> (Map Coord Int, Int)
cellType board coord =
    let (board', el) = erosionLevel board coord
    in (board', el `mod` 3)

buildTypeBoard :: Map Coord Int -> Map Coord Int
buildTypeBoard board = Map.mapWithKey (\coord _ ->
        let (_, t) = cellType board coord
        in t) board


-- distBoard contains current shortest distances to known coordinates
-- frontierSet contains set of coordinates to explore further from
-- Process shortest distance in frontierSet first (priority queue) - remove when doing
-- If coord is target, that's the answer
-- Find reachable neighbors and total distance if going through this square
-- If neighbor not seen or new distance is shorter than old, add (new) distance to distBoard, add to frontierSet
toolOkForType :: Tool -> Int -> Bool
toolOkForType tool cellType =
    case cellType of
        0 -> tool `elem` [Climbing, Torch] -- Rocky
        1 -> tool `elem` [Neither, Climbing] -- Wet
        2 -> tool `elem` [Neither, Torch] -- Narrow

toolForTransition :: Int -> Int -> Tool
toolForTransition a b =
    let trans = if a < b then (a, b) else (b, a) -- Order them lower to higher
    in case trans of
        (0, 1) -> Climbing
        (0, 2) -> Torch
        (1, 2) -> Neither

neighbors :: Coord -> [Coord]
--neighbors (x,y) = filter (\(x,y) -> x >= 0 && y >= 0 && x < (fst target) * 2 && y < (snd target) * 2) [(x-1, y), (x+1, y), (x,y-1), (x,y+1)]
neighbors (x,y) = filter (\(x,y) -> x >= 0 && y >= 0) [(x-1, y), (x+1, y), (x,y-1), (x,y+1)]

neighborCosts :: Tool -> Map Coord Int -> Coord -> (Map Coord Int, [(Coord, (Tool, Int))])
neighborCosts tool board coord =
    let n = neighbors coord
        (board', fromType) = cellType board coord
        (board'', neighborTypes) = List.foldl (\(b, res) c ->
                let (b', ct) = cellType b c
                in (b', ((c, ct):res))) (board', []) n
        -- Compute incremental costs to neighbors
        costs = map (\(c, toType) ->
                let newTool = if fromType == toType then tool else toolForTransition fromType toType
                    cost = if newTool == tool then 1 else 8
                    cost' = if c == target
                        then if newTool /= Torch then cost + 7  else cost
                        else cost
                in (c, (newTool, cost'))) neighborTypes
    in (board'', costs)

findCellToProcess :: Map Int (Set Coord) -> (Map Int (Set Coord), Coord)
findCellToProcess frontierSet =
    let (minVal, closestSet) = Map.findMin frontierSet
        coord = Set.findMin closestSet
        closestSet' = Set.difference closestSet (Set.singleton coord)
        frontierSet' = if Set.null closestSet' then Map.deleteMin frontierSet else Map.insert minVal closestSet' frontierSet
    in (frontierSet', coord)

iterateDist :: Map Coord Int -> DistMap -> Map Int (Set Coord) -> (Map Coord Int, DistMap, Map Int (Set Coord))
iterateDist board distMap frontierSet =
    --let (frontierSet', toProcess) = trace (show $ Map.findMax distMap) $ findCellToProcess distMap frontierSet
    let (frontierSet', toProcess) = findCellToProcess frontierSet
        (tool, currDist) = distMap ! toProcess
        --(tool, currDist) = trace (show toProcess) $ distMap ! toProcess
        --(board', costs) = neighborCosts tool board toProcess
        (board', costs) = trace ((show toProcess) ++ " = " ++ (show currDist)) $ neighborCosts tool board toProcess
        costs' = map (\(coord, (tool, cost)) -> (coord, (tool, cost + currDist))) costs
        (distMap', frontierSet'') = List.foldl (\(dm, fs) (coord, (tool, newDist)) ->
                let doAdd = case Map.lookup coord dm of
                        Nothing -> True
                        Just (_, currDist) -> newDist < currDist
                in if doAdd
                    then
                        let dm' = Map.insert coord (tool, newDist) dm
                            fs' = Map.insertWith Set.union newDist (Set.singleton coord) fs
                        in (dm', fs')
                    else (dm, fs)) (distMap, frontierSet') costs'
    in (board', distMap', frontierSet'')

findTarget :: Map Coord Int -> Int
findTarget board =
    let initDistMap = Map.singleton origin (Torch, 0)
        initFrontierSet = Map.singleton 0 (Set.singleton origin)
    --let initDistMap = Map.singleton target (Torch, 0)
        --initFrontierSet = Set.singleton target
        (_, finalDistMap, _) = until (\(_, dm, fs) ->
                let (_, toProcess) = findCellToProcess fs
                in toProcess == finish)
            (\(b, dm, fs) -> iterateDist b dm fs)
            (board, initDistMap, initFrontierSet)
        (finalTool, targetDist) = finalDistMap ! finish
    in trace ("End: " ++ (show finalTool) ++ " " ++ (show targetDist)) targetDist

-- From input
depth = 7863
target = (14,760)

--origin = target
--finish = (0,0)
origin = (0,0)
finish = target

-- Test input
--depth = 510
--target = (10,10)

main = do
    let board = buildBoard
        typeBoard = buildTypeBoard board
        answer = Map.foldl (+) 0 typeBoard
        time = findTarget board
    print time
