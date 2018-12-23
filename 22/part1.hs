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

erosionLevel :: Map Coord Int -> Coord -> Int
erosionLevel board coord = ((board ! coord) + depth) `mod` 20183

geologicIndex :: Map Coord Int -> Coord -> Map Coord Int
geologicIndex board coord
    | Map.member coord board = board
    | coord == (0,0) = Map.insert coord 0 board
    | coord == target = Map.insert coord 0 board
    | y == 0 = Map.insert coord (x * 16807) board
    | x == 0 = Map.insert coord (y * 48271) board
    | otherwise = Map.insert coord (erosionLevel board (x-1,y) * erosionLevel board (x,y-1)) board
    where (x,y) = coord

buildBoard :: Map Coord Int
buildBoard = List.foldl (\board coord -> geologicIndex board coord) Map.empty [(x,y) | y <- [0..snd target], x <- [0..fst target]]

buildTypeBoard :: Map Coord Int -> Map Coord Int
buildTypeBoard board = Map.mapWithKey (\coord _ -> (erosionLevel board coord) `mod` 3) board

-- From input
depth = 7863
target = (14,760)

main = do
    let board = buildBoard
        typeBoard = buildTypeBoard board
        answer = Map.foldl (+) 0 typeBoard
    print answer
