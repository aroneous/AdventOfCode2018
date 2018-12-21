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

-- Iteration state of a subpattern is the index of currently-selected subpattern element,
-- and nested subpattern iteration state for that subpattern
data SubpatState = SubpatState {
    idx :: Int,
    iterState :: [SubpatState]
} deriving Show

-- Iteration state of a pattern is a list of the subpattern states for subpatterns within the pattern
type IterationState = [SubpatState]

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

main = do
    contents <- getContents
    let (patt, _) = parsePattern $ tail contents
    print patt
