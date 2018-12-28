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

data Bot = Bot {
    x :: Int,
    y :: Int,
    z :: Int,
    r :: Int
} deriving Show

parseNum :: Bool -> String -> Int
parseNum neg s =
    let numpos = read s :: Int
    in if neg then -numpos else numpos

parseBot :: String -> Bot
parseBot line =
    let matches :: [[String]]
        matches = line =~ "pos=<(-?)([[:digit:]]+),(-?)([[:digit:]]+),(-?)([[:digit:]]+)>, r=([[:digit:]]+)"
        match = matches !! 0
        xneg = match !! 1 == "-"
        xs = match !! 2
        x = parseNum xneg xs
        yneg = match !! 3 == "-"
        ys = match !! 4
        y = parseNum yneg ys
        zneg = match !! 5 == "-"
        zs = match !! 6
        z = parseNum zneg zs
        r = read (match !! 7) :: Int
    in Bot x y z r

strongestBot :: [Bot] -> Bot
strongestBot bots = List.maximumBy (comparing r) bots

distance :: Bot -> Bot -> Int
distance (Bot xa ya za _) (Bot xb yb zb _) = (abs $ xa - xb) + (abs $ ya - yb) + (abs $ za - zb)

inRange :: Bot -> [Bot] -> [Bot]
inRange bot bots = filter (\ob -> distance bot ob < r bot) bots

main = do
    contents <- getContents
    let l = lines contents
        bots = map parseBot l
        strongest = strongestBot bots
        botsInRange = inRange strongest bots
        numInRange = length botsInRange
    print numInRange
