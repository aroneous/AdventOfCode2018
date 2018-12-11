import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as List
import qualified Data.List.Split as Split
import Data.Char
import Data.Ord

data Ring = Ring {
    counterclockwise :: [Int],
--    current :: Int
    clockwise :: [Int]
} deriving (Show)

-- Create an initial ring with marble 0
initRing = Ring [0] []

lengthOfRing :: Ring -> Int
lengthOfRing r = (length $ counterclockwise r) + 1 + (length $ clockwise r)

moveClockwise :: Int -> Ring -> Ring
moveClockwise 0 r = r
moveClockwise amount (Ring [] []) = Ring [] []
moveClockwise amount (Ring ccw []) =
    let newCw = reverse ccw in
    moveClockwise amount (Ring [] newCw)
moveClockwise amount (Ring ccw (newCurr:newCw)) = moveClockwise (amount - 1) (Ring (newCurr:ccw) newCw)

moveCounterclockwise :: Int -> Ring -> Ring
moveCounterclockwise 0 r = r
moveCounterclockwise amount (Ring [] []) = Ring [] []
moveCounterclockwise amount (Ring [] cw) =
    let newCcw = reverse cw in
    moveCounterclockwise amount (Ring newCcw [])
moveCounterclockwise amount (Ring (newCurr:newCcw) cw) = moveCounterclockwise (amount - 1) (Ring newCcw (newCurr:cw))

-- insert new value 'after' (clockwise from) current value
insertAfter :: Int -> Ring -> Ring
insertAfter value (Ring ccw cw) = Ring ccw (value:cw)

insertBefore :: Int -> Ring -> Ring
insertBefore value (Ring ccw cw) = Ring (value:ccw) cw

deleteAfter :: Ring -> (Ring, Int)
deleteAfter r =
    let Ring ccw (x:cw) = if List.null $ clockwise r
        then Ring [] (reverse $ counterclockwise r)
        else r
    in (Ring ccw cw, x)

playNormalMarble :: Int -> Ring -> Ring
playNormalMarble value r =
    let newPos = moveClockwise 1 r in
    insertBefore value newPos

-- Remove the proper marble and return its value
playSpecialMarble :: Ring -> (Ring, Int)
playSpecialMarble r =
    let newPos = moveCounterclockwise (7 + 1) r
        (afterDel, score) = deleteAfter newPos in
    (moveClockwise 1 afterDel, score)

-- Play the marble with the given value, returning modified ring and the scored value (if any)
playMarble :: Int -> Ring -> (Ring, Int)
playMarble value r =
    case value `mod` 23 of
        0 -> let (newR, scored) = playSpecialMarble r in (newR, scored + value)
        _ -> let newR = playNormalMarble value r in (newR, 0)

scoreMarble :: (Ring, Map.Map Int Int) -> Int -> (Ring, Map.Map Int Int)
scoreMarble (r, scoreMap) value =
    let (newR, score) = playMarble value r
        player = value `mod` numPlayers
        newScoreMap = Map.insertWith (+) player score scoreMap in
    (newR, newScoreMap)

numPlayers = 459
lastMarble = 7210300
--numPlayers = 9
--lastMarble = 25
--numPlayers = 13
--lastMarble = 7999

main = do
    let (ring, pointMap) = List.foldl scoreMarble (initRing, Map.empty) [1..lastMarble]
        pointList = Map.toList pointMap
        maxPoint = List.maximumBy (comparing snd) pointList
    print $ snd maxPoint
