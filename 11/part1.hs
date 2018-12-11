import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as List
import qualified Data.List.Split as Split
import Data.Monoid (mappend)
import Data.Char
import Data.Ord

-- Map from (x,y) to power level
type Grid = Map.Map (Int, Int) Int

powerLevel :: Int -> Int -> Int -> Int
powerLevel x y sn =
    let rackID = x + 10
        step1 = rackID * y
        step2 = step1 + sn
        step3 = step2 * rackID
        step4 = (step3 `div` 100) `mod` 10
        step5 = step4 - 5
    in step5

makeGrid :: Int -> Grid
makeGrid sn =
    Map.fromList $ map (\(x, y) -> ((x, y), powerLevel x y sn)) [(x,y) | y <- [1..300], x <- [1..300]]

subgrid :: (Int, Int) -> Grid -> Int
subgrid (x,y) grid = sum $ map (\coord -> grid Map.! coord) [(x',y') | y' <- [y..(y+2)], x' <- [x..(x+2)]]

maxSubgrid :: Grid -> ((Int, Int), Int)
maxSubgrid grid = List.maximumBy (comparing snd) $ map (\coord -> (coord, subgrid coord grid)) [(x,y) | y <- [1..298], x <- [1..298]] 

main = do
    let sn = 3628 -- Puzzle input
        grid = makeGrid sn
        max = maxSubgrid grid
    print max
