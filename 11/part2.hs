import qualified Data.Map as Map
import Data.Map ((!))
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

makePrefixSums :: Grid -> Grid
makePrefixSums grid =
        -- Add 0s in row and column 0
    let zeros = List.foldl (\premap coord -> Map.insert coord 0 premap) Map.empty
            ([(0,y) | y <- [1..300]] ++ [(x,0) | x <- [0..300]])
        -- Sum columns down
        cols = List.foldl (\pregrid (x, y) -> Map.insert (x,y) ((pregrid ! (x,y-1)) + (grid ! (x,y))) pregrid)
            zeros [(x,y) | y <- [1..300], x <- [1..300]]
        -- Sum column sums across
        both = List.foldl (\pregrid (x, y) -> Map.insert (x,y) ((pregrid ! (x-1,y)) + (cols ! (x,y))) pregrid)
            zeros [(x,y) | x <- [1..300], y <- [1..300]]
    in both

-- Calculate subgrid sum from prefix sum grid: bottom right corner minus the portions of the grid left and above
subgrid :: (Int, Int, Int) -> Grid -> Int
subgrid (x,y,size) prefix =
    let right = x + size - 1
        bottom = y + size - 1
    in prefix ! (right, bottom) - (prefix ! (x-1,bottom)) - (prefix ! (right, y-1)) + (prefix ! (x-1, y-1))

maxSubgrid :: Grid -> Int -> ((Int, Int, Int), Int)
maxSubgrid prefix size = List.maximumBy (comparing snd) $ map (\coord -> (coord, subgrid coord prefix))
    [(x,y,size) | y <- [1..300-size+1], x <- [1..300-size+1]] 

main = do
    let sn = 3628 -- Puzzle input
        grid = makeGrid sn
        prefix = makePrefixSums grid
        max = List.maximumBy (comparing snd) $ map (maxSubgrid prefix) [1..300]
--        max = maxSubgrid grid
--    sequence $ map (\size -> print (maxSubgrid prefix size)) [1..300]
    print max
