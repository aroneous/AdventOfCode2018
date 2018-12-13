import Data.Map (Map, insert, fromList, (!))
import qualified Data.Set as Set
import qualified Data.List as List
import qualified Data.List.Split as Split
import qualified Data.ByteString as B
import Data.Monoid (mappend)
import Data.Char
import Data.Ord
import Data.Word
import Codec.BMP

parseInitial :: String -> [Bool]
parseInitial [] = []
parseInitial ['\n'] = []
parseInitial (x:xs) =
    let this = case x of
                '#' -> True
                '.' -> False
                huh -> error (huh:[])
    in this : parseInitial xs

cellVal :: Char -> Bool
cellVal '#' = True
cellVal '.' = False

parseNote :: String -> ([Bool], Bool)
parseNote (a:b:c:d:e:rest) =
    let ctx = map cellVal [a,b,c,d,e]
        res = cellVal $ rest !! 4
    in (ctx, res)

calcGeneration :: Map [Bool] Bool -> [Bool] -> [Bool]
calcGeneration notes (a:b:c:d:e:xs) =
    let newVal = notes ! [a,b,c,d,e]
    in newVal : calcGeneration notes (b:c:d:e:xs)
calcGeneration _ _ = []

-- Remove leading False, and return number of leading false removed
prune :: [Bool] -> ([Bool], Int)
prune [] = ([], 0)
prune (True:xs) = (True:xs, 0)
prune (False:xs) = let (res, count) = prune xs in (res, count+1)

doGeneration :: Map [Bool] Bool -> Int -> [Bool] -> (Int, [Bool])
doGeneration notes startIdx state = 
    let paddedStartIdx = startIdx - 1
        iterated = calcGeneration notes (threeFalse ++ state ++ threeFalse)
        (pruned, numRemoved) = prune iterated
    in (paddedStartIdx + numRemoved, pruned)

threeFalse = [False, False, False]

cycles = 3000

main = do
    contents <- getContents
    let l = lines contents
        init = parseInitial $ drop 15 (l !! 0)
        paddedInit = [False, False] ++ init ++ [False, False]
        notes = drop 2 l
        nm = fromList $ map (\note -> parseNote note) notes
        (start, evolved) = List.foldl (\(startIdx, state) _ -> doGeneration nm startIdx state) (0, paddedInit) [1..cycles]
        answer = sum $ map (\(val, idx) -> if val == True then idx else 0) (zip evolved [(start-2)..])
        numPlants = sum $ map (\val -> if val == True then 1 else 0) evolved
        -- Experimentation shows we reach stable state by now, but the plants keep shifting to the right by one spot each
        -- cycle. Maintain the same set of filled pots, calculate the starting index that would be associated with
        -- the 50 billionth generation, and sum up the resulting indices
        offset = cycles - start
        bigStart = 50000000000 - (fromIntegral (offset + 2))
        bigAnswer = sum $ map (\(val, idx) -> if val == True then idx else 0) (zip evolved [bigStart..])
--    print paddedInit
--    print evolved
    print cycles
    print answer
    print numPlants
    print start
    print bigAnswer
