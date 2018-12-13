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

fourFalse = [False, False, False, False]

main = do
    contents <- getContents
    let l = lines contents
        init = parseInitial $ drop 15 (l !! 0)
        paddedInit = [False, False] ++ init ++ [False, False]
        notes = drop 2 l
        nm = fromList $ map (\note -> parseNote note) notes
        (start, evolved) = List.foldl (\(startIdx, state) _ -> (startIdx - 2, calcGeneration nm (fourFalse ++ state ++ fourFalse))) (0, paddedInit) [1..20]
        answer = sum $ map (\(val, idx) -> if val == True then idx else 0) (zip evolved [(start-2)..])
--    print paddedInit
--    print evolved
    print answer
