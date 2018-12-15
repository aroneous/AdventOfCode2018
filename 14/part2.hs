import qualified Data.List as List
import qualified Data.List.Split as Split
import qualified Data.Sequence as S
import Data.Sequence (Seq, (|>), index)
import Data.Monoid (mappend)
import Data.Foldable (toList)
import Data.Char
import Data.Ord
import Debug.Trace (trace)

data State = State {
    r :: Seq Int,
    d1 :: Int,
    d2 :: Int,
    t :: Seq Int
}

initState = State (S.fromList [3, 7]) 0 1 (S.empty)

addAndCheck :: Seq Int -> Int -> Seq Int -> Seq Int -> (Seq Int, Seq Int, Bool)
addAndCheck goal nr r t =
    let r' = r |> nr
        t' = t |> nr
        extra = S.length t' - length goal
        t'' = if extra > 0 then S.drop extra t' else t'
        match = goal == t''
    in (r', t'', match)

-- Bool result is if a match occurs. Bool param unused, but added to support signature of iterate
makeRecipe :: Seq Int -> (State, Bool) -> (State, Bool)
makeRecipe goal ((State r d1 d2 t), _) =
    let dv1 = r `index` d1
        dv2 = r `index` d2
        nr = dv1 + dv2
        (r', t', match) = if nr < 10 then (r, t, False) else addAndCheck goal (nr `div` 10) r t
        (r'', t'', match') = if match then (r', t', match) else addAndCheck goal (nr `mod` 10) r' t'
        --r' = if nr < 10 then r |> nr else (r |> (nr `div` 10)) |> (nr `mod` 10)
        --t' = if nr < 10 then t |> nr else (t |> (nr `div` 10)) |> (nr `mod` 10)
        --extra = S.length t' - tailLen - 1
        --t'' = if extra > 0 then S.drop extra t' else t'
        len = S.length r''
        d1' = (d1 + dv1 + 1) `mod` len
        d2' = (d2 + dv2 + 1) `mod` len
    in (State r'' d1' d2' t'', match')

checkMatch :: Seq Int -> Seq Int -> Bool
checkMatch goal tail =
    S.drop 1 tail == goal || S.take (S.length goal) tail == goal

findPattern :: Seq Int -> State
findPattern goal =
    --let state = until (\s -> t s == goal) (makeRecipe (S.length goal)) initState
    --let state = until (\s -> checkMatch goal (t s)) (makeRecipe (S.length goal)) initState
    let (state, _) = until (\(s,match) -> match || S.length (r s) > 30000000) (makeRecipe goal) (initState, False)
    in state

intToList :: Int -> [Int]
intToList 0 = []
intToList i = i `mod` 10 : intToList (i `div` 10)

input = 824501
--input = 59414

main = do
    let inSeq = S.fromList (reverse $ intToList input)
        goalState = findPattern inSeq
        numBefore = S.length (r goalState) - S.length inSeq
    print numBefore
        --state = makeNRecipes (input + 10)
        --recipes = r state
        --answer = S.take 10 $ S.drop input recipes
        --answerString = (map show $ toList answer) >>= (\a -> a)

    --putStrLn answerString
