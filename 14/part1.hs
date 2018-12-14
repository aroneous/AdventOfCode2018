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
    d2 :: Int
}

initState = State (S.fromList [3, 7]) 0 1

makeRecipe :: State -> State
makeRecipe (State r d1 d2) =
    let dv1 = r `index` d1
        dv2 = r `index` d2
        nr = dv1 + dv2
        r' = if nr < 10 then r |> nr else (r |> (nr `div` 10)) |> (nr `mod` 10)
        len = S.length r'
        d1' = (d1 + dv1 + 1) `mod` len
        d2' = (d2 + dv2 + 1) `mod` len
    in State r' d1' d2'

makeNRecipes :: Int -> State
makeNRecipes n =
    let initCount = length $ r initState
        toMake = n - initCount
        state = iterate makeRecipe initState !! toMake
    in state

input = 824501

main = do
    let state = makeNRecipes (input + 10)
        recipes = r state
        answer = S.take 10 $ S.drop input recipes
        answerString = (map show $ toList answer) >>= (\a -> a)

    putStrLn answerString
