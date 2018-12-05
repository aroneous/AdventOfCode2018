import qualified Data.Set as Set
import Data.Char
import Data.Bits

collapse :: String -> String
collapse [] = []
collapse ['\n'] = [] -- Hacky way to remove trailing EOL
collapse [x] = [x]
collapse (x:y:xs) = if ord x `xor` ord y == 0x20
    then collapse xs
    else (x:collapse(y:xs))

collapseAll :: String -> String
collapseAll s = let s' = collapse s in
    if s == s'
        then s'
        else collapseAll s'

removeChar :: Char -> String -> String
removeChar c [] = []
removeChar c (x:xs) = if (ord x) .|. 0x20 == (ord c) then removeChar c xs else (x:removeChar c xs)

unitTypesInt :: Set.Set Char -> String -> Set.Set Char
unitTypesInt set [] = set
unitTypesInt set (x:xs) = unitTypesInt (Set.insert (toLower x) set) xs

unitTypes :: String -> Set.Set Char
unitTypes s = unitTypesInt Set.empty s

main = do
    contents <- getContents
    let stripped = init contents -- Remove last character, which is newline
        types = Set.toList (unitTypes stripped)
        processed = map (\t -> collapseAll (removeChar t stripped)) types
        --processed = map (\t -> removeChar t stripped) types
        lengths = map length processed
        shortest = minimum lengths
--        collapsed = collapseAll stripped
--        len = length collapsed
    print shortest
