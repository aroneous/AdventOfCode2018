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

main = do
    contents <- getContents
    let collapsed = collapseAll contents
        len = length collapsed
    print len
