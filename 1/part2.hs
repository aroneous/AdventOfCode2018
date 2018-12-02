import qualified Data.Set as Set

toNum s = let op = head s
              val = read (tail s) :: Integer in
              if op == '-'
                then -val
                else val

data Res a = Ans a | Nope (Set.Set a)
checkSum :: Ord a => Set.Set a -> a -> Res a
checkSum set sum = if Set.member sum set
    then Ans sum
    else Nope (Set.insert sum set)

findRepeat sum set nums = let newsum = sum + (head nums) in
    case checkSum set (sum + (head nums)) of
        Ans a -> a
        Nope newset -> findRepeat newsum newset (tail nums)

main = do
    contents <- getContents
    let l = lines contents
        nums = map toNum l
        res = findRepeat 0 Set.empty (cycle nums)
    print res
