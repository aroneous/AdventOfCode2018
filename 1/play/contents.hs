toNum s = let op = head s
              val = read (tail s) :: Integer in
              if op == '-'
                then -val
                else val

main = do
    contents <- getContents
    let l = lines contents
--        tails = map tail l
--        res = unlines tails
        nums = map toNum l
        res = foldl (+) 0 nums
--    let res = unlines map tail lines contents
    print res
