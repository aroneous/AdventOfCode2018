toNum s = let op = head s
              val = read (tail s) :: Integer in
              if op == '-'
                then -val
                else val

main = do
    contents <- getContents
    let l = lines contents
        nums = map toNum l
        res = foldl (+) 0 nums
    print res
