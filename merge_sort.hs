main = print "done"

mergeSort :: Ord a => [a] -> [a]
mergeSort (a:[]) = [a]
mergeSort arr = merge (split arr)

merge :: Ord a => ([a], [a]) -> [a]
merge ([], a:[]) = [a]
merge (b:[], []) = [b]
merge ([], b:bs) = b:(merge ([], bs))
merge (a:as, []) = a:(merge ([], as))
merge (a:as, b:bs) = if a < b
  then a:(merge (as, b:bs))
  else b:(merge (a:as, bs))


split :: [a] -> ([a],[a])
split arr = (take (half arr) arr, take ((half arr)+1) (reverse arr))

half :: [a] -> Int
half arr = floor ((fromIntegral(length arr))/2)
