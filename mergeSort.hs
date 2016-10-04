

split :: [a] -> ([a], [a])
split arr = (take half arr, take otherHalf (reverse arr))
  where
    half = div (length arr) 2
    otherHalf = length arr - half

merge :: (Ord a) => ([a], [a]) -> [a]
merge ([], []) = []
merge (a:as, []) = (a:(merge (as, [])))
merge ([], b:bs) = (b:(merge ([], bs)))
merge (a:as, b:bs)
  | (a > b) = (b:(merge (a:as, bs)))
  | otherwise = (a:(merge (as, b:bs)))
