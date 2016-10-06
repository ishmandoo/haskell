main = print $ transpose [[1,2,3],[3,4,5]]

getVecN ::  [[a]] -> Int -> [a]
getVecN mat i = map (getElem i) mat

getElem :: Int -> [a] -> a
getElem 0 (a:as) = a
getElem i (a:as) = getElem (i-1) as

transpose :: [[a]] -> [[a]]
transpose mat = map (getVecN mat) [0..(length (head mat))-1]
