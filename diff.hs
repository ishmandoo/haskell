main = print $ (iterate (step 0.01) region) !! 100000
  where
    region = [(Cell 1 1),(Cell 2 1),(Cell 3 1),(Cell 4 1),(Cell 5 1)]

data Cell = Cell Float Float deriving Show

step :: Float -> [Cell] -> [Cell]
step dt ((Cell d c):[]) = [(Cell d c)]
step dt ((Cell d1 c1):((Cell d2 c2):as)) = (Cell d1 (c1 + dc)):(step dt ((Cell d2 (c2 - dc)):as))
  where
    dc = (dt * d1 * (c2 - c1))
