main = print $ replaceTile myMap 1 1 Forest
  where
    myMap = Map [[Blank, Blank, Blank, Blank],[Blank, Blank, Blank, Blank],[Blank, Blank, Blank, Blank],[Blank, Blank, Blank, Blank]]

data Map = Map [[Tile]]

instance Show Map where
  show (Map ((b:[]):[])) = (show b)
  show (Map ((b:[]):as)) = (show b) ++ "\n" ++ (show (Map as))
  show (Map ((b:bs):as)) = (show b) ++ (show (Map (bs:as)))

data Tile = Path | Forest | Blank

instance Show Tile where
  show Path = "p"
  show Forest = "f"
  show Blank = "b"

makePath :: Map -> Int -> Int -> Map
makePath (Map tiles) x y = (Map tiles)

--replace :: Map -> Int -> Int -> Tile -> Map
--replace (Map a:as) x y new | (y == (length as)) = ((fst split) ++ [new] ++ (tail (snd split))):as
--  where
--    split = splitAt x a
--replace (Map (a:as)) x y new = a:(replace (Map as) x y new)


--replace :: Map -> Int -> Int -> Tile -> Map
--replace (Map tiles) x y tile = Map ((fst split_rows) ++ ((fst split_row) ++ [tile] ++ (tail (snd split_row))) ++ (tail (snd split_rows)))
--  where
--    split_row = splitAt x (head (snd split_rows))
--    split_rows = splitAt y tiles

replace :: [a] -> Int -> a -> [a]
replace [] _ _ = []
replace (a:as) i new
  | i == (length as) = new:as
  | otherwise = a:(replace as i new)

replaceTile :: Map -> Int -> Int -> Tile -> Map
replaceTile (Map tiles) x y tile = Map (replace tiles y (replace (tiles!!x) x tile))
