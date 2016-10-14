main = print $ makePath myMap 1 3
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
makePath (Map tiles) x y
  | (y == 0) = (replaceTile (Map tiles) x y Path)
  | otherwise = makePath (replaceTile (Map tiles) x y Path) x (y-1)

replace :: [a] -> Int -> a -> [a]
replace [] _ _ = []
replace (a:as) i new
  | i == (length as) = new:as
  | otherwise = a:(replace as i new)

replaceTile :: Map -> Int -> Int -> Tile -> Map
replaceTile (Map tiles) x y tile = Map (replace tiles y (replace (tiles!!x) x tile))
