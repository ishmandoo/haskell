main = print myMap
  where
    myMap = Map [[Blank, Forest, Blank, Blank],[Blank, Blank, Blank, Blank],[Blank, Blank, Blank, Blank],[Blank, Blank, Blank, Blank]]

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

makePath :: Map -> Map
