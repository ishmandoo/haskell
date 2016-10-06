
data Node = Node Int (Maybe Node) (Maybe Node)

insert :: Node -> Node
insert (Node val (Just left) (Just right)) newVal
  | (newVal > val) = (Node val (Just left) (insert right val))
  | otherwise = (Node val (insert left newVal) (Just right))
insert (Node val Nothing (Just right))
  | (newVal > val) = (Node val Nothing (insert right newVal))
  | otherwise = (Node val (Node newVal Nothing Nothing) (Just right))
insert (Node val (Just left) Nothing)
  | (newVal > val) = (Node val (Just left) (Node newVal Nothing Nothing))
  | otherwise = (Node val (insert left newVal) Nothing)
insert (Node val Nothing Nothing)
  | (newVal > val) = (Node val Nothing (Node newVal Nothing Nothing))
  | otherwise = (Node val (Node newVal Nothing Nothing) Nothing)


  data Tree = Node' Int Tree Tree | Nil
