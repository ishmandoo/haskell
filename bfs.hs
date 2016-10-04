main = print $ bfs 0 node0
  where
    node0 = Node "node0" [node1] Nothing
    node1 = Node "node1" [] Nothing
    --node0 = Node "node0" [node1, node2, node4] Nothing
    --node1 = Node "node1" [node0, node3] Nothing
    --node2 = Node "node2" [node0, node5] Nothing
    --node3 = Node "node3" [node1] Nothing
    --node4 = Node "node4" [node0] Nothing
    --node5 = Node "node5" [node2] Nothing

data Node = Node String [Node] (Maybe Int)

instance Show Node where
  show (Node name [] _) =
    name
  show (Node name adj _) =
    name ++ concat (map show adj)

bfs :: Int -> Node -> Node
bfs i (Node name adj Nothing) =
  (Node name (map (bfs (i+1)) adj) (Just i))
bfs _ (Node name adj (Just i)) =
  (Node name adj (Just i))
