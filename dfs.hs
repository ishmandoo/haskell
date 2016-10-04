main = print $ dfs node0
  where
    node0 = Node "node0" [node1] Nothing
    node1 = Node "node1" [] Nothing
    --node0 = Node "node0" [node1, node2, node4] Nothing
    --node1 = Node "node1" [node0, node3] Nothing
    --node2 = Node "node2" [node0, node5] Nothing
    --node3 = Node "node3" [node1] Nothing
    --node4 = Node "node4" [node0] Nothing
    --node5 = Node "node5" [node2] Nothing

data Node = Node String [Node] (Maybe Node) (Maybe Int)

instance Show Node where
  show (Node name [] _) =
    name
  show (Node name adj _) =
    name ++ concat (map show adj)

dfs :: Node -> String -> String
dfs (Node name adj parent) target
  | (name == target) = name (Node name adj parent)
  | otherwise dfs (Node name )

name :: Node -> String
name (Node _ _ Nothing) = name
name (Node name adj parent) = name ++ " " ++ (name parent)
