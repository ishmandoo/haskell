main = print $ (run rule30 (World [Alive]) 100)

data Cell = Dead | Alive

data World = World [Cell]

data Timeline = Timeline [World]

instance Show Cell where
    show Dead = "o"
    show Alive = "x"

instance Show World where
    show (World (a:[])) = (show a)
    show (World (a:as)) = (show a) ++ (show (World as))

instance Show Timeline where
    show (Timeline (a:[])) = (show a)
    show (Timeline (a:as)) = (show a) ++ "\n" ++ (show (Timeline as)) where


stringPad :: Int -> String
stringPad n = (concat (replicate n " "))

padList :: a -> [a] -> [a]
padList pad as = pad:pad:as


tripleList :: a -> [a] -> [[a]]
tripleList pad (a1:a2:a3:[]) = [[a1, a2, a3],[a2, a3, pad], [a3, pad, pad]]
tripleList pad (a1:a2:a3:as) = [a1, a2, a3]:(tripleList pad (a2:a3:as))

padCellList = padList Dead
tripleCellList = tripleList Dead

rule30 :: [Cell] -> Cell
rule30 [Alive, Alive, Alive] = Dead
rule30 [Alive, Alive, Dead] = Dead
rule30 [Alive, Dead, Alive] = Dead
rule30 [Alive, Dead, Dead] = Alive
rule30 [Dead, Alive, Alive] = Alive
rule30 [Dead, Alive, Dead] = Alive
rule30 [Dead, Dead, Alive] = Alive
rule30 [Dead, Dead, Dead] = Dead

step :: ([Cell] -> Cell) -> World -> World
step rule (World cells) = World (map rule (tripleCellList (padCellList cells)))

run :: ([Cell] -> Cell) -> World -> Int -> Timeline
run rule world n = Timeline (take n $ (iterate (step rule) world))