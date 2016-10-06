import Control.Concurrent
import System.Console.ANSI
main = animate $ map (printArena) (iterate stepArena arena)
  where
    ball = Ball 0 0 3 2
    arena = Arena 99 51 ball

repeatn :: Char -> Int -> String
repeatn a n = take n (repeat a)

data Ball = Ball Int Int Int Int
data Arena = Arena Int Int Ball


printArena :: Arena -> String
printArena (Arena w h (Ball x y dx dy)) =
  repeatn '-' (w+2) ++ "\n" ++ (printArenaRow w h (Ball x y dx dy))

printArenaRow :: Int -> Int -> Ball-> String
printArenaRow w 0  _=
  repeatn '-' (w+2) ++ "\n"
printArenaRow w h (Ball x y dx dy)=
  "|" ++ (repeatn ' ' (x)) ++ b ++ (repeatn ' ' (w-x-1)) ++ "|\n" ++ (printArenaRow w (h-1) (Ball x y dx dy))
    where
      b = if ((h-1) == y) then "o" else " "

stepArena :: Arena -> Arena
stepArena (Arena w h (Ball x y dx dy)) =
  (Arena w h (Ball x' y' dx' dy'))
    where
      x' = max (min (x + dx) (w-1)) 0
      y' = max (min (y + dy) (h-1)) 0
      dx' = if ((x' == 0) || (x' == (w-1))) then -dx else dx
      dy' = if ((y' == 0) || (y' == (h-1))) then -dy else dy

multiply :: (a -> a) -> Int -> (a -> a)
multiply f 0 = f
multiply f n = f.(multiply f (n-1))

animate :: [String] -> IO ()
animate [] =
  putStr "done"
animate (a:as) =
  do
    putStr a
    threadDelay 100000
    clearScreen
    animate as
