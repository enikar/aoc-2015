-- AoC 2015, day 2

{- HLINT ignore "Eta reduce" -}

import Data.Set qualified as S

import Data.List (foldl')
import Data.List.Extra (chunksOf)

getDatas :: IO String
getDatas = concat . lines <$> readFile "day3.txt"

printSolution :: Show a => String -> a -> IO ()
printSolution part x = putStrLn (part <> ": " <> show x)

main :: IO ()
main = do
  datas <- getDatas
  printSolution "Part1" (part1 datas)
  printSolution "Part2" (part2 datas)

move :: (Int, Int) -> Char -> (Int, Int)
move (x, y) '^' = (x, y+1)
move (x, y) 'v' = (x, y-1)
move (x, y) '>' = (x+1, y)
move (x, y) '<' = (x-1, y)
move _       _  = error "Error: move: not a possible move"

part1 :: String -> Int
part1 s = (S.size . fst) (foldl' f (S.singleton (0,0), (0,0)) s)
     where
       f (positions, pos) c = (S.insert pos' positions, pos')
         where
           pos' = move pos c

part2 :: String -> Int
part2 s = (S.size . first) (foldl'
                           f
                           (S.singleton (0,0), (0,0), (0,0))
                         (chunksOf 2 s))
  where
    first (x, _, _) = x

    f (positions, pos, rpos) [c,d] =
        let santaPos = move pos c
            robotPos = move rpos d
            positions' = S.insert santaPos positions
            positions'' = S.insert robotPos positions'
        in
          (positions'', santaPos, robotPos)
    f _ _ = error "Error: part2"
