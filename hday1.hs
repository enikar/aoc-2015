-- Aoc 2015 day1
{- HLINT ignore "Eta reduce" -}

module Main(main) where
import Data.List (foldl')

readDatas :: IO String
readDatas = readFile "day1.txt"

printSolution :: Show a => String -> a -> IO ()
printSolution part x = putStrLn (part <> ": " <> show x)

main :: IO ()
main = do
  datas <- readDatas
  printSolution "Part1" (part1 datas)
  printSolution "Part2" (part2' datas)


nextFloor :: Int -> Char -> Int
nextFloor n c
  | c == '('  = n+1
  | otherwise = n-1

part1 :: String -> Int
part1 datas = foldl' f 0 datas
  where
    f acc x = nextFloor acc x

-- Here the pair (Int, Bool) plays two role
-- First the Int is floor while the Bool is False
-- Then when the floor == -1, Int becomes the index
-- in the list datas (count from 1), and the Bool is True
-- This is somewhat ugly.
-- One way to correct this is to use an Either
part2 :: String -> Int
part2 datas = fst (foldl' f (0, False) (zip [1..] datas))
  where
    f (pos, True) _        = (pos, True)
    f (n, False)  (pos, x)
      | n' == -1           = (pos, True)
      | otherwise          = (n', False)
        where n' = nextFloor n x

part2' :: String -> Int
part2' datas =
  case foldl' f (Left 0) (zip [1..] datas) of
    Left _ -> error "Error: part2 can't find the index"
    Right n -> n
  where
    f acc@(Right _) _  = acc
    f (Left n) (pos, x)
      | n' == -1   = Right pos
      | otherwise  = Left n'
      where n' = nextFloor n x
