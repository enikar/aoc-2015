-- AoC 2015, day 2

{- HLINT ignore "Eta reduce" -}

module Main (main) where

import Data.List (sort, foldl')
import Data.Char (isDigit)
import Text.ParserCombinators.ReadP
  (ReadP
  ,readP_to_S
  ,char
  ,sepBy1
  ,munch1
  ,optional
  ,eof
  )

getDatas :: IO [[Int]]
getDatas = parseDatas <$> readFile "day2.txt"

printSolution :: Show a => String -> a -> IO ()
printSolution part x = putStrLn (part <> ": " <> show x)

main :: IO ()
main = do
  boxes <- getDatas
  printSolution "Part1" (part1 boxes)
  printSolution "Part2" (part2 boxes)

part1 :: [[Int]] -> Int
part1 boxes = foldl' f 0 boxes
  where
    f acc box = acc + dimension box + product (take 2 box)

    dimension :: [Int] -> Int
    dimension [x, y, z] = 2 * x * y + 2 * y * z + 2 * x * z
    dimension [] = error "Error: dimension: empty box"
    dimension _  = error "Error: bad dimension for a box"

part2 :: [[Int]] -> Int
part2 boxes = foldl' f 0 boxes
  where
    f acc box = acc + product box + ribbon
      where ribbon = sum (map (*2) (take 2 box))


-- parsing stuff
positive :: ReadP Int
positive = read <$> munch1 isDigit

parseDatas :: String -> [[Int]]
parseDatas str =
  case readP_to_S readDatas str of
    [(datas, "")] -> datas
    []  -> error "Error: parseDatas: can't parse."
    _   -> error "Error: parseDatas: there are more than one result."

readDatas :: ReadP [[Int]]
readDatas = do
  boxes <- sepBy1 readBox (char '\n')
  optional (char '\n')
  eof
  pure boxes

readBox :: ReadP [Int]
readBox = sort <$> sepBy1 positive (char 'x')
