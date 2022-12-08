module Main where

--- Day 8: Treetop Tree House, Part 2 ---

import Data.Char (digitToInt)
import Data.List (transpose)

main :: IO ()
main = do
    input <- readFile "./8_i.txt"
    let parse = map (map digitToInt) $ lines input
    let n = length parse
    
    let rows = concat $ checkRows parse
    let cols = concat $ transpose $ checkRows (transpose parse)

    print $ maximum $ zipWith (*) rows cols

checkRows :: [[Int]] -> [[Int]]
checkRows = map ((\r -> map (scenicRow r) r) . zip [0..])

scenicRow :: [(Int, Int)] -> (Int, Int) -> Int
scenicRow row (i,h) = (\(l, r) -> look (reverse l) h * look (drop 1 r) h) $ splitAt i row

look :: [(Int, Int)] -> Int -> Int
look [] h = 0
look (x:xs) h
    | snd x <  h = 1 + look xs h
    | otherwise  = 1

