module Main where

--- Day 8: Treetop Tree House, Part 1 ---

import Data.Char (digitToInt)
import Data.List (transpose)

main :: IO ()
main = do
    input <- readFile "./8_i.txt"
    let parse = map (map digitToInt) $ lines input
    let n = length parse
    print parse
    
    let rows = concat $ replicate n True : checkRows (middle parse) ++ [replicate n True]
    let cols = concat $ transpose $ replicate n True : checkRows (middle $ transpose parse) ++ [replicate n True]

    print $ sum $ map fromEnum $ zipWith (||) rows cols


middle :: [a] -> [a]
middle l = drop 1 $ init l

checkRows :: [[Int]] -> [[Bool]]
checkRows = map ((\r -> map (visibleInRow r) r) . zip [0..])

visibleInRow :: [(Int, Int)] -> (Int, Int) -> Bool
visibleInRow row (i,h) 
    | i /= 0 && i /= length row - 1 =  (\(l, r) -> higherThanAll l h || higherThanAll (drop 1 r) h) $ splitAt i row
    | otherwise = True

higherThanAll :: [(Int, Int)] -> Int -> Bool
higherThanAll [] _ = True
higherThanAll r t = t > maximum (map snd r)

