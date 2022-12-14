module Main where

import Data.Char (isLetter)


--- Day 10: Cathode-Ray Tube, Part 1 ---


main :: IO ()
main = do
    input <- readFile "./10_i.txt"
    print $ sum . snd $ foldl (\(x,s) (c,i) -> instruction i (c, x, s)) (1, []) $ zip [1..] $ words input

instruction :: String -> (Int, Int, [Int]) -> (Int, [Int])
instruction i (c, x, s) = (\(dx, ds) -> if isLetter $ head i then (dx,ds) else (dx + stoi i, ds)) $ if c `elem` [20,60..220] then (x, (x*c):s) else (x, s)

stoi :: String -> Int
stoi s = read s :: Int

