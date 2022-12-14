module Main where
 
--- Day 4: Camp Cleanup, Part 2 ---

import Data.List (intersect)


split :: Char -> String -> [String]
split d = foldr (\c (a:as) -> if c/=d then (c:a):as else []:a:as) [[]]

checkPair :: [[Int]] -> Int
checkPair (p1:p2:_)
    | null $ intersect [p1!!0..p1!!1] [p2!!0..p2!!1] = 0
    | otherwise                                      = 1

stoi :: String -> Int
stoi s = read s :: Int

main :: IO ()
main = do
    input <- readFile "./4_i.txt"
    let parse = map (map (map stoi . split '-') . split ',') $ lines input
    print $ sum $ map checkPair parse

