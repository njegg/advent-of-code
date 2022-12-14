module Main where

import Data.Text (splitOn)
import Data.List (sortBy)

main :: IO ()
main = do
    input <- readFile "./1_i.txt"
    print $ sum $ take 3 $ sortBy (flip compare)  $ splitSum $ lines input

splitSum :: [String] -> [Integer] 
splitSum = foldr (\n (a:as) -> if null n then 0 : a : as else (a + read n) : as) [0]

