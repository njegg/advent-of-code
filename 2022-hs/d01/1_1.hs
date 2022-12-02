module Main where

main :: IO ()
main = do
    input <- readFile "./1input.txt"
    print $ foldl max 0 $ splitSum $ lines input

splitSum :: [String] -> [Integer] 
splitSum = foldr (\n (a:as) -> if null n then 0 : a : as else (a + read n) : as) [0]

