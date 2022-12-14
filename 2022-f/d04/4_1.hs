module Main where

--- Day 4: Camp Cleanup Part 1 ---

split :: Char -> String -> [String]
split d = foldr (\c (a:as) -> if c/=d then (c:a):as else []:a:as) [[]]

stoi :: String -> Int
stoi s = read s :: Int

checkPair :: [[Int]] -> Int
checkPair ((x1:x2:_):(y1:y2:_):_)
    | x1 >= y1 && x2 <= y2 || x1 <= y1 && x2 >= y2 = 1
    | otherwise                                    = 0

main :: IO ()
main = do
    input <- readFile "./4_i.txt"
    let parse = map (map (map stoi . split '-') . split ',') $ lines input
    print $ sum $ map checkPair parse

