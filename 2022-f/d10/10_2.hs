module Main where

import Data.Char (isLetter)


--- Day 10: Cathode-Ray Tube, Part 2 ---


main :: IO ()
main = do
    input <- readFile "./10_i.txt"

    let display = reverse . snd . foldl (\(x, s) (c, i) -> instruction i (mod c 40, x, s) ) (1, "")  $ zip [0..] $ words input
    putStrLn $ split display 40 

split :: String -> Int -> String
split [] _ = []
split s n = (\(l,r) -> l ++ ['\n'] ++ split r n) $ splitAt n s

instruction :: String -> (Int, Int, String) -> (Int, String)
instruction i (px, x, s)
    | abs (px - x) < 2 = (maybeAdd i x, '#':s)
    | otherwise = (maybeAdd i x, '.':s)

maybeAdd :: String -> Int -> Int
maybeAdd i x
    | isLetter $ head i = x
    | otherwise = x + stoi i

stoi :: String -> Int
stoi s = read s :: Int
