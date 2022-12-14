module Main where

--- Day 5: Supply Stacks, Part 2 ---

import Data.List (transpose)
import Data.Char (isLetter)

readMap :: String -> String
readMap [] = []
readMap s = (\(a,b) ->  (a !! 1) : readMap b) (take 3 s, drop 4 s)

stoi :: String -> Int
stoi s = read s :: Int 

remove :: [String] -> Int -> Int -> ([String],String)
remove (x:xs) n i
    | i /= 1    = (x : fst res, snd res)
    | otherwise = (drop n x : xs, take n x)
    where
        res = remove xs n (i - 1) 

add :: [String] -> String -> Int -> [String]
add (x:xs) a i
    | i /= 1    = x : add xs a (i - 1)
    | otherwise = (reverse a ++ x) : xs

move :: [String] -> Int -> Int -> Int -> [String]
move s n i j = (\(ns, d) -> add ns d j) $ remove s n i

main :: IO ()
main = do
    input <- readFile "./5_s.txt"
    let parse = break ((== '1') . (!! 1)) $ lines input
    let crateMap = map (filter (/= ' ')) $ transpose $ map readMap $ fst parse
    let moves = map (map stoi . words . filter (not . isLetter)) $ drop 2 $ snd parse

    print $ map head $ foldl (\cm mv -> move cm (mv !! 0) (mv !! 1) (mv !! 2)) crateMap moves

