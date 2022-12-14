module Main where

import Data.List (intersect)
import Data.Char (ord, isUpper)

val :: Char -> Int
val c
    | isUpper c = ord c + (-38)
    | otherwise = ord c + (-96)

group3 :: [String] -> [[String]]
group3 [] = []
group3 (x:y:z:xs) = (x:y:z:[]) : group3 xs

intersect3 :: [String] -> String
intersect3 l = foldl intersect (head l) l

main :: IO ()
main = do
    input <- readFile "./3_i.txt"
    print $ sum $ map (val . head . intersect3) $ group3 $ words input

