module Main where

import Data.List (intersect)
import Data.Char (ord, isUpper)

split' :: String -> (String, String)
split' s = splitAt (div (length s) 2) s

val :: Char -> Int
val c
    | isUpper c = ord c + (-38)
    | otherwise = ord c + (-96)

main :: IO ()
main = do
    input <- readFile "./3_i.txt"
    print $ sum $ map (val . head . uncurry intersect . split') $ words input

