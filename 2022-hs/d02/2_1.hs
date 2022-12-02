module Main where
import Data.Char (ord)

data Move = Rock | Paper | Scissors deriving (Eq,Enum)

succ' :: Move -> Move
succ' Scissors = Rock
succ' m = succ m

move :: Char -> Move
move c
    | ord c > ord 'C' = toEnum (ord c - ord 'X')
    | otherwise       = toEnum (ord c - ord 'A')

play :: String -> Int
play (a:_:b:_) = 1 + fromEnum (move b) + result
    where
        result
            | succ' (move a) == move b = 6
            | succ' (move b) == move a = 0
            | otherwise                = 3

main :: IO ()
main = do
    input <- readFile "./2_i.txt"
    print $ sum $ map play $ lines input

