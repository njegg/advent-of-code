module Main where
import Data.Char (ord)

data Move = Rock | Paper | Scissors deriving (Eq,Enum)

succ' :: Move -> Move
succ' Scissors = Rock
succ' m = succ m

pred' :: Move -> Move
pred' Rock = Scissors
pred' m = pred m

move :: Char -> Move
move c = toEnum (ord c - ord 'A')

mymove :: Char -> Move -> Move
mymove 'X' m = pred' m
mymove 'Y' m = m
mymove 'Z' m = succ' m

play :: String -> Int
play (c:_:i:_) = 1 + fromEnum my + result
    where
        op = move c
        my = mymove i op
        result
            | succ' my == op = 0
            | pred' my == op = 6
            | otherwise      = 3

main :: IO ()
main = do
    input <- readFile "./2_i.txt"
    print $ sum $ map play $ lines input

