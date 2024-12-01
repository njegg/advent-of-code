module Main where

--- Day 6: Tuning Trouble, Part 1 ---


nodup :: String -> Bool
nodup [] = True
nodup (x:xs)
    | elem x xs = False
    | otherwise = nodup xs

check :: (Bool, Int, String) -> Char -> (Bool, Int, String)
check (f, i, w) c
    | f = (f, i, w)
    | i == 13 = (nodup (w ++ [c]), i + 1, w ++ [c])
    | otherwise = (nodup wc, i + 1, wc)
    where
        wc = drop 1 w ++ [c]

main :: IO ()
main = do
    input <- readFile "./6_i.txt"
    print $ (\(_,i,_) -> i) $ (\(a,b) -> foldl check (False, 13, a) b) $ splitAt 13 input

