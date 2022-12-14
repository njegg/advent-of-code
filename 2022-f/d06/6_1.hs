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
    | i == 3 = (nodup (w ++ [c]), i + 1, w ++ [c])
    | otherwise = (nodup wc, i + 1, wc)
    where
        wc = drop 1 w ++ [c]

main :: IO ()
main = do
    input <- readFile "./6_s.txt"
    print $ (\(_,i,_) -> i) $ (\(a,b) -> foldl check (False, 3, a) b) $ splitAt 3 input

