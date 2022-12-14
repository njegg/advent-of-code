module Main where

main :: IO ()
main = do
    input <- readFile "./13_i.txt"
    print $ sum . map fst . filter (\(_,(l,r)) -> l < r )  . zip [1..] . split . map (filter (not . flip elem "[],")) $ filter (not . null) $ lines input
    -- print $ split . map (filter (not . flip elem ",")) $ filter (not . null) $ lines input

    print "asd"

split :: [String] -> [(String,String)]
split [x,y] = [(x, y)]
split (x:y:xs) = (x, y) : split xs

