module Main where
import Data.List (sortOn)
import Data.Ord (Down(Down))


--- Day 11: Monkey in the Middle, Part 2 ---


data Monkey = Monkey { items :: [[Int]]
                     , operation :: [Int] -> [Int]
                     , test :: [Int] -> Int
                     , inspections :: Int}

instance Show Monkey where
    show m = show (inspections m, items m)

main :: IO ()
main = do
    input <- readFile "./11_s.txt"
    let ms = readMonkeys $ lines input 
    print $ product . take 2 . sortOn Down $  map inspections $ iterate play ms !! 10000


play :: [Monkey] -> [Monkey]
play m = foldl turn m [0..length m - 1]

turn :: [Monkey] -> Int -> [Monkey]
turn ms i = foldl (\ma it -> (\maa -> add maa (test m it) it) $ remove ma i) ms $ map (operation m) (items (ms!!i))
    where
        m = ms!!i
        ins = operation m

remove :: [Monkey] -> Int -> [Monkey]
remove (m:ms) i
    | i /= 0 = m : remove ms (i - 1)
    | otherwise = Monkey (drop 1 $ items m) (operation m) (test m) (inspections m + 1) : ms

add :: [Monkey] -> Int -> [Int] -> [Monkey]
add [] _ _ = []
add (m:ms) id it
    | id /= 0 = m : add ms (id - 1) it
    | otherwise = Monkey (items m ++ [it]) (operation m) (test m) (inspections m) : ms


splitBy :: Char -> String -> [String]
splitBy del = foldr (\x (a:as) -> if x /= del then (x : a) : as else [] : a : as) [""]

readMonkeys :: [String] -> [Monkey]
readMonkeys [] = []
readMonkeys s = readMonkey (take 6 s) : readMonkeys (drop 7 s)

readMonkey :: [String] -> Monkey
readMonkey s = Monkey (readItems $ s!!1) (readOperation $ s!!2) (readTest (take 3 $ drop 3 s)) 0

readItems :: String -> [[Int]]
readItems [] = []
readItems s = map ((\i -> [mod i x | x <- [1..100]]) . stoi . takeWhile (/=','))  $ words $ drop 2 $ dropWhile (/=':') s

readOperation :: String -> ([Int] -> [Int])
readOperation s
    | split!!2 == "+" = \i -> zipWith (\x y -> mod (x+n) y) i [1..100]
    | otherwise       = \i -> zipWith (\x y -> if n > 0 then mod (x*n) y else mod (x*x) y) i [1..100]
        where
            split = words $ dropWhile (/='=') s
            n = if head (split!!3) == 'o' then (-1) else stoi (split!!3)
        

readTest :: [String] -> ([Int] -> Int)
readTest s = \i -> if i!!(n (head s) - 1) == 0 then n (s!!1) else n (s!!2)
    where
        n l = stoi $ last $ words l


stoi :: String -> Int
stoi s = read s :: Int

