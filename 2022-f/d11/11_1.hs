module Main where
import Data.List (sortOn)
import Data.Ord (Down(Down))


--- Day 11: Monkey in the Middle, Part 1 ---


data Monkey = Monkey { items :: [Int]
                     , operation :: Int -> Int
                     , test :: Int -> Int
                     , inspections :: Int}

instance Show Monkey where
    show m = show (inspections m, items m)

main :: IO ()
main = do
    input <- readFile "./11_s.txt"
    let ms = readMonkeys $ lines input 
    print $ product . take 2 . sortOn Down . map inspections $ iterate play ms !! 20


play :: [Monkey] -> [Monkey]
play m = foldl turn m [0..length m - 1]

turn :: [Monkey] -> Int -> [Monkey]
turn ms i = foldl (\ma it -> (\maa -> add maa (test m it) it) $ remove ma i) ms $ map ins (items (ms!!i))
    where
        m = ms!!i
        ins = inspect (ms!!i) 
    
inspect :: Monkey -> Int -> Int
inspect m i = flip div 3 $ operation m i

remove :: [Monkey] -> Int -> [Monkey]
remove (m:ms) i
    | i /= 0 = m : remove ms (i - 1)
    | otherwise = Monkey (drop 1 $ items m) (operation m) (test m) (inspections m + 1) : ms

add :: [Monkey] -> Int -> Int -> [Monkey]
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

readItems :: String -> [Int]
readItems [] = []
readItems s = map (stoi . takeWhile (/=','))  $ words $ drop 2 $ dropWhile (/=':') s

readOperation :: String -> (Int -> Int)
readOperation s = op $ words $ dropWhile (/='=') s
    where
        op s
            | s!!2 == "+"   = if head (s!!3) == 'o' then (*2) else (+ stoi (s!!3))
            | otherwise     = if head (s!!3) == 'o' then (^2) else (* stoi (s!!3))
        

readTest :: [String] -> (Int -> Int)
readTest s = \x -> if mod x (n (head s)) == 0 then n (s!!1) else n (s!!2)
    where
        n l = stoi $ last $ words l


stoi :: String -> Int
stoi s = read s :: Int

