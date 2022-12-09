module Main where

import Data.Bifunctor (Bifunctor(bimap))
import Data.List (nub)

--- Day 9: Rope Bridge, Part 1 ---


type Knot = (Int, Int)
type Move = (Char, Int)


main :: IO ()
main = do
    input <- readFile "./9_i.txt"
    let moves = map (bimap head stoi . splitAt 1) $ lines input
    let rope = replicate 10 (0, 0)

    print $ length . nub . snd $ foldl move (rope, []) moves


move :: ([Knot], [Knot]) -> Move -> ([Knot], [Knot])
move x (d, n) = iterate (singleMove d) x !! n

singleMove :: Char -> ([Knot], [Knot]) -> ([Knot], [Knot])
singleMove d (r:rs, tm) = (\l -> (l, last l : tm)) $ followRope (moveHead d r : rs)

moveHead :: Char -> Knot -> Knot
moveHead d (x, y)
    | d == 'R' = (x + 1, y)
    | d == 'L' = (x - 1, y)
    | d == 'U' = (x, y + 1)
    | d == 'D' = (x, y - 1)


followRope :: [Knot] -> [Knot]
followRope [x, y] = [x, follow x y]
followRope (x:y:xs) = x : follow x y : drop 1 (followRope (follow x y:xs))

follow :: Knot -> Knot -> Knot
follow h@(xh, yh) t@(xt, yt)
    | adjacent h t = t
    | otherwise = (xt + signum (xh - xt), yt + signum (yh - yt))
    
adjacent :: Knot -> Knot -> Bool
adjacent (xh, yh) (xt, yt) = abs (xh - xt) < 2 && abs (yh - yt) < 2


stoi :: String -> Int
stoi s = read s :: Int

