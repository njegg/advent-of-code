module Main where

import Data.Bifunctor (Bifunctor(bimap))
import Data.List (nub)

--- Day 9: Rope Bridge, Part 1 ---


type Knot = (Int, Int)
type Rope = (Knot, Knot)
type Move = (Char, Int)

main :: IO ()
main = do
    input <- readFile "./9_i.txt"
    let moves = map (bimap head stoi . splitAt 1) $ lines input
    let s = (0, 0)

    print $ length . nub . snd $ foldl move ((s, s), []) moves


move :: (Rope, [Knot]) -> Move -> (Rope, [Knot])
move (r, k) (d, n) = iterate (singleMove d) (r, k) !! n

singleMove :: Char -> (Rope, [Knot]) -> (Rope, [Knot])
singleMove d ((h, t), ts) = (\mr -> (mr, snd mr : ts)) $ follow (moveHead d h, t)

moveHead :: Char -> Knot -> Knot
moveHead d (x, y)
    | d == 'R' = (x + 1, y)
    | d == 'L' = (x - 1, y)
    | d == 'U' = (x, y + 1)
    | d == 'D' = (x, y - 1)


follow :: Rope -> Rope
follow (h@(xh, yh), t@(xt, yt))
    | adjacent h t = (h, t)
    | otherwise    = (h, (xt + signum (xh - xt), yt + signum (yh - yt)))

adjacent :: Knot -> Knot -> Bool
adjacent (xh, yh) (xt, yt) = abs (xh - xt) < 2 && abs (yh - yt) < 2

    
stoi :: String -> Int
stoi s = read s :: Int

