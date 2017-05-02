module Main where

data Serp = On | Off | Out

innerConcat3 = zipWith3 (\l c r -> l ++ c ++ r)

tri :: Serp -> Int -> [[Serp]]
tri s h = let
        lcol = replicate h [s]
        ccol = [[]] ++ (replicate (h-1) [s])
        rcol = [[]] ++ (tri s (h-1))
    in
    innerConcat3 lcol ccol rcol
    
uptri :: Serp -> Int -> [[Serp]]
uptri s h = let
        lcol = replicate h [s]
        ccol = (replicate (h-1) [s]) ++ [[]]
        rcol = (uptri s (h-1)) ++ [[]]
    in
    innerConcat3 lcol ccol rcol
    
halfuptri :: Serp -> Int -> [[Serp]]
halfuptri s h = let
        lcol = replicate h [s]
        rcol = (halfuptri s (h-1)) ++ [[]]
    in
    zipWith (++) lcol rcol

sierpinski :: Int -> Int -> [[Serp]]
sierpinski 0 h = tri On h
sierpinski n h =
    let halfheight = div h 2
        onethird = sierpinski (n-1) halfheight
        bottom = innerConcat3 onethird (uptri Off halfheight) onethird
    in
    onethird ++ bottom

toChar :: Serp -> Char
toChar On = '1'
toChar Off = '_'
toChar Out = '_'

prettyTriangle :: [[Serp]] -> String
prettyTriangle s =
    let outer = (halfuptri Out 31) ++ [[]]
        serps = innerConcat3 outer s outer
    in
    unlines $ map (map toChar) serps

main :: IO ()
main = do
    input <- getLine
    let n = (read :: String -> Int) input
    putStr $ prettyTriangle $ sierpinski n 32
