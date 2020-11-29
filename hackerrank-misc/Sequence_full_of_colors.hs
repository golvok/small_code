module Main where

import Data.List
import Control.Lens (over,each)

countEquals :: (Eq a) => [a] -> a -> Int
countEquals [] _ = 0
countEquals (a:rest) aa =
    (if a == aa then 1 else 0) + countEquals rest aa

-- this has a memory problem!!!
process :: [Char] -> Bool
process str =
    let prefixes = inits str
        totals = map (\p -> map (countEquals p) "RGBY") prefixes
        rgdiffs = map (\t -> t !! 0 - t !! 1) totals
        ybdiffs = map (\t -> t !! 2 - t !! 3) totals
        (rgLT1,ybLT1) = (over each) (map ((<= 1) . abs)) (rgdiffs,ybdiffs)
    in
    (all (== True) rgLT1) && (all (== True) ybLT1) && (last rgdiffs == 0) && (last ybdiffs == 0)

-- weird but efficient
process2 :: [Char] -> Int -> Int -> Int -> Int -> Bool
process2 _ rtot gtot _ _ | abs (rtot - gtot) > 1 = False
process2 _ _ _ btot ytot | abs (btot - ytot) > 1 = False
process2 [] rtot gtot btot ytot = (rtot == gtot) && (btot == ytot)
process2 ('R':rest) rtot gtot btot ytot = process2 rest (rtot+1) gtot btot ytot
process2 ('G':rest) rtot gtot btot ytot = process2 rest rtot (gtot+1) btot ytot
process2 ('B':rest) rtot gtot btot ytot = process2 rest rtot gtot (btot+1) ytot
process2 ('Y':rest) rtot gtot btot ytot = process2 rest rtot gtot btot (ytot+1)

main :: IO ()
main = do
    nteststr <- getLine
    teststr <- getContents
    let tests = lines teststr
    -- mapM_ (putStrLn . show) $ map process tests
    mapM_ (putStrLn . show) $ map (\s -> process2 s 0 0 0 0) tests
