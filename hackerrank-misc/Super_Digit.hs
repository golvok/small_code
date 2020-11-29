module Main where

import Data.Char

intToDigits :: Int -> [Int]
intToDigits i = map digitToInt $ show i

process :: [Int] -> Int
process ii | length ii == 1 = head ii
process ii = process $ intToDigits $ sum ii

main :: IO ()
main = do
    inline <- getLine
    let ww = words inline
        n = map digitToInt $ ww !! 0
        k = (read :: String -> Int) $ ww !! 1
        start = k * sum n
    putStrLn $ show $ process $ intToDigits start
