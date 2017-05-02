module Main where

import Data.List

main :: IO ()
main = do
    instring <- getContents
    let ll = lines instring
    putStrLn $ prettify $ process (ll !! 0) (ll !! 1)

prettify :: (String,String,String) -> String
prettify (prefix,a,b) = 
    let outdata = map (\x -> [show $length x, x]) [prefix,a,b]
    in
    unlines $ map unwords outdata

process :: String -> String -> (String,String,String)
process strA strB =
    let prefixPairs = takeWhile (\(a,b) -> a == b) $ zip strA strB
        prefix = map fst prefixPairs
        dropPrefix = drop (length prefix)
    in
    (prefix, dropPrefix strA, dropPrefix strB)
