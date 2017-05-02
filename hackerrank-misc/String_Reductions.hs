module Main where

import Data.Maybe

main :: IO ()
main = do
    lll <- getLine
    putStrLn $ process [] lll
    
process :: [Char] -> String -> String
process _ [] = []
process seenBefore (c:rest) =
    if elem c seenBefore then
        process seenBefore rest
    else
        (c:process (c:seenBefore) rest)
