module Main where

swappairs :: [Char] -> [Char]
swappairs (x:y:rest) = [y] ++ [x] ++ swappairs rest
swappairs [] = []

main :: IO ()
main = do
    ntest <- getLine
    input <- getContents
    let ll = lines input
    mapM_ putStrLn $ map swappairs ll
