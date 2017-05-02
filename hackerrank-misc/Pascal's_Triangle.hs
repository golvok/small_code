module Main where

pascal :: Int -> [[Int]]
pascal 1 = [[1]]
pascal n =
    let above = pascal $ n-1
        prevrow = [0] ++ last above ++ [0]
        row = zipWith (+) prevrow (tail prevrow)
    in
    above ++ [row]

unwordsunlines :: (Show a) => [[a]] -> String
unwordsunlines x = unlines $ map (unwords . map show) x

main :: IO ()
main = do
    input <- getContents
    let n = (read :: String -> Int) $ head $ words input
    putStr $ unwordsunlines $ pascal n
