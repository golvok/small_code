-- finds total number of ways to docompose a number int a sum of
-- powers with unique positive integer bases and a particular exponent
module Main where

import Data.Maybe

numPowSum :: Int -> Int -> Int -> [[Int]]
numPowSum _ 0 _ = [[]]
-- numPowSum maxNum num n | maxNum^n < num = []
numPowSum maxNum num n =
    let sqrtNum = floor $ (fromIntegral num)**(1.0/(fromIntegral n))
        realMaxNum = min maxNum sqrtNum
        numsToTry = [1..realMaxNum]
        newNums = map (\x -> num - x^n) numsToTry
        recursiveResults = map (\(x,y) -> numPowSum (x-1) y n) $ zip numsToTry newNums -- [[[Int]]]
        withThisLevel = filter (\(_,recursive) -> length recursive /= 0) $ zip numsToTry recursiveResults -- [(Int, [[Int]])]
    in
    -- error $ show $ (numsToTry, newNums, withThisLevel)
    foldr (++) [] $ map (\(thisLevel,recursive) -> map (thisLevel:) $ recursive) withThisLevel

main :: IO ()
main = do
    instr <- getContents
    let ll = lines instr
        ii = map (read :: String -> Int) ll
        num = ii !! 0
        n = ii !! 1
    putStrLn $ show $ length $ numPowSum num num n