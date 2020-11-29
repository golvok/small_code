-- output list of elements that appear at least k times

module Main where

import qualified Data.Map.Strict as SMap
type ItoIMap = SMap.Map Int Int

process :: ItoIMap -> [Int] -> Int -> [Int] -> [Int]
process numSeen keyorder k [] =
    let enoughTimes = SMap.filter (>= k) numSeen
    in
    filter (\n -> SMap.member n enoughTimes) keyorder
process numSeen keyorder k (n:rest) =
    let valBefore = SMap.lookup n numSeen
    in
    case valBefore of
        Nothing -> process (SMap.insert n 1 numSeen) (n:keyorder) k rest
        Just val -> process (SMap.insert n (val+1) numSeen) keyorder k rest

prettify :: [Int] -> String
prettify [] = "-1"
prettify nums = unwords $ map show $ reverse nums

main :: IO ()
main = do
    nteststr <- getLine
    datastr <- getContents
    let ntest = (read :: String -> Int) nteststr
        ll = lines datastr
        ww = map words ll
        ii = map (map (read :: String -> Int)) ww
        dd = map (\i -> (ii !! i !! 1, ii !! (i+1))) $ map (\x -> (x-1)*2) [1..ntest]
    mapM_ (\(k,nums) -> putStrLn $ prettify $ process SMap.empty [] k nums) dd
