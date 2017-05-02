import Data.Set (toList, fromList, size)

isFunc :: [[Int]] -> Bool
isFunc points =
    let uniquepts = fromList points
        uniquexvals = fromList $ map head points
    in
    (size uniquepts) == (size uniquexvals)

process :: [[Int]] -> [[Char]]
process [] = []
process (npoint:rest) =
    let datapair = splitAt (head npoint) rest
        myresult = [if isFunc $ fst datapair then "YES" else "NO"]
        nextresult = process $ snd datapair    
    in
    myresult ++ nextresult
    


main :: IO ()
main = do
    dataIn <- getContents
    mapM_ putStrLn $ process $ tail $ map (map (read :: String -> Int)) $ (map words $ lines dataIn)
