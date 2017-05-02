module Main where

process :: [Char] -> Char -> Int -> [Char]
process [] currletter 0 = []
process [] currletter currcount = 
    if currcount < 2 then
        replicate currcount currletter
    else
        [currletter] ++ show currcount
process (x:rest) currletter currcount =
    if x == currletter then
        process rest x $ succ currcount
    else
        (process [] currletter currcount) ++ process rest x 1

main :: IO ()
main = do
    datain <- getLine
    putStrLn $ process datain 'a' 0
