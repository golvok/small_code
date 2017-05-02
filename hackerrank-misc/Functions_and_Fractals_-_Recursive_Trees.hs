module Main where

data TTree = Tree | NotTree

lheight n = 2^(n-1)

swidth 5 = 2^5;
swidth n = (2^n) + (swidth $ n+1)

innerConcat = zipWith (++)
innerConcat3 = zipWith3 (\l c r -> l ++ c ++ r)

onecol n tt = replicate (lheight n) [tt]

tri :: TTree -> Int -> [[TTree]]
tri s h = let
        lcol = replicate h [s]
        ccol = [[]] ++ (replicate (h-1) [s])
        rcol = [[]] ++ (tri s (h-1))
    in
    innerConcat3 lcol ccol rcol
    
halftri :: TTree -> Int -> [[TTree]]
halftri s h = let
        lcol = replicate h [s]
        rcol = [[]] ++ (halftri s (h-1))
    in
    innerConcat lcol rcol

uptri :: TTree -> Int -> [[TTree]]
uptri s h = let
        lcol = replicate h [s]
        ccol = (replicate (h-1) [s]) ++ [[]]
        rcol = (uptri s (h-1)) ++ [[]]
    in
    innerConcat3 lcol ccol rcol
    
halfuptri :: TTree -> Int -> [[TTree]]
halfuptri s h = let
        lcol = replicate h [s]
        rcol = (halfuptri s (h-1)) ++ [[]]
    in
    innerConcat lcol rcol

makeBox h w tt = replicate h $ replicate w tt

treeyesno True = Tree
treeyesno False = NotTree

makeTree :: Int -> Int -> [[TTree]]
makeTree 0 m = [[]]
makeTree n m = 
    let smallerTree = makeTree (n-1) (m-1)
        levelAbove = innerConcat smallerTree smallerTree
        shorthalftri = [[]] ++ (halftri NotTree $ (lheight n)-1)
        treeType = treeyesno (m > 0)
        topleft = innerConcat shorthalftri (onecol n treeType)
        topcentre = uptri NotTree $ lheight n
        topright = innerConcat (onecol n treeType) shorthalftri
        top = innerConcat3 topleft topcentre topright
        bottomside = makeBox (lheight n) (lheight n) NotTree
        bottom = innerConcat3 bottomside (onecol n treeType) bottomside
        side = makeBox ((lheight n)*2) ((lheight n)*2-1) NotTree
        thislevel = innerConcat side (top ++ bottom)
    in
    levelAbove ++ thislevel

makeSiding 0 = [[]]
makeSiding n = (makeSiding (n-1)) ++ (makeBox ((lheight n)*2) ((swidth (n-1))-1) NotTree)

ttreeToChar :: TTree -> Char
ttreeToChar tt = case tt of
    Tree -> '1'
    NotTree -> '_'

prettyPrint :: [[TTree]] -> String
prettyPrint tree =
    let ptree1 = innerConcat (makeSiding 5) tree
        ptree2 = map (drop 45) ptree1
        ptree3 = map (++ (replicate 100 NotTree)) ptree2
        ptree4 = map (take 100) ptree3
        lines = map (map ttreeToChar) ptree4
    in
    unlines lines

main :: IO ()
main = do
    levelLine <- getLine
    putStrLn $ prettyPrint $ makeTree 5 ((read :: String -> Int) levelLine)