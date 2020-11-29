module Main where

import Data.Char

data State = Alive | Dead deriving (Show, Eq, Ord)
data TreeNode = Empty | Node { left :: TreeNode, state :: State, right :: TreeNode} deriving (Show, Eq)

safeState :: TreeNode -> State
safeState Empty = Dead
safeState n = state n

data LeftRight = LLL | RRR deriving (Show, Eq, Ord)
data Query = Query { steps :: Int, path :: [LeftRight]} deriving (Show, Eq)

-- ACHAR -> 'X' | '.'
-- Tree  -> ACHAR | '(' Tree ACHAR Tree ')'

parseState :: String -> (State,String)
parseState (' ':rest) = parseState rest
parseState ('.':rest) = (Dead,rest)
parseState ('X':rest) = (Alive,rest)
parseState _ = undefined

parseTree :: (String) -> (TreeNode,String)
parseTree "" = (Empty,"")
parseTree (c:rest) = case c of
    ' ' -> parseTree rest
    '(' ->
        let leftres = parseTree rest
            myState = parseState $ snd leftres
            rightres = parseTree $ snd myState
        in (Node (fst leftres) (fst myState) (fst rightres), eatCloseParen $ snd rightres)
    cc ->
        let myState = parseState (c:rest)
        in (Node Empty (fst $ myState) Empty, snd myState)

parseTreeNode :: String -> (TreeNode,String)
parseTreeNode (c:rest) =
    case c of
        ' ' -> parseTreeNode rest

stateToChar :: State -> Char
stateToChar Alive = 'X'
stateToChar Dead = '.'

prettyPrintTree :: TreeNode -> String
prettyPrintTree tree =
    case tree of
        Empty -> ""
        Node Empty s Empty -> [stateToChar s]
        n -> '(':(prettyPrintTree $ left n) ++ [(stateToChar $ state n)] ++ (prettyPrintTree $ right n) ++ ")"

eatCloseParen :: String -> String
eatCloseParen (')':rest) = rest

parseQuery :: String -> (Query,String)
parseQuery (' ':rest) = parseQuery rest
parseQuery str =
    let steps = head $ (reads :: String -> [(Int,String)]) str
        path = parseQueryPath (snd steps)
    in
    (Query (fst steps) (fst path), snd path)

parseQueryPath :: String -> ([LeftRight],String)
parseQueryPath (c:rest) = case c of
    ' ' -> parseQueryPath rest
    '[' -> parseQueryPath rest
    '<' -> (LLL : fst queryPath, snd queryPath)
    '>' -> (RRR : fst queryPath, snd queryPath)
    ']' -> ([],rest)
    where
        queryPath = parseQueryPath rest

intToRule :: Int -> [State]
intToRule 0 = [Dead]
intToRule 1 = [Alive]
intToRule i = (intToRule $ div i 2) ++ (intToRule $ mod i 2)

main :: IO ()
main = do
    ruleStr <- getLine
    treeStr <- getLine
    numQueriesStr <- getLine
    let rule = intToRule $ (read :: String -> Int) ruleStr
        tree = parseTree treeStr
        numQueries = (read :: String -> Int) numQueriesStr
    queriesStrs <- sequence $ replicate numQueries getLine
    let queries = map parseQuery queriesStrs
        results = foldr (process rule) [(fst tree,Alive)] $ map fst $ reverse queries
    -- putStrLn $ prettyPrintTree tree
    -- mapM_ (putStrLn . show) queries
    -- putStrLn $ show $ rule
    -- mapM_ (putStrLn . prettyPrintTree . fst) $ tail $ reverse results
    mapM_ (putStrLn . (\c -> [c]) . stateToChar . snd) $ tail $ reverse results

process :: [State] -> Query -> [(TreeNode,State)] -> [(TreeNode,State)]
process rule query (curr:prevs) =
    (applyQuery rule query (fst curr)):curr:prevs

applyQuery :: [State] -> Query -> TreeNode -> (TreeNode,State)
applyQuery rule query tree =
    let newTree = advanceTree rule (steps query) Dead tree
        result = queryTree (path query) newTree
    in
    (newTree,result)

advanceTree :: [State] -> Int -> State -> TreeNode -> TreeNode
advanceTree _ t _ tree | t < 0 = tree
advanceTree _ 0 _ tree = tree
advanceTree rule 1 _ Empty = Empty
advanceTree rule 1 parentState tree = Node
        (advanceTree rule 1 (state tree) $ left tree)
        (applyRule rule [parentState, safeState $ left tree, state tree, safeState $ right tree])
        (advanceTree rule 1 (state tree) $ right tree)
advanceTree rule t parentState tree = advanceTree rule (t-1) parentState $ advanceTree rule 1 parentState tree

queryTree :: [LeftRight] -> TreeNode -> State
queryTree [] tree = state tree
queryTree (p:path) tree = case p of
    LLL -> queryTree path $ left tree
    RRR -> queryTree path $ right tree

applyRule :: [State] -> [State] -> State
applyRule [r] [] = r
applyRule rule (o:restobserved) =
    let (lrule,rrule) = splitAt (div (length rule) 2) rule
    in case o of
        Alive -> applyRule lrule restobserved
        Dead -> applyRule rrule restobserved

