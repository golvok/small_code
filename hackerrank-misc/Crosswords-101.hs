
module Main where
import Data.List.Split (splitOn)
import Data.List (find)
import qualified Data.Map.Strict as SMap
import Data.Maybe
import Control.Lens (each,over)

data WordDir = Down | Across deriving (Show, Eq, Ord)

data SquareType = Blocked deriving Show
type BoardSquare = Either SquareType (Maybe Char)

type StartInfo = ((Int,Int), WordDir)
loc startinfo = fst startinfo
xVal startinfo = fst $ loc startinfo
yVal startinfo = snd $ loc startinfo
dirOf startinfo = snd startinfo

nudgeOver startinfo = case startinfo of
    ((x,y),Across) -> ((x+1,y),Across)
    ((x,y),Down) -> ((x,y+1),Down)

couldIntersectWith start wordLocations =
    let inOtherDirections = SMap.filterWithKey (\k v -> dirOf start /= dirOf k) wordLocations
        intersectFilter k v = case dirOf start of
            Across -> (yVal k < yVal start) && (xVal k >= xVal start)
            Down   -> (xVal k < xVal start) && (yVal k >= yVal start)
    in
    SMap.filterWithKey intersectFilter inOtherDirections

getBoardSquare :: [[BoardSquare]] -> (Int,Int) -> SInfoToWord -> BoardSquare
getBoardSquare board (x,y) wordLocations =
    let pointsAt start word = case start of
            ((_,sy),Across) -> sy == y
            ((sx,_),Down)   -> sx == x
        overlaps start word = case dirOf start of
            Across -> (xVal start <= x) && (x < xVal start + length word)
            Down   -> (yVal start <= y) && (y < yVal start + length word)
        overlappingWords = SMap.toAscList $ SMap.filterWithKey (\s w -> pointsAt s w && overlaps s w) wordLocations
        charIndex start = case dirOf start of
            Across -> x - xVal start
            Down   -> y - yVal start
    in
    case overlappingWords of
        [] -> board !! y !! x
        ((start,word):rest) -> Right $ Just $ word !! charIndex start

type SInfoToWord = SMap.Map StartInfo String

boardDim = 10

doesWordWork :: [[BoardSquare]] -> StartInfo -> String -> SInfoToWord -> Bool
doesWordWork board start [] wordLocations = True
doesWordWork board start word wordLocations =
    let thisdir = dirOf start
        valueAtStart = getBoardSquare board (loc start) wordLocations
        noProblemHere = case valueAtStart of
            Left Blocked -> False
            Right Nothing -> True
            Right (Just c) -> c == head word
    in
    noProblemHere && doesWordWork board (nudgeOver start) (tail word) wordLocations

tryWordHere :: [[BoardSquare]] -> [StartInfo] -> [String] -> StartInfo -> String -> SInfoToWord -> Maybe (SInfoToWord)
tryWordHere board unusedStarts unusedWords start word wordLocations =
    let otherStarts = filter (/= start) unusedStarts
        otherWords = filter (/= word) unusedWords
        wordWorks = doesWordWork board start word wordLocations
    in
    if wordWorks then
        let withThisWord = SMap.insert start word wordLocations
        in
        tryEverything board otherStarts otherWords withThisWord
    else
        Nothing

tryEverything :: [[BoardSquare]] -> [StartInfo] -> [String] -> SInfoToWord -> Maybe (SInfoToWord)
tryEverything _ _ [] wordLocations = Just wordLocations
tryEverything board unusedStarts unusedWords wordLocations =
    let allResults = map (\start -> map (\word -> tryWordHere board unusedStarts unusedWords start word wordLocations) unusedWords) unusedStarts
    in
    case find (isJust . find isJust) allResults of
        Nothing -> Nothing
        Just results -> case find isJust results of
            Nothing -> undefined
            Just result -> result

process :: [[BoardSquare]] -> [String] -> Maybe (SInfoToWord)
process board words =
    let starts = findStarts 1 1 board
    in
    tryEverything board starts words SMap.empty

isStart :: Int -> Int -> [[BoardSquare]] -> [WordDir]
isStart x y board =
    case (over each) (\(x,y) -> board !! y !! x) ((x-1, y), (x, y-1), (x+1, y), (x, y+1)) of
        (Left Blocked, Left Blocked, Left Blocked, _           ) -> [Down]
        (Left Blocked, Left Blocked, _,            Left Blocked) -> [Across]
        (Left Blocked, Left Blocked, _,            _           ) -> [Across, Down]
        (Left Blocked, _,            Left Blocked, _           ) -> []
        (_,            Left Blocked, _,            Left Blocked) -> []
        (_,            Left Blocked, _,            _           ) -> [Down]
        (Left Blocked, _,            _,            _           ) -> [Across]
        _ -> []

findStarts :: Int -> Int -> [[BoardSquare]] -> [StartInfo]
findStarts x y board | length board == (y+1) = []
findStarts x y board | length (board !! y) == (x+1) = findStarts 0 (y+1) board
findStarts x y board =
    (case board !! y !! x of
        Left Blocked -> []
        Right Nothing -> (let isst = isStart x y board
            in
            map (\ss -> ((x,y),ss)) isst)
    ) ++ findStarts (x+1) y board

-- -- --

parseInput :: String -> ([[BoardSquare]],[String])
parseInput strin = let
    ll = lines strin
    board = map (map charToBs) (take boardDim ll)
    sidepaddedboard = map (\l -> (Left Blocked):l ++ [Left Blocked]) board
    vpad = [replicate (boardDim+2) (Left Blocked)]
    paddedboard = vpad ++ sidepaddedboard ++ vpad
    ws = splitOn ";" $ last ll
    in
    (paddedboard, ws)

charToBs :: Char -> BoardSquare
charToBs c =
    case c of
        '+' -> Left Blocked
        '-' -> Right Nothing
        cc -> Right (Just cc)

bsToChar :: BoardSquare -> Char
bsToChar bs =
    case bs of
        Left Blocked -> '+'
        Right Nothing -> '-'
        Right (Just c) -> c

prettify :: [[BoardSquare]] -> Maybe SInfoToWord -> String
prettify board wordLocations =
    let onedim = [1..boardDim]
        coords = [[(x,y) | x <- onedim] | y <- onedim]
        lines = map (map (\xy -> bsToChar $ getBoardSquare board xy (fromMaybe SMap.empty wordLocations))) coords
    in
    unlines lines

main :: IO ()
main = do
    inlines <- getContents
    let (board,words) = parseInput inlines
    putStr $ prettify board $ process board words
