{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where
import Log
import Data.List
import Data.List.Split
import Text.Read()

isM :: String -> Bool
isM s = s `elem` ["I","W","E"]

isInt :: String -> Bool
isInt s = not (False `elem` boolIntList s)

boolIntList :: [Char] -> [Bool]
boolIntList i = if i == [] then [True] else  (i!!0 `elem` "1234567890") : (boolIntList (tail i))

char2IorW :: [Char] -> MessageType
char2IorW c = case c of
    "I" -> Info
    "W" -> Warning
    _ -> error "not a  I or W MessageType character"

checkLen :: Int -> String -> Bool
checkLen n str = if length (splitOn " " str) < n then False else True


parseMessage :: String -> LogMessage

parseMessage line = case (isM (parsed!!0), isInt (parsed!!1), isInt (parsed!!2))  of
    (False, _, _) -> Unknown line
    (True, True, True) -> if((parsed!!0) == "E")
        then (LogMessage (Error (read (parsed!!1)::Int)) (read (parsed!!2)::Int) (intercalate " " (drop 3 parsed)))
        else LogMessage (char2IorW (parsed!!0)) (read (parsed!!1)::Int) (intercalate " " (drop 2 parsed))
    (True, True, False) -> if ((parsed!!0) `elem` ["I","W"])
        then LogMessage (char2IorW (parsed!!0)) (read (parsed!!1)::Int) (intercalate " " (drop 2 parsed))
        else Unknown line
    (_,_,_) -> Unknown line
    where parsed = splitOn " " line

parse :: String -> [LogMessage]

parse str = map parseMessage ls
    where ls = lines str


myinsert :: LogMessage -> MessageTree -> MessageTree

myinsert logMessage tree  = case logMessage of
    Unknown _ -> tree
    LogMessage _ insert_ts _ -> case tree of
        Leaf -> Node Leaf logMessage Leaf
        Node leftTree (LogMessage tree_mt tree_ts tree_text) rightTree -> if insert_ts > tree_ts then (Node leftTree (LogMessage tree_mt tree_ts tree_text) (myinsert logMessage rightTree)) else (Node (myinsert logMessage leftTree) (LogMessage tree_mt tree_ts tree_text) rightTree)
        _ -> Leaf

build :: [LogMessage] -> MessageTree


build [] = Leaf

build (x:xs) = myinsert x (build xs)


build2 :: [LogMessage] -> MessageTree
build2 lst = case lst of
    [] -> Leaf
    x:xs -> myinsert x (build xs)

inOrder :: MessageTree -> [LogMessage]

inOrder Leaf = []

inOrder tree = case tree of
    Leaf -> []
    Node Leaf lm Leaf -> [lm]
    Node lt lm rt -> inOrder lt ++ [lm] ++ inOrder rt


whatWentWrong :: [LogMessage] -> [String]

whatWentWrong lst = case srtList of
    [] -> []
    x:xs -> case x of
        LogMessage (Error num) _ text -> if num > 50
                then [text] ++ whatWentWrong xs
                else whatWentWrong xs
        _ -> whatWentWrong xs
    where srtList =inOrder (build lst)


{-
a = LogMessage Info 305 "sdfa3g"
b = LogMessage (Error 23425) 299 "testing error 2"
c = LogMessage Info 388 "fhs"
d =  Unknown "dsfgsdfgw"
e = LogMessage Info 290 "fasd"
f = LogMessage Info 301 "rtj"
g = LogMessage Warning 295 "sad"
lst = [a,b,c,d,e,f,g]
myTree = build2 lst
-}
