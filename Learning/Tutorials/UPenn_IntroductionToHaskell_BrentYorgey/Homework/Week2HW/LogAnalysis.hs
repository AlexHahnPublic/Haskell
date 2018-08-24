{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where
import Log
import Data.List
import Data.List.Split
import Text.Read

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

