-- file .../Ch01/wc.hs
-- lines beginning with


main = interact wordCount
    where wordCount input = show (length (lines input)) ++ "\n"
