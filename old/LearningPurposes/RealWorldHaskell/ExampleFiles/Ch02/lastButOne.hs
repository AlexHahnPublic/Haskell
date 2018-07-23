-- file: Ch02/lastButOne

lastButOne :: [a] -> a

lastButOne xs = head (drop ((length xs) - 2) xs)
