-- file: Ch02/lastButOne2.hs

lastButOne2 :: [a] -> a

lastButOne2 xs = if null (tail(tail xs))
                 then head xs
                 else lastButOne2 (tail xs)
