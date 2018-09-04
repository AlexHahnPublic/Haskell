module Golf where

skips :: [a] -> [[a]]
skips xs = [getIndexs [ind,(ind*2+1)..length xs-1] xs | ind <- [0..((length xs)-1)]]
    where getIndexs inds lst = [lst!!i | i <- inds]

localMaxima :: [Integer] -> [Integer]

localMaxima xs = [isLocalMax xs!!i xs!!(i+1) xs!!(i+2) | i <- [0..(length xs -3)]]
    where isLocalMax a b c = if a < b && c < b then b else 0
