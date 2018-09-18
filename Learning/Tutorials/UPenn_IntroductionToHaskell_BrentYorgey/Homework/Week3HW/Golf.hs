module Golf where

import Data.Maybe
import Data.Map as D --(fromListWith, toList, keys, filter, elems)

skips :: [a] -> [[a]]
skips xs = [getIndexs [ind,(ind*2+1)..length xs-1] xs | ind <- [0..((length xs)-1)]]
    where getIndexs inds lst = [lst!!i | i <- inds]

localMaxima :: [Integer] -> [Integer]

localMaxima xs = catMaybes[isLocalMax (xs!!i) (xs!!(i+1)) (xs!!(i+2)) | i <- [0..(length xs -3)]]
    where isLocalMax a b c = if a < b && c < b then Just b else Nothing

--what is the type of frequency?
frequency :: [Integer] -> Map Integer Integer
frequency xs = fromListWith (+) [(x,1) | x <- xs]

--horizontal_histogram :: [Int] -> [String]

getMaxfromMap :: Map Integer Integer -> [Integer]
getMaxfromMap mp = D.keys $ D.filter (==d) mp
    where d = maximum $ D.elems mp

--TODO: insert type sig
decreaseRecord :: Map Integer Integer -> [Integer] -> Map Integer Integer
decreaseRecord m lstKeys = case lstKeys of
                             [] -> m
                             x:xs -> decreaseRecord (insertWith (+) x (-1) m) xs
                             --_ -> ex

-- TODO: this type definition apparently has a problem
--decreaseRecord' :: Map Integer Integer -> [Integer] -> Map Integer Integer
decreaseRecord' m lstKeys = Prelude.map decreaseBy1 m
  where decreaseBy1 (t1, t2) = if elem t1 lstKeys then (t1,t2-1) else (t1, t2)


empty10StringTuple = zip [0..9] "          "
printInds inds = Prelude.map indsToStars empty10StringTuple
  where indsToStars (t1, t2) = if elem t1 inds then '*' else ' '

tuple2lst :: (Integer, Integer) -> [Integer]
tuple2lst (x1,x2) = replicate (fromIntegral x2) x1

map2lst :: Map Integer Integer -> [Integer]
map2lst mp = concat (Prelude.map tuple2lst (toList mp))





histogram :: [Integer] -> String
histogram ls = case ls of
  [] -> "\n==========\n0123456789\n"
  x:xs -> "\n" ++ printInds(getMaxfromMap ctMap) ++ histogram (map2lst (decreaseRecord ctMap (getMaxfromMap ctMap)))
    where ctMap = frequency ls

ls = [1,2,4,1,7,1,8,7,9]

mp = frequency ls







count :: Eq a => a -> [a] -> Int
count ls = length . Prelude.filter (==ls)

counts :: Eq a => [a] -> [a] -> [Int]
counts vs xs = Prelude.map (flip count xs) vs




