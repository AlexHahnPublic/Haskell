import Data.List
import Data.Ord

myLength :: [a] -> Int

myLength lst = case lst of
    [] -> 0
    x:xs -> 1+ myLength xs

myMean lst = sum lst / fromIntegral (length lst)

myPalindrome :: [a] -> [a]
myPalindrome [] = []
myPalindrome (x:xs) = [x] ++ myPalindrome xs ++ [x]

isPalindrome [] = True
isPalindrome (x:[]) = True
isPalindrome (x:xs) = if(x /= (last xs))
    then False
    else isPalindrome (init xs)

sortByLen :: [[a]] -> [[a]]
sortByLen xs = sortBy (comparing length) xs

myIntersperse :: a -> [[a]] -> [a]
myIntersperse c [[]] = []
myIntersperse c (x:[]) = x
myIntersperse c (x:xs) = x ++ [c] ++ myIntersperse c xs


data Tree a = Node a (Tree a) (Tree a)
            | Empty
              deriving (Show)


--treeHeight :: Tree -> Integer
treeHeight Empty = 0
treeHeight (Node x left right) = 1 + max (treeHeight left) (treeHeight right)

data Direction = MyLeft | MyRight | MyStraight deriving (Eq, Show)

-- if you want to use a new data type point:
data PointData2D = PointData2D {x :: Double, y :: Double}

-- if you just want to use a type synonym
type PointTypeXY = (Double, Double)

whichDirection :: PointTypeXY -> PointTypeXY -> PointTypeXY -> Direction

whichDirection x y z = case compare crossproduct 0 of
    LT -> MyRight
    EQ -> MyStraight
    GT -> MyLeft
    where crossproduct = (fst y - fst x)*(snd z - snd x) - (snd y - snd x)*(fst z - fst x)

whichDirection2 :: PointData2D -> PointData2D -> PointData2D -> Direction

whichDirection2 p1 p2 p3 = case compare crossproduct 0 of
    LT -> MyRight
    EQ -> MyStraight
    GT -> MyLeft
    where crossproduct = ((x p2 - x p1) * (y p3 - y p1)) - ((y p2 -y p1) * (x p3 - x p1))

directions :: [PointTypeXY] -> [Direction]

directions [] = []
directions lstPts = if(length lstPts < 3)
    then []
    else whichDirection (lstPts!!0) (lstPts!!1) (lstPts!!2) : directions (drop 1 lstPts)

