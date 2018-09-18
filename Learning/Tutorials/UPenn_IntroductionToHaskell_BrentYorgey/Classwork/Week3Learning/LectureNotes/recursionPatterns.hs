data IntList = Empty | Cons Int IntList
    deriving Show

pn = (Cons 4 (Cons (-2) (Empty)))

absAll :: IntList -> IntList
absAll Empty = Empty
absAll (Cons x xs) = Cons (abs x) (absAll xs)

squareAll :: IntList -> IntList
squareAll Empty = Empty
squareAll (Cons x xs) = Cons (x*x) (squareAll xs)

mapIntList :: (Int -> Int) -> IntList -> IntList

mapIntList (f) Empty = Empty
mapIntList (f) (Cons x xs) = Cons (f x) (mapIntList f xs)


addOne x = x + 1
square x = x * x
myAbs x = abs x

addOneToAll :: IntList -> IntList
addOneToAll il = mapIntList addOne il

absAll2 il = mapIntList myAbs il

squareAll2 il = mapIntList square il

keepOnlyEven :: IntList -> IntList
keepOnlyEven Empty = Empty
keepOnlyEven (Cons x xs) = case (even x) of
    True -> Cons x (keepOnlyEven xs)
    False -> keepOnlyEven xs

--What is this notation of defining a function? no equals?
keepOnlyEven2 :: IntList -> IntList
keepOnlyEven2 Empty = Empty
keepOnlyEven2 (Cons x xs)
  | even x    = Cons x (keepOnlyEven2 xs)
  | otherwise = keepOnlyEven2 xs

data List t = E | C t (List t)
    deriving Show


filterList _ E = E
filterList p (C x xs)
  | p x = C x (filterList p xs)
  | otherwise = filterList p xs

mapList :: (a -> b) -> List a -> List b
mapList _ E
mapLIst f (C x xs) = X (f x) (mapList f xs)

