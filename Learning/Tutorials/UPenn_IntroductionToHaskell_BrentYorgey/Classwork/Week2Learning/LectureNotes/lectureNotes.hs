data Thing = Shoe
    | Ship
    | SealingWax
    | Cabbage
    | King
    deriving Show

isSmall :: Thing -> Bool
isSmall Shoe = True
isSmall Ship = False
isSmall SealingWax = True
isSmall Cabbage = True
isSmall King = False

isSmall2 :: Thing -> Bool
isSmall2 Ship = False
isSmall2 King = False
isSmall2 _ = True

data FailableDouble = Failure
    | OK Double
    deriving Show

safeDiv :: Double -> Double -> FailableDouble
safeDiv _ 0 = Failure
safeDiv x y = OK (x/y)


failureToZero :: FailableDouble -> Double
failureToZero Failure = 0
failureToZero (OK d) = d

-- Store a person's name, age, and favorite Thing
data Person = Person String Int Thing
    deriving Show

brent :: Person
brent = Person "Brent" 31 SealingWax

getAge :: Person -> Int
getAge (Person _ a _) = a

getName :: Person -> String
getName p@(Person n _ _) = "The name field of(" ++ show p ++ ") is " ++ n

checkFav :: Person -> String
checkFav (Person n _ SealingWax) = n ++ ", you're my favorite kind of person"
checkFav (Person n _ _) = n ++ ", your favorite thing is lame"

failureToZero' :: FailableDouble -> Double
failureToZero' x = case x of
    Failure -> 0
    OK d -> d

data IntList = Empty  | Cons Int IntList

intListProd :: IntList -> Int
intListProd Empty = 1
intListProd (Cons x l) = x * intListProd l

data Tree = Leaf Char
    | Node Tree Int Tree
    deriving Show

treeTest :: Tree
treeTest = Node (Leaf 'x') 1 (Node (Leaf 'y') 2 (Leaf 'z'))
