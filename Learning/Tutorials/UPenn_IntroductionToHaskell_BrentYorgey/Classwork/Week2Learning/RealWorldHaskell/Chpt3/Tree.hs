data Tree a = Node a (Tree a) (Tree a)
    | Empty
    deriving (Show)

simpleTree = Node "parent" (Node "left child" Empty Empty)
    (Node "right child" Empty Empty)


data Tree2 a = MyNode a (Maybe(Tree a)) (Maybe(Tree a))

simpleTree2 = MyNode "parent2" (MyNode "left child") (MyNode)
