data List a = Cons a (List a)
    | Nil
    deriving (Show)

fromList (x:xs) = Cons x (fromList xs)
fromList [] = Nil

convList (Cons a as) = a : convList as
convList Nil = []
