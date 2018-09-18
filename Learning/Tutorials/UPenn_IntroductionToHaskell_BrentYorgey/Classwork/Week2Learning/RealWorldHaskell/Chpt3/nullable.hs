data MyMaybe a = MyJust a
    | MyNothing
    deriving (Show)

someBool = MyJust True

someString = MyJust "something"

wrapped = MyJust (MyJust "wrapped")


