-- file: Ch03/distinction.hs

--These tuples are structurally identical and therefore have the same type
a = ("Porpoise", "Grey")
b = ("Table", "Oak")

-- These constructors make it possible to differentiate the two!
data Cetacean = Cetacean String String
data Furniture = Furniture String String

c = Cetacean "Porpoise" "Grey"
d = Furniture "Table" "Oak"


