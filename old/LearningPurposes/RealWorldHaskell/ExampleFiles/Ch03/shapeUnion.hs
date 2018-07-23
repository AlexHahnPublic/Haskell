-- file: Ch03/shapeUnion.hs

type Vector = (Double, Double)

data Shape = Circle Vector Double
           | Poly [Vector]
