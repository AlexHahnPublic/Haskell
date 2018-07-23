-- file: Ch03/tuple.hs

-- Return the third element of a tuple
third (a, b, c) = c

complicated (True, a, x:xs, 5) = (a, xs)
