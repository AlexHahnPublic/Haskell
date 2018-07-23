-- file Ch02/myDropOneLine

myDropOneLine n xs = if n<=0 || null xs then xs else myDropOneLine (n-1) (tail xs)
