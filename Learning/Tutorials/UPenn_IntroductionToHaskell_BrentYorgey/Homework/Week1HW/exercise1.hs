-- toDigits converts an integer to a Integer list of its digits
toDigits :: Integer -> [Integer]

toDigits 0 = []

toDigits n = if n < 0
    then []
    else toDigits (n `div` 10) ++ [n `mod` 10]

-- toDigits rev does the same but returns the list reversed
toDigitsRev :: Integer -> [Integer]

toDigitsRev 0 = []

toDigitsRev n = if n < 0
    then []
    else [n `mod` 10] ++ toDigitsRev (n `div` 10)

-- For doubling every other element starting from end
mult2lists :: [Integer] -> [Integer] -> [Integer]

mult2lists [] [] = []
mult2lists (x:xs) [] = []
mult2lists [] (y:ys) = []
mult2lists (x:xs) (y:ys) = x*y : mult2lists xs ys

-- Double every other returns a list with every other element doubled,
-- starting from the end
doubleEveryOther :: [Integer] -> [Integer]

doubleEveryOther xs = reverse(mult2lists (reverse  xs) (take (length xs) (cycle [1,2])))

-- sumDigits sums every digit in a list of numbers eg [12,13,8] = 1+2+1+3+8=15
sumDigits :: [Integer] -> Integer

sumDigits [] = 0

sumDigits (x:xs) = sum (toDigits x) + sumDigits xs

-- validate indicates whether an Integer could be a valid credit card number
validate :: Integer -> Bool

validate x = if(sumDigits(doubleEveryOther(toDigits x)) `mod` 10 == 0)
    then True
    else False






