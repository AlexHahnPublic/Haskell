-- toDigits converts an integer to a Integer list of its digits
toDigits :: Integer -> [Integer]
toDigits n
  | n <= 0 =
    []
  | otherwise =
    toDigits (n `div` 10) ++ [n `mod` 10]


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

mult2lists' :: [Integer] -> [Integer] -> [Integer]
mult2lists' l1 l2 = case (l1, l2) of
                      | ((x:xs),(y:ys)) -> x*y : mult2lists xs ys
                      | _ = []


-- Double every other returns a list with every other element doubled,
-- starting from the end
doubleEveryOther :: [Integer] -> [Integer]

doubleEveryOther xs = reverse(mult2lists (reverse  xs) (take (length xs) (cycle [1,2])))


doubleEveryOther' :: [Integer] -> [Integer]
doubleEveryOther' =
  snd . foldr doubleIfEvenIndex (0, [])
  where
    doubleIfEvenIndex n (i,ds) =
      if i `mod` 2 == 0
      then
        (i+1, n : ds)
      else
        (i+1, 2*n : ds)


-- sumDigits sums every digit in a list of numbers eg [12,13,8] = 1+2+1+3+8=15
sumDigits :: [Integer] -> Integer

sumDigits [] = 0

sumDigits (x:xs) = sum (toDigits x) + sumDigits xs

sumDigits' :: [Integer] -> Integer
sumDigits' =
  foldl (+) 0 . concatMap toDigits

-- validate indicates whether an Integer could be a valid credit card number
validate :: Integer -> Bool

validate x =
  if(sumDigits(doubleEveryOther(toDigits x)) `mod` 10 == 0)
    then True
    else False

validate' = ((==0) . (`mod` 10)) . sumDigits' . doubleEveryOther' . toDigits

ifelse :: (a -> Bool) -> (a -> b) -> (a->b) -> a -> b
ifelse p t f x =
  if
    p x
  then
    t x
  else
    f x









