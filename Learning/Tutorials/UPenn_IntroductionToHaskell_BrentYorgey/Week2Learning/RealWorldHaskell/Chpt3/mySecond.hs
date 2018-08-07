mySecond :: [a] -> a

mySecond xs = if null (tail xs)
    then error "list is too short"
    else head (tail xs)

safeSecond [] = Nothing
safeSecond xs = if null (tail xs)
    then Nothing
    else Just (head (tail xs))

tidySecond :: [a] -> Maybe a
tidySecond (_:x:_) = Just x
tidySecond _ = Nothing

