{-
This task is here for you to practice basic recursion.

Write a function onlyDigits :: String -> Bool that, given a string, checks
whether the string contains only digits or not. Empty string should return false.

Return a file named only_digits.hs containing the required function.
-}


isDigit :: Char -> Bool
isDigit c = '0' <= c && c <= '9'

onlyDigits :: String -> Bool
onlyDigits "" = False
onlyDigits (head:"") = isDigit head
onlyDigits (head:tail) = (isDigit head) && onlyDigits(tail)
