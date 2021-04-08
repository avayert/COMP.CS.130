{-
Notice that e.g. length of a list is an Int and you can get a fractional
(non-Integer) value out of that with the fromIntegral function.

a) Write a function distance1 :: String -> String -> Float that, given two
strings s1 and s2, calculates their distance using the following formula
((count of how many of the characters in s1 do not appear in s2) + (count of how many of the characters in s2 do not appear in s1) ) / ( (length of s1) + (length of s2) ) .

If both of the lists are empty, then the distance is 0. For example, the
distance between “aaabc” and “aabdd” with this function is (1 + 2) / (5 + 5).

b) Write a function distance2 :: String -> String -> Float that, given two
strings s1 and s2, calculates their distance using the following formula

((count of characters in s1 that are other than any of ‘0’..‘9’) + (count of characters in s2 that are other that any of ‘0’..‘9’) ) / ((length of s1) + (length of s2) ).

If both lists are empty, then the distance is 0. For example, the distance
between “xy765” and “abc2311” with this function is (2 + 3) / (5 + 7).

Please note that these functions are not standard well-behaving distance functions.

Hint: List comprehension is useful here.

Return a file distances1.hs containing the required functions.
-}

import Data.List

isDigit :: Char -> Bool
isDigit c = '0' <= c && c <= '9'

difference :: String -> String -> Int
difference s1 s2 = length . filter(\i -> not $ elem i s2) $ s1

distance1 :: String -> String -> Float
distance1 "" "" = 0
distance1 s1 s2 = (fromIntegral ((difference s1 s2) + (difference s2 s1))) / (fromIntegral (sum . map(length) $ [s1, s2]))


predicate :: String -> Int
predicate s = length . filter(not . isDigit) $ s

distance2 :: String -> String -> Float
distance2 "" "" = 0
distance2 s1 s2 = (fromIntegral . sum . map(predicate) $ [s1, s2]) / (fromIntegral (sum . map(length) $ [s1, s2]))
