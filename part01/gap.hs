{-
This task is suitable for a recursive solution.

We say that character pair (c1,c2) appears in string s with gap g, if c1 is
before c2 and there are exactly g characters between c1 and c2 in s.

Write a function gap :: (Char, Char) -> Int -> String -> Int that, given a pair
(c1,c2), a gap g and a string s returns an Int telling how many times (c1,c2)
appear in s with gap g.

For example:

gap ('a','b') 1 "aaabbb"
2

Submit a file gap.hs containing the required function.
-}

gap :: (Char, Char) -> Int -> String -> Int
gap t n s = length . filter(==t) $ zip s (drop (n + 1) s)
