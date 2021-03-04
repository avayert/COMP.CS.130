{-
Let us number the smaller case characters from ‘a’ to ‘z’ with numbers starting
from 1, that is, ‘a’ is given 1, ‘b’ is given number 2, etc.

Write two functions:

A function charsDivisibleBy :: Int -> [Char] that, given a number n, returns all
the characters that have a number divisible by n.

A function charsProductOf :: [Int] -> [Char] that, given a list of numbers ns,
returns all the characters that have a number that is a product of any two numbers in ns.

As an example, charsDivisibleBy 2 = "bdfhjlnprtvxz" and charsProductOf [2,3,4] = "fhl".

The grader sorts the output of your function so the order is unimportant.

Hint: Using list comprehensions seems like a relatively easy way to solve this.

Return a file named number_char_pairs.hs containing the required functions.
-}

import Data.Char (chr)
import Data.List

dedupe :: String -> String
dedupe [] = []
dedupe (head:tail) = head:filter(/= head) (dedupe tail)

pairs :: [Int] -> [(Int, Int)]
pairs (_:[]) = []
pairs (head:tail) = map(\v -> (head, v)) tail ++ pairs tail

isLetter :: (Int, Int) -> Bool
isLetter (a, b) = 0 <= a * b && a * b <= 26

charsDivisibleBy :: Int -> [Char]
charsDivisibleBy n = map(chr) . map(+ 96) . filter(\x -> (mod x n) == 0) $ [1..26]

charsProductOf :: [Int] -> [Char]
charsProductOf numbers = dedupe . map(chr) . map(+ 96) . map(\(a, b) -> a * b) . filter(isLetter) $ pairs numbers
