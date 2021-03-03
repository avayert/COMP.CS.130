{-
This task is meant to be solved with list comprehension.

Write a function headOrLast :: [String] -> Char -> [String] that, given a list
of strings and a character, evaluates to a list with all the strings of the
input list that either begin or end with the input character.

Grader sorts the output of your function so the order in which the elements are does not matter.


Submit a file named list_of_strings.hs that has the function
-}

reverseString :: String -> String
reverseString s = foldl (\curr new -> new : curr) [] s

startsWith :: String -> Char -> Bool
startsWith (head:tail) char = head == char

startsOrEndsWith :: String -> Char -> Bool
startsOrEndsWith string char = (startsWith string char) || (startsWith (reverseString string) char)

headOrLast :: [String] -> Char -> [String]
headOrLast strings char = filter(\s -> startsOrEndsWith s char) $ strings
