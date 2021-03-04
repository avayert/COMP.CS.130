{-
Write a function commonSubstring :: String -> String -> String that, given two strings s1 and s2, computes a common “substring” of s1 and s2 as follows. The function finds the earliest common character c (closest to head of either s1 or s2 appearing in both sequences). The function removes c and all the characters before it in both strings, puts c in the output string, and continues.

If there are two candidates for the earliest common character, pick the one from s1.

For example:

commonSubstring "XabcdefgY" "abcdefgXY"
"XY"
commonSubstring "abcdefgXY" "XabcdefgY"
"abcdefgY"

Please note that the result is not what is normally meant by substring.

Submit a file common_substring.hs containing the required function.
-}

commonSubstring :: String -> String -> String
commonSubstring "" _ = ""
commonSubstring (head:tail) s | all(/=head) s = commonSubstring tail s
commonSubstring (head:tail) s = head:commonSubstring tail (dropWhile (/=head) s)
