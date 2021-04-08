{-
NOTE: There was prior information about this task in the old Weto page and in Plussa. This is the old Weto version of the task. The Plussa version was quite trivial and not what was meant.

Write a function clusters that is given:
- f, a distance function of type String -> String -> Float (like the ones in Task 1.10)
- d :: Float
- ss :: [String]
For each string s  in ss, the function clusters computes a "cluster", ie a list of
similar strings in ss (strings that are at most distance d from the s).
The list of strings similar to s should also contain s (if the distance function allows).

The clusters and the list of clusters may be in any order. The grader sorts them.

Calling this function with function of

1.10 a), d=0.3 and ss=["aaabc", "aabdd", "a", "aa", "abdd", "bcbcb", "", "abcdefghij"]
should return [[""],["a","aa"],["a","aa","aaabc"],["aa","aaabc","aabdd","bcbcb"],["aaabc","aabdd","abdd"],["aaabc","bcbcb"],["aabdd","abdd"],["abcdefghij"]] (in some order).

1.10 b), d=0.2 and ss=["123a","456789b","45","abc", "ab1", "a12", "abcdefghij"] should return [[],[],[],["123a","45","456789b","a12"],["123a","45","456789b","a12"],["45","456789b"],["45","456789b"]] (in some order).



Grader sorts the output of your function to make sure that the implementation is considered correct as long as the right elements are in the output.
Return a file named clusters.hs containing the required function.
-}

doCluster :: (String -> String -> Float) -> Float -> String -> [String] -> [String]
doCluster func distance source strings = filter(\s -> (func source s) <= distance) $ strings


clusters :: (String -> String -> Float) -> Float -> [String] -> [[String]]
clusters func distance strings = map(\s -> doCluster func distance s strings) $ strings
