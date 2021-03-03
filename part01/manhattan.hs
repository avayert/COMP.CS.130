{-
This task is meant to be solved by list comprehension, which gives you a very short solution.

Manhattan distance between two points is the sum of x-distance and y-distance.
Ie. Manhattan distance between (3,3) and (2,6) is 1 + 3 = 4
(distance between 3 and 2 + distance between 3 and 6)

Write a function points :: Int -> [(Int, Int)] that given an Int x, evaluates
to a list of such points in 2-dimensional space (pairs of type (Int, Int)),
that their Manhattan distance from origin (0, 0) is at most x.

The grader sorts the output of your function so the order in which the elements are does not matter.

Submit a file named manhattan.hs that has the function.
-}

manhattanDistance :: (Int, Int) -> Int
manhattanDistance (a, b) = sum . map(abs) $ [a, b]

points :: Int -> [(Int, Int)]
points r = filter (\p -> (manhattanDistance p) <= r) $ [(x, y) | x <- [-r..r], y <- [-r..r]]
