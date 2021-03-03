{-
Write a function nextIsGreater :: [Int] -> [Int] that, given a list of numbers,
produces a list with all elements of the input list such that the element is
followed by a greater number in the input list (the next number is greater).

The numbers need to be in the same order relative to each other in the output
list as they are in the input list.

Return your solution in a file named greater_numbers.hs

An example evaluation of the function:
-}

nextIsGreater :: [Int] -> [Int]
nextIsGreater (head:tail) = map(\(a, b) -> a) . filter(\(x, y) -> x < y) $ (zip (head:tail) tail)
