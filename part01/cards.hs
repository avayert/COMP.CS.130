{-
We represent playing cards with (Char, Int) pairs. ‘s’ means spades, ‘h’ hearts,
‘c’ clubs’ and ‘d’ diamonds, with number values going from 2 to 14 (Ace being 14).
Consider a game, where a player is dealt two cards and wins credits based on the following rules:

  • If the player has the Ace of Spades (‘s’, 14), then the player wins 14 credits.
  • Otherwise if the player has two consecutive numbers of the same suit, then the player wins 8 credits.
  • Otherwise if the player has a pair (same number values), then the player wins 6 credits.
  • Otherwise if the player has to consecutive numbers, then the player wins 4 credits.
  • Otherwise if the player has two cards of the same suit, then the player wins 2 credits.
  • Otherwise, the player wins 0 credits.

Write a function credits :: (Char, Int) -> (Char, Int) -> Int that evaluates the given credits.

You can assume that the given cards are real.

Return your solution in a file cards.hs that contains the required function.
-}

credits :: (Char, Int) -> (Char, Int) -> Int
credits _ ('s', 14) = 14
credits ('s', 14) _ = 14
credits (s1, v1) (s2, v2) | ((v1 == v2 + 1 || v2 == v1 + 1) && (s1 == s2)) = 8
credits (_, v1) (_, v2) | (v1 == v2) = 6
credits (_, v1) (_, v2) | (v1 == v2 + 1 || v2 == v1 + 1) = 4
credits (s1, _) (s2, _) | (s1 == s2) = 2
credits _ _ = 0
