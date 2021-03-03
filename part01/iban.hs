{-
This task is suitable for practicing guards and splitting your code into smaller functions.

Write a function validate :: String -> Bool that, given a string validates the string as a Finnish IBAN code.

For details, see https://en.wikipedia.org/wiki/International_Bank_Account_Number#Validating_the_IBAN.

You will also need the following information:

  • Length of a Finnish IBAN code is 18.
  • Finnish IBAN begins with the country code FI.
  • All the characters after the country code are digits.

You can assume that the input is without whitespaces.
Submit a file iban.hs containing the required function.
-}

startswith :: String -> String -> Bool
startswith haystack needle = all(\(a, b) -> a == b) $ zip haystack needle

isDigit :: Char -> Bool
isDigit c = '0' <= c && c <= '9'

rotate :: Int -> String -> String
rotate n s = take (length s) (drop n (cycle s))

transformIBAN :: String -> String
transformIBAN "" = ""
transformIBAN (head:tail) | isDigit head = head:transformIBAN(tail)
transformIBAN (head:tail) = (show ((fromEnum head) - 55)) ++ transformIBAN(tail)

calculateIBAN :: String -> Integer
calculateIBAN iban = read . transformIBAN $ rotate 4 iban

validate :: String -> Bool
validate s | length s /= 18 = False
validate s | not (startswith s "FI") = False
validate s | any(\p -> not . isDigit $ p) $ drop 2 s = False
validate s | not (startswith (drop 2 s) ['2', '1']) = False
validate s | (mod (calculateIBAN s) 97) == 1 = True
validate s = False
