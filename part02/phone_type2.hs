{-
Define the types, the record and the function as in Task 2.1 with the following changes:

Now, insted of using type synonyms, define data types CountryCode and PhoneNo so that both of them have a value constructor that takes an integer.
Derive an instance for Eq, Ord and Show for PhoneType.
Derive instances for Eq and Ord for CountryCode and PhoneNo and make Show instances for them so that:
CountryCode: print '+' in front of the number.
PhoneNo: print only the number.
Make a function for both of them (toCountryCode and toPhoneNo) that takes an Integer and throws an error if the integer is negative otherwise it creates the value.

If CountryCode is negative, the error should be "Negative country code" and if PhoneNo is negative, the error should be "Negative phone number" and you should follow these literally to pass the automatic testing.

Again, using the record syntax, define Phone type for phone numbers that has only one value constructor with fields
phoneType :: PhoneType,
countryCode :: CountryCode, (This time the type defined as above)
and phoneNo :: PhoneNo. (This time the type defined as above)

Derive an instance for Eq and Ord for the record, but for Show make it "pretty-print" the infromation in this form:
<country code><space><phone number><space><phone type in parenthesis>
e.g. +358 123456789 (WorkLandline)

Return your code in a file named phone_type2.hs

Note: You need to make both CountryCode and PhoneNo an instance of Num typeclass by defining fromInteger, (+), (-), and (*).

Note: You will probably get some warnings from the compiler for not explicitly implementing abs and signum for Num CountryCode  and Num PhoneNo. You may ignore those.

Note: Another option would be to use newtype keyword instead of data keyword for CountryCode and PhoneNo, but we will get to it later.
-}


data PhoneType = WorkLandline | PrivateMobile | WorkMobile | Other deriving (Show, Eq, Ord)


data CountryCode = CountryCode Integer deriving (Eq, Ord)
instance Show CountryCode where
    show (CountryCode code) = "+" <> show code
instance Num CountryCode where
    fromInteger = toCountryCode

toCountryCode :: Integer -> CountryCode
toCountryCode code
 | code < 0 = error "Negative country code"
 | otherwise = CountryCode code


data PhoneNo = PhoneNo Integer deriving (Eq, Ord)
instance Show PhoneNo where
    show (PhoneNo num) = show num
instance Num PhoneNo where
    fromInteger = toPhoneNo

toPhoneNo :: Integer -> PhoneNo
toPhoneNo num
 | num < 0 = error "Negative phone number"
 | otherwise = PhoneNo num


data Phone = Phone {
    phoneType :: PhoneType,
    countryCode :: CountryCode,
    phoneNo :: PhoneNo
} deriving (Ord, Eq)
instance Show Phone where
    show (Phone ptype code num) = show code <> " " <> show num <> " (" <> show ptype <> ")"

makePhone :: PhoneType -> CountryCode -> PhoneNo -> Phone
makePhone pt cc pn = Phone pt cc pn
