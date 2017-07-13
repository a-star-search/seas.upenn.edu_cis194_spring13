
--validating credit card numbers
validate :: Integer -> Bool
validate x = ((checksum (toDigits x)) `mod` 10) == 0

checksum :: [Integer] -> Integer
checksum [] = 0
checksum arr =  sum (numberListToDigitList (doubleEveryOther (reverse arr)))

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther arr = zipWith (*) arr (cycle[1, 2])

numberListToDigitList :: [Integer] -> [Integer]
numberListToDigitList [] = []
numberListToDigitList arr = concat( (map (toDigits) arr))

toDigits :: Integer -> [Integer]
toDigits 0 = [0]
toDigits x = toDigitsRecursive x

toDigitsRecursive :: Integer -> [Integer]
toDigitsRecursive 0 = []
toDigitsRecursive x = toDigitsRecursive (x `div` 10) ++ [x `mod` 10]
