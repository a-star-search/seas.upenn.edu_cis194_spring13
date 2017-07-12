
--validating credit card numbers
validate :: [Integer] -> Integer
validate [] = 0
validate arr =  sum (numberListToDigitList (
  doubleElementsInEvenIndeces (
    numberListToDigitList (reverse arr) )))

doubleElementsInEvenIndeces :: [Integer] -> [Integer]
doubleElementsInEvenIndeces [] = []
doubleElementsInEvenIndeces arr = zipWith (*) arr (cycle[1, 2])

numberListToDigitList :: [Integer] -> [Integer]
numberListToDigitList [] = []
numberListToDigitList arr = concat( (map (numberToDigitList) arr))

numberToDigitList :: Integer -> [Integer]
numberToDigitList 0 = [0]
numberToDigitList x = numberToDigitListRecursive x

numberToDigitListRecursive :: Integer -> [Integer]
numberToDigitListRecursive 0 = []
numberToDigitListRecursive x = numberToDigitListRecursive (x `div` 10) ++ [x `mod` 10]
