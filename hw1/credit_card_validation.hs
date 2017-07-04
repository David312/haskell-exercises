{- Card Number Validator -}

{-
  Exercise 1
-}
toDigitsRev :: Integer -> [Integer]
toDigitsRev x
  | x <= 0 = []
  | otherwise = (x `mod` 10) : toDigitsRev (x `div` 10)

toDigits :: Integer -> [Integer]
toDigits x = reverse (toDigitsRev x)

{-
  Exercise 2
-}
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther l@(x:xs)
  | even (length l) = doubleSnds l
  | otherwise = x : doubleSnds xs
  where doubleSnds [] = []
        doubleSnds [x] = [x]
        doubleSnds (x:y:xs) = x*2 : y : doubleSnds xs

{-
  Exercise 3
-}
sumDigits :: [Integer] -> Integer
sumDigits x = sum (map sum (map toDigits x))

{-
  Exercise 4
-}
validate :: Integer -> Bool
validate x = (sumDigits (doubleEveryOther (toDigits x))) `mod` 10 == 0



{- Towers of Hanoi -}

{-
  Exercise 5
-}
type Peg = String
type Move = (Peg, Peg)

-- hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
