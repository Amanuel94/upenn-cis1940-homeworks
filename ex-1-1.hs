{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
toDigits :: Integer -> [Integer]
toDigitsRev :: Integer -> [Integer]
reverseList :: ([Integer], Int) -> [Integer]
reverse_ :: [Integer] -> [Integer]
leng :: ([Integer], [Integer], Int) -> Int
length_ :: [Integer] -> Int
doubleEveryOther :: [Integer] -> [Integer]
doubler:: ([Integer], Int) -> [Integer]
sumDigits :: [Integer] -> Integer
sum_ :: ([Integer], Int, Integer) -> Integer
validate :: Integer -> Bool



------------------------------------------------
leng (a, b, n)
    | a == b = n
    | otherwise = leng(a,
    reverseList ((a!!n) : reverseList((b, n)), n+1), n+1)
length_ a = leng(a, [], 0)
------------------------------------------------
reverseList (a, n)
    | n == 0 = []
    | otherwise = (a !! (n-1)) : reverseList((a, n-1))
reverse_ a =  reverseList(a, length_ a)
------------------------------------------------
-- Exercise 1  
toDigitsRev n
    | n <= 0  = []
    | n `div` 10 == 0 = n:[]
    | otherwise = (n `mod` 10): toDigitsRev(n `div` 10)
toDigits n = reverseList(toDigitsRev(n), leng(toDigitsRev(n), [], 0))
------------------------------------------------
--Exercise 2
doubleEveryOther a = reverse_(doubler (a, 0))
doubler (a, n)
    | n == length_ a = []
    | n `mod` 2 == 0 = a !! (length_ a -1-n) : doubler (a, n+1)
    | n `mod` 2 == 1 = 2*(a !! (length_ a -1-n)):doubler (a, n+1)
------------------------------------------------
--Exercise 3
sumDigits a = sum_ (a, 0, 0)
sum_ (a, n, count)
    | n == length_ a = count
    | a!!n > 9 =sum_ (a, n+1, ((a!!n) `mod` 10) + (a!!n `div` 10) + count)
    |otherwise = sum_ (a, n+1, (a!!n) + count)

------------------------------------------------
--Exercise 4
validate n = (sumDigits(doubleEveryOther(toDigits n))) `mod` 10 == 0

------------------------------------------------


leng_mv (a, b, n)
    | a == b = n
    | otherwise = leng_mv(a, reverseList_mv((a!!n) : reverseList_mv((b, n)), n+1), n+1)
length_mv a = leng_mv(a, [], 0)

reverseList_mv (a, n)
    | n == 0 = []
    | otherwise = (a !! (n-1)) : reverseList_mv((a, n-1))
reverse_mv a =  reverseList_mv(a, length_mv a)
------------------------------------------------

--Excercise 5

type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
append_mv :: [Move]-> Move -> [Move]
append_mv list move = reverse_mv (move: reverse_mv list)

extendd_mv :: [Move] -> [Move]-> Int ->[Move]
extend_mv :: [Move] -> [Move]->[Move]
extendd_mv a b n
    | n == length_mv b  = a
    | otherwise = append_mv (extendd_mv a b (n+1)) (b!!n)

extend_mv a b =  extendd_mv a (reverse_mv b) 0

hanoi n peg1 peg2 peg3  
    | n == 1 =  [(peg1, peg2)]
    | otherwise = extend_mv (append_mv (hanoi (n-1) peg1 peg3 peg2) (peg1, peg2)) (hanoi (n-1) peg3 peg2 peg1)




main :: IO ()
main = do
    print(hanoi 3 "a" "b" "c")




