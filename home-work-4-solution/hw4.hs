------------------------------------------------
-- Exercise 1 


fun1 :: [Integer] -> Integer
fun1 = product . map (\x -> x - 2) . filter even

fun2 :: Integer -> Integer
fun2 = sum . filter (even) .takeWhile (>0).iterate (\x -> if even x then x `div` 2 else if x /= 1 then 3 * x + 1 else 0)

main :: IO ()
main = do
    print(fun2 201)

