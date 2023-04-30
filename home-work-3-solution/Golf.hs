--------------------------------------------------

--Excercise 1

skips:: [a] -> [[a]]
skips li = [[li!!(x-1)| x <- [k, 2*k .. length li]]| k<-[1..length li]]

--------------------------------------------------

--Excercise 2

localMaxima :: [Integer] -> [Integer]
localMaxima li = [li!!x| x <- [1.. length li -2], (li!!x) - li!!(x-1) > 0, li!!x - li!!(x+1)> 0]

--------------------------------------------------

-- Excercise 3

stringify :: Integer-> Char
stringify 0 = ' '
stringify _ = '*'

translate :: Integer-> Integer
translate n
    | n == 0 = 0
    | otherwise = n - 1

printLine :: [Integer] -> [Char]
printLine [0,0,0,0,0,0,0,0,0,0] = [' ']
printLine li = printLine( map translate li)  ++ ['\n'] ++ map stringify li


histogram :: [Integer] -> String
histogram li =
    let arr = [length [x | x <- [1..length li], li!!(x-1) == n] | n <- [0..9]]
        ans = printLine (map fromIntegral arr) ++ ['\n', '=', '=', '=', '=', '=', '=', '=', '=', '=', '\n', '0','1','2','3','4','5','6','7','8','9']
    in ans




