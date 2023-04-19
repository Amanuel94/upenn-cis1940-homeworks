-- data FailableDouble = Failure
--                     | OK Double
--             deriving Show

-- safeDiv :: Double->Double-> FailableDouble
-- safeDiv _ 0 = Failure
-- safeDiv  x y = OK (x/y)

-- data Person = Student String Int 
--             | Teacher String Int
--     deriving Show 
    
-- aman :: Person
-- aman =  Student "Amanuel Tewodros" 10

-- getName :: Person -> String
-- getName(Student a _) =  a
-- a :: [Integer]
-- a = [1,2,3,4]

main = do
    contents <- readFile "er.log"
    print (lines contents)
    
