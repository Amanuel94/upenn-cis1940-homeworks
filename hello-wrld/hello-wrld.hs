-- data FailableDouble = Failure
--                     | OK Double
--             deriving Show

-- safeDiv :: Double->Double-> FailableDouble
-- safeDiv _ 0 = Failure
-- safeDiv  x y = OK (x/y)

data Person = Student String Int 
            | Teacher String Int
    deriving Show 
    
aman :: Person
aman =  Student "Amanuel Tewodros" 10

getName :: Person -> String
getName(Student a _) =  a
main = do
    print(getName(aman))
    
