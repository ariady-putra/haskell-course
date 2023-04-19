

-- Question 1
-- Write a function that takes in an integer n and returns a list of the 
-- first n fibonacci numbers. Use the function scanl to achieve this.
fibonacci :: Integer -> [Integer]
fibonacci n = init $ foldl (\ z _ -> z ++ [sum . take 2 . reverse $ z]) [1] [1..n]
-- >>> fibonacci 8
-- [1,1,2,3,5,8,13,21]

-- Question 2
-- Create a cabal project with an app/ folder that contains Main.hs and a lib/
-- folder that contains Libraries.hs. In the Libraries.hs file specify a function
-- that takes an integer and returns True if it is prime and False if it is not.
-- Then in Main.hs let the program ask the user for a number and let him know
-- wheather it is prime or not by using the module defined in Libraries.hs.
prime :: Integer -> Bool
prime n = n > 1 && all ((0/=) . (mod n)) [2..(div n 2)]
-- >>> filter prime [-100..100]
-- [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97]
