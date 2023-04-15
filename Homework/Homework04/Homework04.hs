-- Question 1
-- Lets say you have the nested values defined bellow. How would you get the value of
-- 4 by using only pattern matching in a function?

nested :: [([Int], [Int])]
nested = [([1,2],[3,4]), ([5,6],[7,8])]

q1 ((_, (_ : a : _)) : _) = a
-- >>> q1 nested
-- 4

-- Question 2
-- Write a function that takes a list of elements of any type and, if the list has 3 or more elements, it
-- removes them. Else, it does nothing. Do it two times, one with multiple function definitions and one with
-- case expressions.

q2a :: [a] -> [a]
q2a (_ : _ : _ : _) = []
q2a ls              = ls

q2b :: [a] -> [a]
q2b ls = case ls of
    (_ : _ : _ : _) -> []
    _               -> ls

{-
>>> q2a []
>>> q2a [1]
>>> q2a [1,2]
>>> q2a [1,2,3]
>>> q2a [1,2,3,4]
[]
[1]
[1,2]
[]
[]
-}

{-
>>> q2b []
>>> q2b [1]
>>> q2b [1,2]
>>> q2b [1,2,3]
>>> q2b [1,2,3,4]
[]
[1]
[1,2]
[]
[]
-}

-- Question 3
-- Create a function that takes a 3-element tuple (all of type Integer) and adds them together

q3 :: (Integer, Integer, Integer) -> Integer
q3 (a, b, c) = a + b + c
-- >>> q3 (1, 2, 3)
-- 6

-- Question 4
-- Implement a function that returns True if a list is empty and False otherwise.

q4 :: [a] -> Bool
q4 ls = case ls of
    [] -> True
    _  -> False
-- >>> q4 []
-- >>> q4 [1]
-- True
-- False

-- Question 5
-- Write the implementation of the tail function using pattern matching. But, instead of failing if
-- the list is empty, return an empty list.

q5 :: [a] -> [a]
q5 ls = case ls of
    []      -> []
    (_ : z) -> z
-- >>> q5 [1]
-- >>> tail [1]
-- >>> q5 [1..4]
-- >>> tail [1..4]
-- >>> q5 []
-- []
-- []
-- [2,3,4]
-- [2,3,4]
-- []

-- Question 6
-- write a case expression wrapped in a function that takes an Int and adds one if it's even. Otherwise does nothing. 
-- (Use the `even` function to check if the number is even.)

q6 :: Int -> Int
q6 i = case even i of
    True -> i + 1
    _    -> i
-- >>> q6 6
-- >>> q6 7
-- 7
-- 7
