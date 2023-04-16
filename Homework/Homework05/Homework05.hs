-- Create a higher-order function that takes 3 parameters: A function and the two parameters that that function takes, and
-- flips the order of the parameters.
-- For example this: `(/) 6 2` returns `3`. But this: `flip' (/) 6 2` returns `0.3333333333`
q1 :: (a -> b -> c) -> b -> a -> c
q1 f a b = f b a

-- >>> q1 (/) 6 2
-- 0.3333333333333333

-- Create the `uncurry'` function that converts a curried function to a function on pairs. So this: `(+) 1 2` that returns `3` can be written as
-- `uncurry' (+) (1,2)` (with the two different arguments inside a pair).
q2 :: (a -> b -> c) -> (a, b) -> c
q2 f (a, b) = f a b

-- >>> q2 (+) (1, 2)
-- 3

-- Create the `curry'` function that converts an uncurried function to a curried function. So this: `fst (1,2)` that returns `1` can be written as
-- `curry' fst 1 2` (with the tuple converted into two different arguments).
q3 :: ((a, b) -> c) -> a -> b -> c
q3 f a b = f (a, b)

-- >>> q3 fst 1 2
-- 1

-- Use higher-order functions, partial application, and point-free style to create a function that checks if a word has an uppercase letter.
-- Start with using just higher-order functions and build from there.
q4 :: String -> Bool
q4 = any (`elem` ['A' .. 'Z'])

-- >>> q4 "ariadyPutra"
-- >>> q4 "ariadyputra"
-- True
-- False

-- Create the `count` function that takes a team ("Red", "Blue", or "Green") and returns the amount of votes the team has inside `votes`.

votes :: [String]
votes = ["Red", "Blue", "Green", "Blue", "Blue", "Red"]

q5 :: String -> Int
q5 = length . (`filter` votes) . (==)

-- >>> q5 "Red"
-- >>> q5 "Blue"
-- >>> q5 "Green"
-- 2
-- 3
-- 1

-- Create a one-line function that filters `cars` by brand and then checks if there are any left.

cars :: [(String, Int)]
cars = [("Toyota", 0), ("Nissan", 3), ("Ford", 1)]

q6 :: String -> Maybe Int
q6 = (`lookup` cars)

-- >>> q6 "Toyota"
-- >>> q6 "Nissan"
-- >>> q6 "Ford"
-- Just 0
-- Just 3
-- Just 1
