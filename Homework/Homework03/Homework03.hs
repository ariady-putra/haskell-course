-- Question 1
-- Write a function that checks if the monthly consumption of an electrical device is bigger, equal, or smaller than the maximum allowed and
-- returns a message accordingly. 
-- The function has to take the hourly consumption of an electrical device, the hours of daily use, and the maximum monthly consumption allowed.
-- (Monthly usage = consumption (kW) * hours of daily use (h) * 30 days).
maximumAllowed :: Integer
maximumAllowed = 270

monthlyUsage :: Integer -> Integer -> Integer
monthlyUsage kW h = kW * h * 30
-- >>> monthlyUsage 3 3
-- 270

q1 :: Integer -> Integer -> String
q1 kW h =
    let usage = monthlyUsage kW h
    in case compare usage maximumAllowed of
        GT -> "Bigger"
        EQ -> "Equal"
        LT -> "Smaller"
-- >>> q1 3 3
-- "Equal"

-- Question 2
-- Prelude:
-- We use the function `show :: a -> String` to transform any type into a String.
-- So `show 3` will produce `"3"` and `show (3 > 2)` will produce `"True"`.

-- In the previous function, return the excess/savings of consumption as part of the message.
q2 :: Integer -> Integer -> String
q2 kW h =
    let usage = monthlyUsage kW h
    in case compare usage maximumAllowed of
        GT -> "Excess " ++ show (usage - maximumAllowed)
        EQ -> "No excess/savings"
        LT -> "Savings " ++ show (maximumAllowed - usage)
-- >>> q2 1 1
-- >>> q2 3 3
-- >>> q2 4 4
-- "Savings 240"
-- "No excess/savings"
-- "Excess 210"

-- Question 3
-- Write a function that showcases the advantages of using let expressions to split a big expression into smaller ones.
-- Then, share it with other students in Canvas.


-- Question 4
-- Write a function that takes in two numbers and returns their quotient such that it is not greater than 1.
-- Return the number as a string, and in case the divisor is 0, return a message why the division is not
-- possible. To implement this function using both guards and if-then-else statements.  


-- Question 5
-- Write a function that takes in two numbers and calculates the sum of squares for the product and quotient
-- of those numbers. Write the function such that you use a where block inside a let expression and a
-- let expression inside a where block. 
