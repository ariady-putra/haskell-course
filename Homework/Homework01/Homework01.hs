
-- Question 1
-- Write a multiline comment below.
{-
Multiline
Comment
-}

-- Question 2
-- Define a function that takes a value and multiplies it by 3.
q2 :: Integer -> Integer
q2 = (*3)
-- >>> q2 3
-- 9

-- Question 3
-- Define a function that calculates the area of a circle.
type Radius = Double
type Area = Double
q3 :: Radius -> Area
q3 = (*pi) . (^2)
-- >>> q3 2
-- 12.566370614359172

-- Question 4
-- Define a function that calculates the volume of a cylinder by composing the previous function together with the height of the cylinder. 
type Height = Double
type Volume = Double
q4 :: Radius -> Height -> Volume
q4 = (*) . q3
-- >>> q4 2 3
-- 37.69911184307752

-- Question 5
-- Define a function that takes the height and radius of a cylinder and checks if the volume is greater than or equal to 42.
q5 :: Height -> Radius -> Bool
q5 h r = (q4 r h) >= 42
-- >>> q5 3 2
-- False
