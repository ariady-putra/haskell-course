
-- Question 1
-- Add the type signatures for the functions below and then remove the comments and try to compile.
-- (Use the types presented in the lecture.)

f1 :: Double -> Double -> Double -> Double
f1 x y z = x ** (y/z)
-- >>> f1 3 2 1
-- 9.0

f2 :: Double -> Double -> Double -> Double
f2 x y z = sqrt (x/y - z)
-- >>> f2 4 1 0
-- 2.0

f3 :: Bool -> Bool -> [Bool]
f3 x y = [x == True] ++ [y]
-- >>> f3 False True
-- [False,True]

f4 :: Eq e => [e] -> [e] -> [e] -> Bool
f4 x y z = x == (y ++ z)
-- >>> f4 [1,2] [1] [2]
-- True

-- Question 2
-- Why should we define type signatures of functions? How can they help you? How can they help others?


-- Question 3
-- Why should you define type signatures for variables? How can they help you?


-- Question 4
-- Are there any functions in Haskell that let you transform one type to the other? Try googling for the answer.

-- Question 5
-- Can you also define in Haskell list of lists? Did we showed any example of that? How would you access the inner
-- most elements?
