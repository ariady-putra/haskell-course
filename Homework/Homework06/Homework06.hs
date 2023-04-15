-- Question 1
-- Write a function called `repeat'` that takes a value and creates an infinite list with
-- the value provided as every element of the list.
--
-- >>> repeat 17
--[17,17,17,17,17,17,17,17,17...

q1 :: t -> [t]
q1 v = v : (q1 v)
-- >>> take 9 $ q1 17
-- [17,17,17,17,17,17,17,17,17]

-- Question 2
-- Using the `repeat'` function and the `take` function we defined in the lesson (comes with Haskell),
-- create a function called `replicate'` that takes a number `n` and a value `x` and creates a list
-- of length `n` with `x` as the value of every element. (`n` has to be Integer.)
--
-- >>> replicate 0 True
-- []
-- >>> replicate (-1) True
-- []
-- >>> replicate 4 True
-- [True,True,True,True]

q2 n x
    | n < 1     = []
    | otherwise = x : (q2 (n - 1) x)
-- >>> q2 0 True
-- >>> q2 (-1) True
-- >>> q2 4 True
-- []
-- []
-- [True,True,True,True]

-- Question 3
-- Write a function called `concat'` that concatenates a list of lists.
--
-- >>> concat' [[1,2],[3],[4,5,6]]
-- [1,2,3,4,5,6]

-- >>> q3 [[1,2],[3],[4,5,6]]
-- [1,2,3,4,5,6]
q3 :: [[a]] -> [a]
q3 []      = []
q3 (a : z) = a ++ q3 z

-- Question 4
-- Write a function called `zip'` that takes two lists and returns a list of
-- corresponding pairs (zips them) like this:
--
-- >>> zip' [1, 2] ['a', 'b']
-- [(1,'a'),(2,'b')]
--
-- If one input list is shorter than the other, excess elements of the longer
-- list are discarded, even if one of the lists is infinite:
--
-- >>> zip' [1] ['a', 'b']
-- [(1,'a')]
-- >>> zip' [1, 2] ['a']
-- [(1,'a')]
-- >>> zip' [] [1..]
-- []
-- >>> zip' [1..] []
-- []

q4 :: [a] -> [b] -> [(a, b)]
q4 [] _          = []
q4 _ []          = []
q4 (a:az) (b:bz) = (a, b) : q4 az bz

-- Question 5
-- Create a function called `zipWith'` that generalises `zip'` by zipping with a
-- function given as the first argument, instead of a tupling function.
--
-- > zipWith' (,) xs ys == zip' xs ys
-- > zipWith' f [x1,x2,x3..] [y1,y2,y3..] == [f x1 y1, f x2 y2, f x3 y3..]
--
-- For example, `zipWith' (+)` is applied to two lists to produce the list of
-- corresponding sums:
--
-- >>> zipWith (+) [1, 2, 3] [4, 5, 6]
-- [5,7,9]

q5 :: (a -> b -> c) -> [a] -> [b] -> [c]
q5 _ [] _          = []
q5 _ _ []          = []
q5 f (x:xs) (y:ys) = f x y : q5 f xs ys

-- Question 6
-- Write a function called `takeWhile'` that takes a precate and a list and
-- returns the list up until an element that doesn't satisfy the predicate.
--
-- >>> takeWhile (< 3) [1,2,3,4,1,2,3,4]
-- [1,2]
-- >>> takeWhile (< 9) [1,2,3]
-- [1,2,3]
-- >>> takeWhile (< 0) [1,2,3]
-- []

q6 :: (a -> Bool) -> [a] -> [a]
q6 _ []    = []
q6 f (a:z) = if f a
    then a : q6 f z
    else []

-- Question 7 (More difficult)
-- Write a function that takes in an integer n, calculates the factorial n! and
-- returns a string in the form of 1*2* ... *n = n! where n! is the actual result.

q7factorial :: Integer -> Integer
q7factorial n
    | n < 2     = 1
    | otherwise = n * q7factorial (n - 1)

q7string :: Integer -> String
q7string n
    | n < 2     = "1"
    | otherwise = q7string (n - 1) ++ "*" ++ show n

q7 :: Integer -> String
q7 n = q7string n ++ " = " ++ show (q7factorial n)
-- >>> q7 3
-- "1*2*3 = 6"
-- >>> q7 2
-- >>> q7 1
-- >>> q7 0
-- >>> q7 (-1)
-- "1*2 = 2"
-- "1 = 1"
-- "1 = 1"
-- "1 = 1"

-- Question 8
-- Below you have defined some beer prices in bevogBeerPrices and your order list in
-- orderList + the deliveryCost. Write a function that takes in an order and calculates
-- the cost including delivery. Assume that the two lists have the beers in the same order.

bevogBeerPrices :: [(String, Double)]
bevogBeerPrices =
  [ ("Tak", 6.00),
    ("Kramah", 7.00),
    ("Ond", 8.50),
    ("Baja", 7.50)
  ]

orderList :: [(String, Double)]
orderList =
  [ ("Tak", 5),
    ("Kramah", 4),
    ("Ond", 7)
  ]

deliveryCost :: Double
deliveryCost = 8.50

q8 :: [(String, Double)] -> Double
q8 []            = deliveryCost
q8 ((b, q) : os) = q8 os + case lookup b bevogBeerPrices of
    Just p -> q * p
    _      -> 0
-- >>> q8 orderList
-- 126.0
