import Language.Haskell.Interpreter
import System.IO

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    
    putStr "Input an integer: "
    n <- read <$> getLine
    
    putStrLn $ "Combinations of " ++ show [[1..n] | _ <- [1..n]] ++ " are:"
    result <- combinationsOf n
    putStrLn $ show result

-- Question 1
-- Rewrite the following code below that used do-notation to equivalent code that does not use do-notation. 

ioExample :: IO ()
ioExample = do
  print "Input number:"
  string <- getLine
  let n = read string
      add1 x = x + 1
  print (add1 n)

ioExample' :: IO ()
ioExample' =
  print "Input number:" >>
  getLine >>= \string ->
  let n = read string
      add1 x = x + 1 in
  print (add1 n)

-- Question 2
-- Write a function that takes in n of type Int and returns a list of type [Int]. The elements of the list
-- are combination counts for lists [1 .. x] where x goes from 1 to n. So the fisrt combination count is for
-- the list [1], the second for the list [1,2] and the last for the list [1..n]. 

-- How to compute a combination count for a list: e.g. the list [1,2] has 4 possible combinations which are: 
-- (1,1) (1,2) (2,1) and (2,2). Do not use your knowledge of mathematics. Do it by computing all combination 
-- pairs and counting them. Use list comprehension to help yourself. 

listComprehension :: Int -> String
listComprehension n =
    let l = foldr (\ x next -> 'x' : show x ++ comma next) "" ns
        r = foldr (\ x next -> 'x' : show x ++ "<-" ++ show ns ++ comma next) "" ns
    in "[[" ++ l ++ "]|" ++ r ++ "]"
    where
        ns = [1..n]
        comma next = (
            if next == []
                then ""
                else ","
            ) ++ next
-- >>> listComprehension 5
-- "[[x1,x2,x3,x4,x5]|x1<-[1,2,3,4,5],x2<-[1,2,3,4,5],x3<-[1,2,3,4,5],x4<-[1,2,3,4,5],x5<-[1,2,3,4,5]]"

combinationsOf :: Int -> IO [[Int]]
combinationsOf n = do
    interpretation <- runInterpreter . eval $ listComprehension n
    return $ case interpretation of
        Right result -> read result
        _ -> [[]]
-- >>> combinationsOf (-4)
-- >>> combinationsOf 0
-- >>> combinationsOf 1
-- >>> combinationsOf 2
-- >>> combinationsOf 3
-- [[]]
-- [[]]
-- [[1]]
-- [[1,1],[1,2],[2,1],[2,2]]
-- [[1,1,1],[1,1,2],[1,1,3],[1,2,1],[1,2,2],[1,2,3],[1,3,1],[1,3,2],[1,3,3],[2,1,1],[2,1,2],[2,1,3],[2,2,1],[2,2,2],[2,2,3],[2,3,1],[2,3,2],[2,3,3],[3,1,1],[3,1,2],[3,1,3],[3,2,1],[3,2,2],[3,2,3],[3,3,1],[3,3,2],[3,3,3]]

-- Question 3
-- If you succesfully computed the function from Question 2 you should get for n = 5 the list
-- [1,4,9,16,25] which clearly represents the function f(x) = x**2. Write now a function that uses the
-- fittingFunc defined below and finds the best exponent a from the input list of type [Double] that fits 
-- the function f(x) = x**2. So for instance for [1.5, 1.6 .. 2.5] it should return 2.0. Your fitting 
-- check should be done by calculating the mean squared error: (x - x1)^2 + ... + (x - xn)^2

combinationsFor :: Int -> [(Int, Int)]
combinationsFor n = [(x,y) | x <- ns, y <- ns]
    where ns = [1..n]
-- >>> combinationsFor 2
-- [(1,1),(1,2),(2,1),(2,2)]

combinationCountFor :: Int -> [Int]
combinationCountFor n = map (length . combinationsFor) [1..n]
-- >>> combinationCountFor 5
-- [1,4,9,16,25]

fittingFunc :: Double -> Double -> Double
fittingFunc a x = x ** a

