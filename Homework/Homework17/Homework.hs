-- Question 1
-- Take the book code exple from the lesson and write a function that asks the user to
-- input an index (1, 2 or 3) and returns the title of the book by using the <$> operator.
-- The code you need is defined below. If it can't get the title write a message.

import Data.Map qualified as Map
import Data.Maybe (fromJust, isJust)

data Book
    = Book
    { price         :: Int
    , yearPublished :: Int
    , title         :: String
    }
    deriving (Show)

book1 =
  Book
    { price = 15,
      yearPublished = 1997,
      title = "Harry Potter and the Philosopher's Stone"
    }

book2 =
  Book
    { price = 17,
      yearPublished = 1998,
      title = "Harry Potter and the Chamber of Secrets"
    }

book3 =
  Book
    { price = 19,
      yearPublished = 1999,
      title = "Harry Potter and the Prisoner of Azkaban"
    }

books :: Map.Map Int Book
books = Map.fromList $ zip [1 .. 3] [book1, book2, book3]
-- >>> books
-- fromList [(1,Book {price = 15, yearPublished = 1997, title = "Harry Potter and the Philosopher's Stone"}),(2,Book {price = 17, yearPublished = 1998, title = "Harry Potter and the Chamber of Secrets"}),(3,Book {price = 19, yearPublished = 1999, title = "Harry Potter and the Prisoner of Azkaban"})]

getTitle :: Int -> String
getTitle index =
    let maybeBook = Map.lookup index books
     in if isJust maybeBook
        then title $ fromJust maybeBook
        else "404 Not Found"
-- >>> getTitle 1
-- >>> getTitle 2
-- >>> getTitle 3
-- >>> getTitle 4
-- "Harry Potter and the Philosopher's Stone"
-- "Harry Potter and the Chamber of Secrets"
-- "Harry Potter and the Prisoner of Azkaban"
-- "404 Not Found"

-- Question 2
-- For the Wrapper example make an instance of Functor for the Wrapper type. Then write
-- a function that asks the user to input a number (can be also decimal), creates a Wraper
-- type from it with (<$) and prints the result of applying add1 to it.

data Wrapper a = Empty | Wrapper a deriving (Show)

instance Functor Wrapper where
    fmap f (Wrapper a) = Wrapper $ f a
    fmap _ _           = Empty

add1 :: Num a => a -> a
add1 n = n + 1

q2 :: (Num n) => n -> Wrapper n
q2 n = add1 <$> (n <$ Wrapper n) -- ??

-- Question 3
-- Implement a linked list data type (a list that is build similar as Haskell lists are -
-- with consing) and then define an Functor instance for this List data type. Then create
-- a list that has this type and apply the add1 function from the previous question to it.

infixr 5 :~
data LinkedList a = O | a :~ (LinkedList a) deriving (Show)
instance Functor LinkedList where
    fmap f (a:~z) = f a :~ fmap f z
    fmap _ _      = O

q3 = 1:~2:~3:~4:~5:~O
-- >>> fmap add1 q3
-- 2 :~ (3 :~ (4 :~ (5 :~ (6 :~ O))))
