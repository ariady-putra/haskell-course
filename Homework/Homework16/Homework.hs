-- Question 1
-- Define an instance of a semigroup that multiplies the even number data type EvenNr bellow

data EvenNr
    = EvenNr Integer

instance Show EvenNr where
  show (EvenNr n) = show (2 * n)

instance Semigroup EvenNr where
    (EvenNr a) <> (EvenNr b) = EvenNr $ a * b
-- >>> (EvenNr 3) <> (EvenNr 5)
-- 30

-- Question 2
-- Definan an instance of a semigroup that adds time for the Time data type bellow

newtype Hour
    = Hour Int
    deriving (Show)

newtype Min
    = Min Int
    deriving (Show)

newtype Sec
    = Sec Int
    deriving (Show)

instance Num Sec where
    (Sec a) + (Sec b) = Sec $ a + b
    (Sec a) - (Sec b) = Sec $ a - b
    (Sec a) * (Sec b) = Sec $ a * b
    abs (Sec a)       = Sec $ abs a
    signum (Sec a)    = Sec $ signum a
    fromInteger       = Sec . fromInteger

data Time
    = Time (Hour, Min, Sec)
    deriving (Show)

instance Semigroup Time where
    a <> b = secToTime $ (timeToSec a) + (timeToSec b)
-- >>> a = secToTime $ fromInteger 4000
-- >>> b = secToTime $ fromInteger 5000
-- >>> a
-- >>> b
-- >>> c = a <> b
-- >>> c
-- >>> timeToSec c
-- Time (Hour 1,Min 6,Sec 40)
-- Time (Hour 1,Min 23,Sec 20)
-- Time (Hour 2,Min 30,Sec 0)
-- Sec 9000

secToTime :: Sec -> Time
secToTime (Sec x) =
  let h = (x `div` 3600) `mod` 24
      m = (x `mod` 3600) `div` 60
      s = x `mod` 60
   in Time (Hour h, Min m, Sec s)

timeToSec :: Time -> Sec
timeToSec (Time (Hour h, Min m, Sec s)) =
    Sec (h * 3600 + m * 60 + s)

-- Question 3
-- Is it possible to extend the semigroup instance of EvenNr above to a monoid?
-- Does there exist another instance of a semigroup that makes the the collection of even numbers a monoid?
-- If so, implement this semigroup and monoid.
instance Monoid EvenNr where
    mempty = EvenNr 1
-- >>> mappend (EvenNr 3) (EvenNr 5)
-- >>> 2 * 3 * 5
-- >>> mconcat [EvenNr 7, EvenNr 9, EvenNr 11]
-- >>> 2 * 7 * 9 * 11
-- 30
-- 30
-- 1386
-- 1386

-- Question 4
-- Define an instance of a monoid for the Time data type.
instance Monoid Time where
    mempty = secToTime $ Sec 0
-- >>> a = secToTime $ fromInteger 6000
-- >>> b = secToTime $ fromInteger 7000
-- >>> c = secToTime $ fromInteger 8000
-- >>> "a = " ++ show a ++ " = Sec 6000"
-- >>> "b = " ++ show b ++ " = Sec 7000"
-- >>> "c = " ++ show c ++ " = Sec 8000"
-- >>> d = mappend a b
-- >>> e = mconcat [a, b, c]
-- >>> "d = " ++ show d
-- >>> "e = " ++ show e
-- >>> "d =   a `mappend` b   = " ++ (show . timeToSec) d
-- >>> "e = mconcat [a, b, c] = " ++ (show . timeToSec) e
-- "a = Time (Hour 1,Min 40,Sec 0) = Sec 6000"
-- "b = Time (Hour 1,Min 56,Sec 40) = Sec 7000"
-- "c = Time (Hour 2,Min 13,Sec 20) = Sec 8000"
-- "d = Time (Hour 3,Min 36,Sec 40)"
-- "e = Time (Hour 5,Min 50,Sec 0)"
-- "d =   a `mappend` b   = Sec 13000"
-- "e = mconcat [a, b, c] = Sec 21000"
