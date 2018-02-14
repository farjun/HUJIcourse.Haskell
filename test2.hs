import Data.Maybe

class Fluffy f where

  furry ::(a -> b) -> f a -> f b

-- Exercise 1
-- Relative Difficulty: 1
instance Fluffy [] where
  furry = map

-- Exercise 2
-- Relative Difficulty: 1
instance Fluffy Maybe where
  furry f (Just a) = Just (f a)
  furry f Nothing = Nothing

-- Exercise 3
-- Relative Difficulty: 5
instance Fluffy ((->) t) where
  furry a2b x2a = a2b.x2a


newtype EitherLeft b a = EitherLeft (Either a b)
newtype EitherRight a b = EitherRight (Either a b)

-- Exercise 4
-- Relative Difficulty: 5
instance Fluffy (EitherLeft t) where
  furry f (EitherLeft (Left a)) = EitherLeft (Left (f a))
  furry _ (EitherLeft (Right b)) = EitherLeft (Right b)


-- Exercise 5
-- Relative Difficulty: 5
instance Fluffy (EitherRight t) where
  furry f (EitherRight (Right a)) = EitherRight (Right (f a))
  furry _ (EitherRight (Left b)) = EitherRight (Left b)


class Misty m where
  banana :: (a -> m b) -> m a -> m b
  unicorn :: a -> m a


  -- Exercise 6
  -- Relative Difficulty: 3
  -- (use banana and/or unicorn)
  furry' :: (a -> b) -> m a -> m b
  furry' f a = banana (unicorn.f) a

-- Exercise 7
-- Relative Difficulty: 2
instance Misty [] where
  banana = concatMap
  unicorn a = [a]

instance Misty Maybe where
	banana f (Just a) = f a
	banana _ Nothing = Nothing
	unicorn a = (Just a)

-- Exercise 9
-- Relative Difficulty: 6
instance Misty ((->) t) where
  banana f  ma mb = f (ma mb) mb
  unicorn = const

-- Exercise 10
-- Relative Difficulty: 6
instance Misty (EitherLeft t) where
  banana f (EitherLeft (Left a)) = f a
  banana f (EitherLeft (Right a)) = (EitherLeft (Right a)) 

  unicorn a = EitherLeft(Left a)


-- Exercise 11
-- Relative Difficulty: 6
instance Misty (EitherRight t) where
  banana f (EitherRight (Right b)) = f b
  banana f (EitherRight (Left a))  = EitherRight . Left $ a
  
  unicorn = EitherRight . Right
-- ====================================
-- class Misty m where
--   banana :: (a -> m b) -> m a -> m b
--   unicorn :: a -> m a

-- class Fluffy f where
  -- furry ::(a -> b) -> f a -> f b
  
-- furry' :: (a -> b) -> m a -> m b

-- ====================================


-- Exercise 12
-- Relative Difficulty: 3
jellybean :: (Misty m) => m (m a) -> m a
jellybean = banana id


-- Exercise 13
-- Relative Difficulty: 6
apple :: (Misty m) => m a -> m (a -> b) -> m b
apple ma f = furry' (jellybean f) ma



-- ====================================================================================
data Person = Person{firstName::String,lastName::String,age::Int} deriving (Show)

class ClassPerson a b where 
  getAge::Person->a
  getLastNme::Person->b


instance ClassPerson Int String  where
  getAge = error ""
  getLastNme = error ""
-- getAge::Person->Int
-- getAge (Person _ _ age) = age

-- getLastNme::Person->String
-- getLastNme (Person _ lastName _) = lastName


-- ======================================================================================


