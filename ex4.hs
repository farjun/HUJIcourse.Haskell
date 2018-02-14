import Debug.Trace
class Fluffy f where 
    furry :: (a->b) -> f a -> f b

newtype EitherLeft b a = EitherLeft (Either a b)

newtype EitherRight a b = EitherRight (Either a b)

-- | Q1

instance Fluffy (EitherLeft t) where
    furry f (EitherLeft (Left a)) = (EitherLeft (Left (f a))) 
    furry _ (EitherLeft (Right a)) = (EitherLeft (Right a) )

-- | Q2

instance Fluffy (EitherRight t) where
    furry f (EitherRight (Right a)) = (EitherRight (Right (f a)))
    furry _ (EitherRight (Left a)) = (EitherRight (Left (a)))

class Misty m where
    banana :: (a -> m b) -> m a -> m b
    unicorn :: a -> m a

-- | Q3

    furry' :: (a -> b) -> m a -> m b

    furry' f = banana $ unicorn . f


newtype State s a = State {
    state :: (s -> (s, a))
}

-- | Q4
instance Misty (State s) where
    banana f (State v) = State $ \x -> state ( f . snd . v $ x ) x
    unicorn s = State $ \x -> (x, s)


-- | Q5
fix :: (a -> a) -> a
fix f = let {x = f x} in x

myMap :: (a->b) -> [a] -> [b]
myMap = fix (\self f l -> if null l then [] else f (head l) : self f (tail l))

-- | Q6
myMapWithTrace::(Show a)=>(Show b) => (a->b) -> [a] -> [b]
myMapWithTrace = fix (\self f l -> if null l then [] else trace ("calling f with = next parameter") f (head l) : self f (tail l))
