-- Total grade = 25+23+21+21 = 90

class Fluffy f where
  furry :: (a -> b) -> f a -> f b


-- 1.a Grade 25/25
-- wrote excectly that in the quiz (wrote another solution they both work)
-- 25/25
instance Fluffy ((->) t) where

  furry = (.)

-- 1.b Grade 23 / 25
-- write a program (not completely trivial) using furry of ->
-- i wrote it exactly the way it would be in the ghci in the quiz so i'll take off 2 points for 
-- not writing it like a function. as i said in the quiz the output of programA 2 = 17. so  23/25 

programA:: Int -> Int
programA a = furry f1 f2 a where
	f1 = (+5)
	f2 = (+10)



newtype State s a = State {

  state :: (s -> (s, a))

}

class Misty m where
  banana :: (a -> m b) -> m a -> m b

  unicorn :: a -> m a

-- 2.a Grade 21/25
-- in unicorn and banana forgot the state but its working withut the state as well so -2 for that
-- forgot the second state after the in which will make it crash so -2.
-- total 21/25
-- 
instance Misty (State t) where

    banana f ma = State (\s -> let (s', a) = (state ma s) in state (f a) s')

    unicorn a = State (\x -> (x, a))



-- 2.b Grade: 20 / 25

-- write a program (not completely trivial) using banana and unicorn of state
-- i could not put it in a function since these are let commands
-- they work - i tested them in the ghci
-- they are almost what i wrote in the quiz.
-- but i did try to get a very complicated compisition to work to a tuple and couldent because of
-- misty type has no attibute Int so +1 for the effort :)

let a = unicorn 5
let f = banana a


