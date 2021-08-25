module Exercises where
-- show Fluffy instances
class Fluffy f where
  furry :: (a -> b) -> f a -> f b -- is fmap

-- Exercise 1
-- Relative Difficulty: 1
instance Fluffy [] where
  --furry = map 
  furry _ []     = []
  furry f (x:xs) = f x : furry f xs

-- Exercise 2
-- Relative Difficulty: 1
instance Fluffy Maybe where
  furry f (Just x) = Just . f $ x
  furry _ Nothing  = Nothing 

-- Exercise 3
-- Relative Difficulty: 5
-- furry :: (a -> b) -> (t -> a) -> (t ->  b)
instance Fluffy ((->) t) where
  furry = (.)

newtype EitherLeft b a = EitherLeft (Either a b)
newtype EitherRight a b = EitherRight (Either a b)

-- Exercise 4
-- Relative Difficulty: 5
instance Fluffy (EitherLeft t) where
  furry f (EitherLeft (Left a)) = EitherLeft . Left . f $ a
  furry f (EitherLeft (Right a)) = EitherLeft . Right $ a

-- Exercise 5
-- Relative Difficulty: 5
instance Fluffy (EitherRight t) where
  furry f (EitherRight (Left a)) = EitherRight . Left  $ a
  furry f (EitherRight (Right a)) = EitherRight . Right . f $ a

-- show Additional Misty functions
class Misty m where -- is Monada
  banana :: (a -> m b) -> m a -> m b -- is flip bind   =<<
  unicorn :: a -> m a                -- is return 

-- Exercise 6
-- Relative Difficulty: 3
-- (use banana and/or unicorn)
furry' :: (Misty m) => (a -> b) -> m a -> m b
furry' f {-ma-} = banana (unicorn . f) {-ma-}


-- Exercise 7
-- Relative Difficulty: 2
instance Misty [] where
  banana k ma   = concat $ map k ma 
  unicorn {-x-} = (:[]) {-x-}

-- Exercise 8
-- Relative Difficulty: 2
instance Misty Maybe where
  banana k (Just x) = k x
  banana _ Nothing  = Nothing 
  unicorn {-x-}= Just {-x-}

-- Exercise 9
-- Relative Difficulty: 6
instance Misty ((->) t) where -- reader
  banana k ma = \e -> let a = ma e in (k a) e
  unicorn x   = \_ -> x

-- Exercise 10
-- Relative Difficulty: 6
instance Misty (EitherLeft t) where
  banana k     (EitherLeft (Left a))  = k a
  banana k     (EitherLeft (Right a)) = EitherLeft . Right $ a
  unicorn {-x-} = EitherLeft . Left {-x-}


-- Exercise 11
-- Relative Difficulty: 6
instance Misty (EitherRight t) where
  banana k (EitherRight (Right a)) = k a
  banana k (EitherRight (Left a)) = EitherRight . Left $ a
  unicorn {-x-} = EitherRight . Right {-x-}


-- Exercise 12
-- Relative Difficulty: 3
jellybean :: (Misty m) => m (m a) -> m a
jellybean = banana id

-- Exercise 13
-- Relative Difficulty: 6
apple :: (Misty m) => m a -> m (a -> b) -> m b -- filp <*> Applicative
apple ma mf = banana (\f ->  banana (\a -> unicorn $ f a) ma ) mf   

-- Exercise 14
-- Relative Difficulty: 6
--moppy :: (Misty m) => [a] -> (a -> m b) -> m [b]
moppy [x]    k = banana (\n -> unicorn $ [n]) (k x)
moppy (x:xs) k = apple (moppy xs k) (banana (\n -> unicorn $ (:) $ n) (k x)) 

-- Exercise 15
-- Relative Difficulty: 6
-- (bonus: use moppy)
--sausage :: (Misty m) => [m a] -> m [a]
--sausage = flip moppy id

-- Exercise 16
-- Relative Difficulty: 6
-- (bonus: use apple + furry')
banana2 :: (Misty m) => (a -> b -> c) -> m a -> m b -> m c
banana2 f ma mb = apple mb (furry' (\a ->  f a ) ma) 


-- Exercise 17
-- Relative Difficulty: 6
-- (bonus: use apple + banana2)
banana3 :: (Misty m) => (a -> b -> c -> d) -> m a -> m b -> m c -> m d
banana3 f ma mb mc = apple mc $ banana2 f  ma mb  

-- Exercise 18
-- Relative Difficulty: 6
-- (bonus: use apple + banana3)
banana4 :: (Misty m) => (a -> b -> c -> d -> e) -> m a -> m b -> m c -> m d -> m e
banana4 f ma mb mc md = apple md $ banana3 f ma mb mc

data State s a = State {
  state :: (s -> (s, a))
}

-- Exercise 19
-- Relative Difficulty: 9
-- 
instance Fluffy (State s) where
  furry f ma = State $ \s -> let (s', a) = state ma s in (s', f a)
{-
-- Exercise 20
-- Relative Difficulty: 10
instance Misty (State s) where
  banana = error "todo"
  unicorn = error "todo"
-}
