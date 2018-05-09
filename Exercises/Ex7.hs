{- Exercise 7, due (Nov 17, 11:50pm)

General exercise instructions:
- Exercises must be done individually.
- You may not import any Haskell libraries, unless explicitly told to.
- You may write helper functions freely; in fact, you are encouraged
  to do so to keep your code easy to understand.
- Your grade will be determined by our automated testing.
  You can find some sample tests on the course webpage.
- Submit early and often! MarkUs is rather slow when many people
  submit at once. It is your responsibility to make sure your work is
  submitted on time.
- No late submissions will be accepted!
-}

-- This line creates a module to allow exporting of functions.
-- DON'T CHANGE IT!
module Ex7 (domain, TripleDeep(ShallowEnd, DeepEnd)) where

domain :: Eq a => [(a, b)] -> [a] 
domain ((a,b):xs) = my_function [] ((a,b):xs)

check_membership element (x:xs) = if (element == x) then False else (check_membership element xs)
check_membership element _ = True

my_function init ((a,b):xs) = if (check_membership a init) 
                              then (let init' = a:init
                                   in seq init' a:(my_function init' xs))  
                              else (my_function init xs)
my_function init _ = []

-- TripleDeep is a datatype to define a tree, where each node contains either:
--   - A value of some type a
--   - Three TripleDeep subtrees 
-- Write a Functor instance for TripleDeep.
data TripleDeep a =
      ShallowEnd a
    | DeepEnd (TripleDeep a) (TripleDeep a) (TripleDeep a)
    deriving(Show, Eq)

-- An example of a TripleDeep structure.
tree :: TripleDeep Integer
tree = DeepEnd (ShallowEnd 1) 
               (DeepEnd (ShallowEnd 5) (ShallowEnd 2) (ShallowEnd 3)) 
               (DeepEnd (ShallowEnd 1) (ShallowEnd 2) (ShallowEnd 3))

-- Question 2
-- Write a Functor instance for TripleDee
instance Functor TripleDeep where
    fmap f (ShallowEnd x) = ShallowEnd (f x)
    fmap f (DeepEnd t1 t2 t3) = DeepEnd (fmap f t1) (fmap f t2) (fmap f t3)

