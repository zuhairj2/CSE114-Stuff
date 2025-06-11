{-# LANGUAGE BangPatterns #-}

message :: String
message = "Welcome to lecture 8!"

{-

Agenda:

- Tail recursion
  - ...and how it interacts with laziness in Haskell
  - `where` clauses
- Quiz/break
- Tuples (and pattern-matching on them)
- If time: return to our little `ArithExpr` language
-}

len :: [a] -> Int
len []     = 0 
len (x:xs) = len xs + 1

{-

len [1, 2, 3, 4, 5]
= (len [2, 3, 4, 5]) + 1
= ((len [3, 4, 5]) + 1) + 1
= (((len [4, 5]) + 1) + 1) + 1
= ((((len [5]) + 1) + 1) + 1) + 1
= (((((len []) + 1) + 1) + 1) + 1) + 1
= ((((0 + 1) + 1) + 1) + 1) + 1
= 5

-}

len' :: [a] -> Int
len' l = helper l 0
    where helper :: [a] -> Int -> Int
          helper []     !acc = acc
          helper (x:xs) !acc = helper xs (acc + 1)

{-

len' [1, 2, 3, 4, 5] 0
= len' [2, 3, 4, 5] 1
= len' [3, 4, 5] 2
= len' [4, 5] 3
= len' [5] 4
= len' [] 5
= 5

-}

{-
That was a lie.  It's actually more like this:

len' [1, 2, 3, 4, 5] 0
= len' [2, 3, 4, 5] (0 + 1)
= len' [3, 4, 5] ((0 + 1) + 1)
= len' [4, 5] (((0 + 1) + 1) + 1)
= len' [5] ((((0 + 1) + 1) + 1) + 1)
= len' [] (((((0 + 1) + 1) + 1) + 1) + 1)
= (((((0 + 1) + 1) + 1) + 1) + 1)
= 5

-}

{-

Haskell is a "lazy" language,
meaning that the arguments to functions don't get evaluated immediately.

They only get evaluated when needed.

-}

-- Here's a function where laziness might be our friend.
-- Get the fifth element from a list
fifth :: [a] -> Maybe a
fifth (_:_:_:_:x:_) = Just x
fifth _             = Nothing

-- A couple things that are on the quiz to be familiar with

-- Maybe types

-- The equivalent of "None" and "Some" in Rust, for example 
-- data Maybe a = Nothing | Just a

-- Functions that take functions as arguments

-- Take a function `f` of type `Int -> Int` and an Int `n`,
-- and return the result of applying `f` to `n`
apply :: (Int -> Int) -> Int -> Int
apply f n = f n

-- Find the first element in a list that satisfies a given predicate
find :: (a -> Bool) -> [a] -> Maybe a
find predicate [] = Nothing
find predicate (x:xs) = if predicate x then Just x else find predicate xs

{-

The last thing that `find` does before returning is call `find`.
There's nothing else "waiting around" to be evaluated after the last `find` call returns.
So it is, in fact, tail-recursive.

find (\x -> x `mod` 2 == 0) [1, 2, 3]
= find (\x -> x `mod` 2 == 0) [2, 3]
= Just 2

-}

-- Tuples

{-

Tuples will let you combine things of different types

ghci> :t ("hello", True)
("hello", True) :: (String, Bool)

Tuples' length is "part of their type" 
in the sense that the type (String, String)
is different from (String, String, String).

-}

list :: [(String, Int, String)]
list = [("Lindsey", 42, "bulbasaur"), ("Sylvia", 7, "houndoom"), ("Alex", 42, "charizard")]

lookupPokemon :: [(String, Int, String)] -> String -> String
lookupPokemon []              _ = error "that person wasn't in the list"
lookupPokemon ((name,_,p):xs) n = if n == name then p else lookupPokemon xs n