module Lecture10 where

import Prelude hiding (filter, map, foldr)

message :: String
message = (\x -> "Welcome to lecture " ++ x ++ "!") "10"

{-
Agenda:

- Higher-order functions
- Some higher-order functions to know
  - filter
  - map
- Break/quiz
- More higher-order functions to know
  - foldr
  - if time: foldl, ...?

-}

{-

Higher-order functions either
- take functions as arguments
- return functions
- or both!

-}

-- We encountered a higher-order function a week ago on the quiz: this `find` function

-- Find the first element in a list that satisfies a given predicate
find :: (a -> Bool) -> [a] -> Maybe a
find predicate [] = Nothing
find predicate (x:xs) = if predicate x then Just x else find predicate xs

-- Let's see an example of how HOFs can help us make our code more concise

-- Take a list of `Int`s and filter out the odd ones
evensOnly :: [Int] -> [Int]
evensOnly []     = []
evensOnly (x:xs) | even x    = x : evensOnly xs
                 | otherwise = evensOnly xs

-- Take a list of `String`s and only keep strings of length 4

fourOnly :: [String] -> [String]
fourOnly []     = []
fourOnly (x:xs) | length x == 4 = x : fourOnly xs
                | otherwise     = fourOnly xs


{-
The only differences between `evensOnly` and `fourOnly` are:
- the type of list elements
- the guard expression
-}

filter :: (a -> Bool) -> [a] -> [a]
filter _ []     = []
filter f (x:xs) | f x       = x : filter f xs
                | otherwise = filter f xs

-- Partial applications of `filter` that specialize it to a particular predicate
evensOnly' :: [Int] -> [Int]
evensOnly' = filter (\x -> even x) 

fourOnly' :: [String] -> [String]
fourOnly'  = filter (\x -> length x == 4)

-- Another famous higher-order function: map

-- Take a list of `Int`s and square all of them
squares :: [Int] -> [Int]
squares []     = []
squares (x:xs) = x*x : squares xs

-- Take a list of `Int`s and produce a list of `Bool`s 
-- that tell us whether the corresponding `Int` was divisible by 3.
divisibleBy3 :: [Int] -> [Bool]
divisibleBy3 []     = []
divisibleBy3 (x:xs) = (x `mod` 3 == 0) : divisibleBy3 xs

-- Take a list of predicates, apply them to the string "foo",
-- and return a list of the results
applyAllPreds :: [String -> Bool] -> [Bool]
applyAllPreds []     = []
applyAllPreds (f:fs) = f "foo" : applyAllPreds fs

{-
What's different about these three functions?

- What gets cons'd on to the recursive call is different
- The types! They all take some kind of list as an argument,
  they all return some (possibly different) kind of list,
  but other than that, they're different.

What's the same: the base case, and the second argument to `(:)` in the recursive case
-}

map :: (a -> b) -> [a] -> [b]
map _ []     = []
map f (x:xs) = f x : map f xs

-- Partial applications of `map`
squares' :: [Int] -> [Int]
squares' = map (\x -> x*x)

divisibleBy3' :: [Int] -> [Bool]
divisibleBy3' = map (\x -> x `mod` 3 == 0)

applyAllPreds' :: [String -> Bool] -> [Bool]
applyAllPreds' = map (\f -> f "foo")

-- Second quiz question

{-

(\w x y z -> w) "oliver"
=b> \x y z -> "oliver"

So this is what we get:
[(\x y z -> "oliver"), (\x y z -> "angie"), (\x y z -> "sunny"), (\x y z -> "coco")]

-}

-- What about boiling down a list to just one value?

-- Take a list of `Int`s and return their sum
ourSum :: [Int] -> Int
ourSum [] = 0
ourSum (x:xs) = x + ourSum xs

-- Takes a list of `String`s and concatenate them
ourConcat :: [String] -> String
ourConcat []     = ""
ourConcat (x:xs) = x ++ ourConcat xs

-- Take a list of lists and return the total number of elements in all the lists
lengthAll :: [[a]] -> Int
lengthAll []     = 0
lengthAll (x:xs) = length x + lengthAll xs

{-
What do these have in common ... and not in common?
- They all take a list argument,
  but the lists are of different types.
- They all have a base case where there's some kind of base value we want to return,
  but it's different each time.
- The all make a recursive call on the tail of the list
- They all combine the result of the recursive call with something,
  but they all do it in different ways. 

So to generalize this, we need...

- A polymorphic list argument, so, something of type `[a]`
- Something to return in the base case, let's call its type `b`
- A function to do the combining.  This function will need to take two arguments:
  - one will need to be the first element of the list, so it has type `a`,
  - one will need to be the result of a recursive call, so it has type `b`.
  And this combining function will also need to return something of type `b`.
-}

-- The legendary `foldr` function, also known as `reduce` to its friends
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f b []     = b
foldr f b (x:xs) = f x (foldr f b xs)

ourSum' :: [Int] -> Int
ourSum' = foldr (+) 0

ourConcat' :: [String] -> String
ourConcat' = foldr (++) ""

lengthAll' :: [[a]] -> Int
lengthAll' = foldr (\x y -> length x + y) 0
