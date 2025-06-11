module Lecture12 where

import Prelude hiding (foldr, foldl)

message :: String
message = (\x -> "Welcome to lecture " ++ x ++ "!") "12"

{-

Agenda

- foldr vs foldl
- Break/quiz
- Adding variables to our ArithExpr interpreter

-}

-- Here's the definition of foldr that we wrote a week ago:
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f b []     = b
foldr f b (x:xs) = f x (foldr f b xs)

-- Take a list of `Int`s and return their sum
ourSum :: [Int] -> Int
ourSum [] = 0
ourSum (x:xs) = x + ourSum xs

-- In terms of `foldr`
ourSum' :: [Int] -> Int
ourSum' = foldr (+) 0


{-
ourSum' [1, 2, 3, 4]
foldr (+) 0 [1, 2, 3, 4]
(+) 1 (foldr (+) 0 [2, 3, 4])
(+) 1 ((+) 2 (foldr (+) 0 [3, 4]))
(+) 1 ((+) 2 ((+) 3 (foldr (+) 0 [4])))
(+) 1 ((+) 2 ((+) 3 ((+) 4 (foldr (+) 0 []))))
(+) 1 ((+) 2 ((+) 3 ((+) 4 0)))

1 + (2 + (3 + (4 + 0)))
1 + (2 + (3 + 4))
1 + (2 + 7)
1 + 9
10
-}

{-
The "r" in the name `foldr` refers to the fact that we're accumulating the result
by combining the elements of the list starting from the *right*.

To combine from the *left*, we use `foldl`.  And `foldl` is tail-recursive!
-}

-- Tail-recursive version of ourSum:
ourSumTR :: [Int] -> Int
ourSumTR l = helper 0 l
  where 
    helper :: Int -> [Int] -> Int
    helper acc []     = acc
    helper acc (x:xs) = helper (acc + x) xs

-- Can we factor this tail-recursive behavior out into a generic function,
-- like `foldr`?  Yes!  That's what `foldl` does.
foldl :: (b -> a -> b) -> b -> [a] -> b
foldl f acc []     = acc
foldl f acc (x:xs) = foldl f (f acc x) xs

-- We can now implement `ourSumTR` with `foldl`
ourSumTR' :: [Int] -> Int
ourSumTR' = foldl (+) 0

{-
ourSumTR' [1, 2, 3, 4]
foldl (+) 0 [1, 2, 3, 4]
foldl (+) (0 + 1) [2, 3, 4]
foldl (+) ((0 + 1) + 2) [3, 4]
foldl (+) (((0 + 1) + 2) + 3) [4]
foldl (+) ((((0 + 1) + 2) + 3) + 4) []
(((0 + 1) + 2) + 3) + 4
((1 + 2) + 3) + 4
(3 + 3) + 4
6 + 4
10

This version is tail-recursive, and notice that we accumulated the sum from the *left*,
instead of from the *right*, this time.

But what we would've really wanted would've been to have the accumulator argument
be evaluated eagerly, like this.  This is actually what `foldl'` from the Prelude does, btw.

ourSumTR' [1, 2, 3, 4]
foldl (+) 0 [1, 2, 3, 4]
foldl (+) 1 [2, 3, 4]
foldl (+) 3 [3, 4]
foldl (+) 6 [4]
foldl (+) 10 []
10

Look up `foldl` and `foldl'` in the documentation for more discussion of this!
-}

{-
Quiz:

foldl' (++) "" ["sunny", "coco", "toby", "pinky"]
foldl' (++) "sunny" ["coco", "toby", "pinky"]
foldl' (++) "sunnycoco" ["toby", "pinky"]
foldl' (++) "sunnycocotoby" ["pinky"]
foldl' (++) "sunnycocotobypinky" []
"sunnycocotobypinky"

(This is what it would look like with `foldl'`.  If we just used `foldl`, the result
would be the same but the evaluation steps would happen in a different order)
-}

-- Adding variables to our `ArithExpr` language

data ArithExpr = Plus ArithExpr ArithExpr
               | Minus ArithExpr ArithExpr
               | Times ArithExpr ArithExpr
               | IfZero ArithExpr ArithExpr ArithExpr
               | Leaf Int
               | Var String
  deriving (Show)

-- [("x", 3) ("y", 4)]
type Env = [(String, Int)]

interp :: Env -> ArithExpr -> Maybe Int
interp _   (Leaf n) = Just n
interp env (Var s) = lookup s env
interp env (Plus e1 e2) = case (interp env e1, interp env e2) of
    (Just v1, Just v2) -> Just (v1 + v2)
    _                  -> Nothing 
interp env (Minus e1 e2) = case (interp env e1, interp env e2) of
    (Just v1, Just v2) -> Just (v1 - v2)
    _                  -> Nothing
interp env (Times e1 e2) = case (interp env e1, interp env e2) of
    (Just v1, Just v2) -> Just (v1 * v2)
    _                  -> Nothing
interp env (IfZero e1 e2 e3) = case interp env e1 of
    Just 0 -> interp env e2
    Just n -> interp env e3
    _      -> Nothing