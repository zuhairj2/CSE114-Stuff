{- 

Welcome to lecture 6!

Agenda:

- First steps with Haskell
- Working with lists
- Writing functions
- ...with pattern matching
- Quiz/break
- Pattern guards
- Polymorphic types
- Advice for hw1

Working with lists:

Lists can either be:

- Empty
- an element followed by a list

Therefore, Haskell has two *list constructors*:

- The empty list constructor, [], pronounced "nil"
- A constructor that puts together an element with a list,
  written (:), and pronounced "cons"

Whenever we're working with lists, we should consider these two cases.

-}

-- Let's write the `tri` function in Haskell.

-- Non-idiomatic version
-- tri = \n -> if n == 0 then 0 else n + tri (n-1)

-- Slightly more idiomatic
-- tri n = if n == 0 then 0 else n + tri (n-1)

-- Truly idiomatic: use pattern matching
tri :: Int -> Int -- but really, Nat -> Nat
tri 0 = 0
tri n = n + tri (n-1)

-- A version that has nicer behavior if called with a negative number
safeTri :: Int -> Int
safeTri n | n < 0 = error "Please don't call me with arguments less than 0 kthx"
safeTri n = tri n

-- Pattern matching on lists
sprinklesInList :: [String] -> Bool
sprinklesInList []     = False
sprinklesInList (x:xs) = (x == "sprinkles") || sprinklesInList xs

-- This works too
sprinklesInList' :: [String] -> Bool
sprinklesInList' []              = False
sprinklesInList' ("sprinkles":_) = True
sprinklesInList' (_:xs)          = sprinklesInList' xs

-- ["rainbow", "sprinkles", "foo"]
-- x  would match against "rainbow"
-- xs ["sprinkles", "foo"]

f :: [String] -> [String]
f [] = []
f (_:xs) = "kona" : xs

-- Let's write a function that duplicates all elements in a list
duplicateAll :: [a] -> [a]
duplicateAll []     = []
duplicateAll (x:xs) = x : x : duplicateAll xs

-- Homework hints

lastDigit :: Integer -> Integer
lastDigit n = n `mod` 10

allButLastDigit :: Integer -> Integer
allButLastDigit n = n `div` 10
