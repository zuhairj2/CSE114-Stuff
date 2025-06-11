import System.Posix.Process.ByteString (getProcessTimes)
import Control.Monad.RWS (Ap(Ap))
message :: String
message = "Welcome to lecture 7!"

{-
Agenda:

- Product types, sum types, recursive types
- Some Haskell features we'll encounter along the way: multiple pattern guards, `where` clauses
- Break/quiz
- Defining and working with abstract syntax trees
- Thoughts on hw0

-}

-- Toy database of pets

-- Product type
-- pet's name, owner's name, species, age
data PetRecord = PetRec String String String PetAge
  deriving Show

-- Sum type
data PetAge = IntAge Int | ApproxAge String | Unspecified
  deriving Show

-- We could do this, but it'd be awkward,
-- so let's stick with the built-in list type and say
-- our database is of type [PetRecord]
data PetsDB = EmptyDB | NonEmptyDB PetRecord PetsDB

database :: [PetRecord]
database = [
    PetRec "rainbow"   "lindsey" "cat" (IntAge 3),
    PetRec "sprinkles" "lindsey" "cat" (IntAge 4),
    PetRec "kona"      "niko"    "cat" (ApproxAge "older"),
    PetRec "batman"    "niko"    "cat" (ApproxAge "younger"),
    PetRec "bowie"     "niko"    "cat" (ApproxAge "younger"),
    PetRec "edelman"   "maya"    "cat" Unspecified]

-- `PetRec` is a *constructor* for data of type `PetRecord`

-- `PetRecord` is a product type
-- Every type is a set
-- Product types are cross products of other types

-- getPetsByOwner -- get a list of a particular owner's pets
getPetsByOwner :: [PetRecord] -> String -> [String]
getPetsByOwner []                            _ = []
getPetsByOwner (PetRec petname owner _ _:xs) o =
    if o == owner
        then petname : getPetsByOwner xs o
        else getPetsByOwner xs o

getPetsByOwner' :: [PetRecord] -> String -> [String]
getPetsByOwner' [] _ = []
getPetsByOwner' (PetRec petname owner _ _:xs) o 
  | o == owner = petname : getPetsByOwner' xs o
  | otherwise  = getPetsByOwner' xs o

-- Inductively defined data

{- 

What can a list be?

- empty (in Haskell, the `[]` constructor)
- an element, together with a list (in Haskell, the `(:)` constructor)

-}

-- A recursive type: it refers to itself!
data StringList = Empty | NonEmpty String StringList
  deriving Show

-- Quiz question
-- data Tree = Leaf Int | NodeTree Tree

{-

data Tree = Leaf | Node IntTree
data Tree = Leaf | NodeTree Tree
data Tree = Leaf | Node IntTree Tree
✓ data Tree = Leaf Int | Node Tree Tree
data Tree = Leaf Int | Node Int Tree Tree

-}

{-

A data structure for arithmetic expressions

3 + 3
5 * 6
3 + (4 - 2)
(3 + 4) + (7 - 2)
-}

-- Not quite right
-- data ArithExpr = Plus Int Int | Minus Int Int | Times Int Int

-- Better
data ArithExpr = Plus ArithExpr ArithExpr
               | Minus ArithExpr ArithExpr
               | Times ArithExpr ArithExpr
               | Leaf Int

-- 3 + 3 as an ArithExpr
threePlusThree :: ArithExpr
threePlusThree = Plus (Leaf 3) (Leaf 3)

-- 3 + (4 - 2)
biggerExpression :: ArithExpr
biggerExpression = Plus (Leaf 3) (Minus (Leaf 4) (Leaf 2))

-- These ArithExprs are abstract syntax trees, or ASTs for short.

-- We'll be working with ASTs a lot!

-- Of course, they'll get more complicated...