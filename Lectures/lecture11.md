## Assignment 1 submissions: overview

- 155 of 157 enrolled students submitted either by the deadline, or using a few late days
- Heads up: Some people are now out of late days
  - You can see how many late days you have left on Canvas
- Most submissions passed all the tests
 - Of those that failed tests, `validateCardNumber` failed tests most often
- But, as with Assignment 0, passing the tests is a very low bar:
  - It doesn't mean your code is correct
  - It doesn't mean your code can't be dramatically simplified
  
## Advice: Read the assignment description carefully

An example of something some folks seemed to miss in the assignment description (for the `palindrome` problem):

> You can (and should) use `listReverse` as part of your implementation; if you find yourself writing more than one short line of code, you should reconsider your approach.
  
Some `palindrome` implementations were more complicated than necessary!
  
## Advice: Use pattern matching
  
Quoting from the assignment description:

> You are allowed to use any library function on integers, but only the following library functions when dealing with lists:
> 
> * `length :: [a] -> Int`, to compute the number of elements in a list;
> * `(++) :: [a] -> [a] -> [a]`, to combine two lists sequentially into one; and
> * `(==) :: [a] -> [a] -> Bool`, to check whether two lists are identical.
> 
> You may also use the list constructors `[]` and `(:)`; in fact, you won't be able to complete the assignment without using them.

- The main reason these rules are in place is to **encourage you to use pattern matching**
  - Why? Because we will write programs that operate on ASTs a lot, and typically the best (or only) way to do that is via pattern matching
- Three submissions used the `null`/`head`/`tail` library functions, which is against the rules and unnecessary
  - Pattern match against `[]` and `(:)` and you will completely avoid the need for these
- Another three submissions used the `init`/`last` library functions -- also against the rules, and also unnecessary
  - You don't need these if you use pattern matching, the allowed list functions, and the list constructors

## Advice: Use a Haskell linter

- A linter is a simple tool that makes code improvement suggestions
- The Haskell linter `hlint` is great, and the VS Code extension for Haskell has `hlint` integrated
- I use it in class all the time, and you should too!
- Some students are already using it
  - One person wrote in their integrity statement about how their linter suggested `foldr` for `digitsOfInts`
- An example of something that `hlint` would catch:
  - 30 submissions had something along the lines of `if expr then True else False`
  - This is bad style; in Haskell it can always be replaced with just `expr`!
  - Especially common on `palindrome` and `validateCardNumber`
  
## Something that makes me sad

The implementation of `validateCardNumber` that someone submitted:
```
validateCardNumber :: Integer -> Bool
validateCardNumber xs = 
  if sumList(doubleEveryOther(digitsOfInts(digitsOfInt(xs)))) == 131
  then True
  else False
```
The worst part is that it passes the tests 😭

## Integrity statements overview

- Nearly every submission had a meaningful integrity statement that satisfies the course policies
- 5% of submissions (8 people) had a problematic or missing integrity statement
- 6% of submisssions (10 people) violated the course academic integrity policy
- We are in the process of flagging these as "Missing" on Canvas
- Some integrity statements we judged to be borderline -- didn't give *exact* LLM prompts, or said things like "I used stackoverflow" but without including links or specifics
  - Next time, **be specific**!

## Ways people got help

- 37 submissions mentioned getting help from tutors, TAs, office hours, lecture, etc.
- 10 submissions used the "Learn You a Haskell for Great Good!" book
  - Some said they found it from my Zulip post, and some apparently discovered it independently
- 34 submissions mentioned generative AI tools
- 50 mentioned various websites that were helpful
  - 3 people mentioned the same video: [How to read Haskell code (in 7 minutes)](https://www.youtube.com/watch?v=gK0hMxJhqwM)
- A smattering of people talked to a friend

## Other observations from integrity statements

- People generally thought this assignment was easier than the first one
  ```
  I found this assignment easier than assignment 0.
  ```
- People are still getting used to Haskell syntax and idioms
  ```
  I had fun doing this assignments and used the class resources like what we learned in lecture and the hints that the prof gave at the end of one of the lectures. One thing I struggled with was syntax like remembering to do (x:xs) and not just x:xs and when to use ++ vs :. I also confused `` and '' for using functions like mod and div. But refering to my class notes helped me and testing. 
  ```
- Some people thought the assignment was a fun challenge
  ```
  I enjoyed solving these problems, especially the challenge of doubling every list element from the left.
  ```

