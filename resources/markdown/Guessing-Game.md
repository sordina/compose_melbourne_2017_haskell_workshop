
----

# Let's Make a Guessing Game

<div class="important">

## The Game

Your task is to make a guessing game where the computer comes up with a number,
and you have to make guesses - with the computer telling you if you were too
high, or too low.

## Lexicon

-----------       -------------     ------------
-----------       -------------     ------------
Game              Stack             main
print             System.Random     randomRIO
readLn            compare           case
do                Procedure

## Example Session

```
$ stack exec -- runhaskell Module_1470214643_41323.hs
"Let's play the number guessing game"
"Enter a number"
2
"Too low :("
"Enter a number"
8
"Too high :("
"Enter a number"
5
"Too high :("
"Enter a number"
4
"You win!"
```

</div>

## Solution Toolbox

In order to solve the problem the following should be used:

Tool          | Details
---           | ---
main          | You must define a `main` function in your module
print         | Lets you `print` things to the console
System.Random | import System.Random to get access randomRIO
randomRIO     | Generates a random number from a range as an IO action
readLn        | read a value from the console as a number
compare       | `compare x y` tells you if `x` is LT, EQ, or GT `y`
case          | Dispatch on a value
do            | Execute multiple IO actions

## Thinking Functionally

Try to build the solution piece-by-piece. Examine how you can use recursion in conjunction
with pattern-matching to keep re-running a procedure until a condition is satisfied.

<div class="important">

## Solution

If you're having trouble coming up with the solution, then here are some hints!

### Main

~~~{ data-language=haskell .answer}
main :: IO ()
main = do
    print "Let's play the number guessing game"
    n <- randomRIO (1, 10)
    game n
~~~

### Game

~~~{ data-language=haskell .answer}
game :: Int -> IO ()
game n = do
    print "Enter a number"
    g <- readLn
    case compare g n
      of LT -> tooLow  n
         GT -> tooHigh n
         EQ -> print "You win!"
~~~

### Too Low

~~~{ data-language=haskell .answer}
tooLow :: Int -> IO ()
tooLow n = do
  print "Too low :("
  game n
~~~

### Full Solution

~~~{ data-language=haskell data-filter=resources/scripts/check.sh .answer}
import System.Random

main :: IO ()
main = do
    print "Let's play the number guessing game"
    n <- randomRIO (1, 10)
    game n

game :: Int -> IO ()
game n = do
    print "Enter a number"
    g <- readLn
    case compare g n
      of LT -> tooLow  n
         GT -> tooHigh n
         EQ -> print "You win!"

tooLow :: Int -> IO ()
tooLow n = do
  print "Too low :("
  game n

tooHigh :: Int -> IO ()
tooHigh n = do
  print "Too high :("
  game n
~~~

</div>
