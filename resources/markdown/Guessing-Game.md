
----

# Let's Make a Guessing Game

## Solution Toolbox

In order to solve the problem the following toolbox of functions can be used:

---       ---
Tool      Details
---       ---
bla       bla
---       ---

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
    case compare g n of LT -> print "Too low :("  >> game n
                        GT -> print "Too high :(" >> game n
                        EQ -> print "You win!"

~~~
