
----

# Let's Make a Guessing Game

~~~{ data-language=haskell data-filter=resources/scripts/check.sh .answer}
{-# LANGUAGE MultiWayIf #-}

module Main where

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
    if | g <  n -> print "Too low :("  >> game n
       | g >  n -> print "Too high :(" >> game n
       | g == n -> print "You win!"
~~~
