#!/usr/bin/env runhaskell

import System.Random
import Text.Printf
import Control.Monad

main = mapM_ (\n -> (randH >>= putStrLn . line n)) [0..50]

line :: Int -> String -> String
line n s = printf "#toc table tr:nth-child(50n + %i) td:last-child, .chapter:nth-child(50n + %i) { background: %s; }" n (n+2) s

randH :: IO String
randH = do
  r <- randC
  g <- randC
  b <- randC
  return $ printf "#%02x%02x%02x" r g b

randC :: IO Int
randC = do
  c <- randomRIO (0,256) :: IO Float
  return $ floor $ (c + 256) / 2
