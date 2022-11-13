module Main where

import System.IO

-- Brute Force function to find the modular inverse 
inverse c m = [x | x <- [0 .. m], let r = c * x `mod` m, r == 1]

prettyFormat c i m
  | null i = "\ESC[31m\nNo inverse for " ++ show c ++ "x≡1`mod`" ++ show m ++ "\ESC[m\STX"
  | otherwise = "\ESC[32m\nThe inverse of " ++ show c ++ "x≡1`mod`" ++ show m ++ " is " ++ show (head i) ++ "\ESC[m\STX"

loop = do
  putStr "Input a constant to find the inverse of: "
  input <- getLine
  putStrLn ""
  let c = read input :: Integer
  putStr "Input a mod: "
  input <- getLine
  putStrLn ""
  let m = read input :: Integer
  let inv = inverse c m
  putStrLn $ prettyFormat c inv m
  putStrLn "\n\nCtrl-C to quit\n"
  loop

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  loop
