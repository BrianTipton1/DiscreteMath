module Main where
-- The Euclidean Algorithm to solve for the gcd of two numbers
-- while y != 0
-- r = x mod y
-- x = y 
-- y = remainder
-- endwhile
-- return x

main :: IO ()
main =  do 
    putStrLn "Input two numbers to get the gcd: "
    input <- getLine
    let split = words input
    let x = read(head split) :: Integer
    let y = read(split!!1) :: Integer

    let a = split!!0
    let b = split!!1
    putStr ("The greatest common divisor of " ++ a ++ " and " ++ b ++ " is: "  )
    print(euc' x y)


euc' x y
  | y == 0 = x
  | otherwise = do 
    let r = x`mod`y
    euc' y r