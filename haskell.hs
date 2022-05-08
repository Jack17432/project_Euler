main :: IO ()
main = problemThree

problemOne :: IO ()
problemOne =
    putStr $ show $ sumOfThreeOrFiveMultiplies (1000 - 1)

problemTwo :: IO ()
problemTwo =
    putStr $ show $ 2 + fibSum (1, 2)

problemThree :: IO ()
problemThree =
    putStr $ show $ getLargestPrime (round (sqrt 600851475143)) 600851475143

sumOfThreeOrFiveMultiplies :: Integer -> Integer
sumOfThreeOrFiveMultiplies 0 = 0
sumOfThreeOrFiveMultiplies x
    | x `mod` 3 == 0    = x +   sumOfThreeOrFiveMultiplies (x-1)
    | x `mod` 5 == 0    = x +   sumOfThreeOrFiveMultiplies (x-1)
    | otherwise         =       sumOfThreeOrFiveMultiplies (x-1)

fibSum :: (Integer, Integer) -> Integer
fibSum (x, y)
    | x + y > 4000000   = 0
    | even (x + y)      = (x + y) + fibSum (y, x + y)
    | otherwise         = fibSum (y, x + y)

checkIfPrime :: Integer -> Bool
checkIfPrime x = minimum (map (mod x) [2,3..x-1]) > 0

getLargestPrime :: Integer -> Integer -> Integer
getLargestPrime x y
    | (y `mod` x == 0) && checkIfPrime x    = x
    | x < 3                                 = x
    | otherwise            = getLargestPrime (x - 1) y