import Data.List
main = do
    print "HowManyPrimes? - OnlyInteger"
    inputNumber <- getLine
    let x = (read inputNumber :: Int)
    print (firstNPrimes x)

-- prime - algorithm
primeNumber:: Int -> Bool
primeNumber 2 = True
-- primeNumber x = primNumberRec x (div x 2)
primeNumber x = primNumberRec x (ceiling (sqrt (fromIntegral x)))

primNumberRec:: Int -> Int -> Bool
primNumberRec x y
      |y == 0 = False
      |y == 1 = True 
      |mod x y == 0 = False
      |otherwise = primNumberRec x (y-1)

-- prime numbers till n
primesTillN:: Int -> [Int]
primesTillN n = 2:[ x | x <- [3,5..n], primeNumber x ]


--firstNPrimes
firstNPrimes:: Int -> [Int]
firstNPrimes 0 = []
firstNPrimes n = 2: take (n-1) [x|x <- [3,5..], primeNumber x]
