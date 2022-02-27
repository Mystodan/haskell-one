module Lib  (myLength,
             member,
             countOcc,
             listSum,
             myReverse,
             listUp,
             palindrome,
             rotate,
             rotate',
             rotate'',
             countDown,
             countUp,
             mult,
             power,
             power') where

-- Lists

-- | myLength
--
-- Examples:
--
-- >>> myLength [1..5]
-- 5
--
-- >>> myLength []
-- 0
myLength :: [a] -> Int
myLength = sum . map(const 1)

-- | member
--
-- Examples:
--
-- >>> member 1 [1..6]
-- True
--
-- >>> member 1 []
-- False
member :: Eq a => a -> [a] -> Bool
member want [] = False
member want (elem:list)|
 want == elem = True |
 otherwise = member want list
 
-- | countOcc
--
-- Examples:
--
-- >>> countOcc 1 [1..6]
-- 1
--
-- >>> countOcc 1 []
-- 0
countOcc :: Eq a => a -> [a] -> Int
countOcc want [] = 0
countOcc want list = sum $
 map(const 1) $
 filter (==want) list

 
-- | listSum
--
-- Examples:
--
-- >>> listSum [1..3]
-- 6
--
-- >>> listSum []
-- 0
listSum::[Int] -> Int
listSum = foldr (+) 0x0

-- | myReverse
--
-- Examples:
--
-- >>> myReverse [1..3]
-- [3,2,1]
--
-- >>> myReverse []
-- []
myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

-- | listUp
--
-- Examples:
--
-- >>> listUp 3
-- [1,2,3]
--
-- >>> listUp 0
-- []
listUp::Int -> [Int]
listUp n = [1..n]

-- | palindrome
--
-- Examples:
--
-- >>> palindrome "eve"
-- True
--
-- >>> palindrome "palindrome"
-- False
palindrome :: String -> Bool
palindrome inn = inn == myReverse inn

-- | rotate
--
-- Examples:
--
-- >>> rotate 3 [1..6]
-- [4,5,6,1,2,3]
--
-- >>> rotate 0 []
-- []
rotate :: Int -> [a] -> [a]
rotate n list = val ++ rotVal where
  (rotVal, val) = splitAt n list 

{- 
!! Function provided by assignment
  removes(drop n) the n amount of numbers from the start of the inf. looping array(cycle xs).
  then it takes the values from the inf. array and zips it until one of its arguments have been fully traversed
  f.ex: 1,2,3,4 -> 1,2,3,4,1,2,3,4... -> 3,4,1,2,3,4... -> 3,4,1,2
        original     cycle xs               drop n         zipWith ends when xs has been fully traversed
 -}
rotate' :: Int -> [a] -> [a]  
rotate' _ [] = []
rotate' n xs = zipWith const (drop n (cycle xs)) xs

{- 
!! Function provided by assignment
  removes(drop n) the n amount of numbers from the start of the inf. looping array(cycle xs).
  then grabs the array with its original intended size(take(length xs)), containing the new numbers with the numbers switched 
  f.ex: 1,2,3,4 -> 1,2,3,4,1,2,3,4... -> 3,4,1,2,3,4... -> 3,4,1,2
        original     cycle xs               drop n          take (length xs) ()
 -}
rotate'' :: Int -> [a] -> [a]
rotate'' n xs = take (length xs) (drop n (cycle xs)) 


-- Recursion



-- | countDown
--
-- Examples:
--
-- >>> countDown 3 
-- "3 2 1"
--
-- >>> countDown 0 
-- ""
-- 
-- >>> countDown (-1) 
-- "invalid input"
countDown :: Int -> String
countDown n
  | n == 0 = ""
  | n == 1 = show n ++ countDown(n-1)
  | n > 0 = show n ++ " " ++ countDown(n-1)
  | otherwise = "invalid input"

-- | countUp
--
-- Examples:
--
-- >>> countUp 3 
-- "1 2 3"
--
-- >>> countUp 0 
-- ""
-- 
-- >>> countUp (-1) 
-- "invalid input"
countUp :: Int -> String
countUp n
  | n == 0 = ""
  | n == 1 = countUp(n-1) ++  show n
  | n > 0 =  countUp(n-1) ++ " " ++ show n
  | otherwise = "invalid input"

-- | mult
--
-- Examples:
--
-- >>> mult 10 10
-- 100
--
-- >>> mult 0 0
-- 0
mult :: Int -> Int -> Int -- without built-in multiplication
mult x y
 | x <= 0 = 0
 | otherwise = y + mult (x-1) y

-- | power
--
-- Examples:
--
-- >>> power 5 2
-- 25
--
-- >>> power 1 10
-- 1
--
-- >>> power 0 0
-- 1
power :: Int -> Int -> Int -- using built-in multiplication
power x n
 | n == 1 = x
 | n == 0 = 1
 | n < 0 = 0
 | otherwise =  x * power x (n-1)
-- | power'
--
-- Examples:
--
-- >>> power' 5 2
-- 25
--
-- >>> power' 1 10
-- 1
--
-- >>> power' 0 0
-- 1
power' :: Int -> Int -> Int -- using my own multiplication
power' x n
 | n == 1 = x
 | n == 0 = 1
 | n < 0 = 0
 | otherwise =  mult x (power' x (n-1))

