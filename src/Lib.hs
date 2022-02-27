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
myLength :: [a] -> Int
myLength = sum . map(const 1)

member :: Eq a => a -> [a] -> Bool
member want [] = False
member want (elem:list)|
 want == elem = True |
 otherwise = member want list
 
countOcc :: Eq a => a -> [a] -> Int
countOcc want [] = 0
countOcc want list = sum $
 map(const 1) $
 filter (==want) list

listSum::[Int] -> Int
listSum = foldr (+) 0x0

myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

listUp::Int -> [Int]
listUp n = [1..n]

palindrome :: String -> Bool
palindrome inn = inn == myReverse inn

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
countDown :: Int -> String
countDown n
  | n == 0 = ""
  | n == 1 = show n ++ countDown(n-1)
  | n > 0 = show n ++ " " ++ countDown(n-1)
  | otherwise = "invalid input"

countUp :: Int -> String
countUp n
  | n == 0 = ""
  | n == 1 = countUp(n-1) ++  show n
  | n > 0 =  countUp(n-1) ++ " " ++ show n
  | otherwise = "invalid input"

-- without built-in multiplication
mult :: Int -> Int -> Int
mult x y
 | x <= 0 = 0
 | otherwise = y + mult (x-1) y

-- using built-in multiplication
power :: Int -> Int -> Int
power x n
 | n == 1 = x
 | n == 0 = 1
 | n < 0 = 0
 | otherwise =  x * power x (n-1)
-- using my own multiplication
power' :: Int -> Int -> Int
power' x n
 | n == 1 = x
 | n == 0 = 1
 | n < 0 = 0
 | otherwise =  mult x (power' x (n-1))

