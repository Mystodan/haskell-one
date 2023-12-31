-- file Spec.hs
import Test.Hspec
import Control.Exception(evaluate)
import Test.DocTest (doctest)
import Lib (myLength,
             member,
             countOcc,
             listSum,
             myReverse,
             listUp,
             palindrome,
             rotate,
             countDown,
             countUp,
             mult,
             power,
             power')




spec_myLength :: Spec
spec_myLength = do
-- myLength
  describe "myLength" $ do
    it "returns the size of a list" $ do
      myLength ([1..6]::[Int]) `shouldBe` (6 :: Int)
      myLength (["A","B","C","D"]::[[Char]]) `shouldBe` (4 :: Int)

    context "if list is empty" $ do
      it "returns 0" $ do
        myLength ([]::[Int]) `shouldBe` (0 :: Int)

spec_member :: Spec
spec_member =  do
-- member
  describe "member" $ do
    it "checks if x is part of list" $ do
      member (1::Int) ([1..6]::[Int]) `shouldBe` (True :: Bool)
      member ("A"::[Char]) (["A","B","C","D"]::[[Char]]) `shouldBe` (True :: Bool)
      member (7::Int) ([1..6]::[Int]) `shouldBe` (False :: Bool)

    context "if list is empty" $ do
      it "returns false" $ do
        member (1::Int) ([]::[Int]) `shouldBe` (False :: Bool)

spec_countOcc ::Spec
spec_countOcc = do
-- countOcc
  describe "countOcc" $ do
    it "counts occurances of x from a provided list" $ do
      countOcc (1::Int) ([1..6]::[Int]) `shouldBe` (1 :: Int)
      countOcc ("A"::[Char]) (["A","B","C","A"]::[[Char]]) `shouldBe` (2 :: Int)
      countOcc (7::Int) ([1..6]::[Int]) `shouldBe` (0 :: Int)

    context "if list is empty" $ do
      it "returns 0" $ do
        countOcc (1::Int) ([]::[Int]) `shouldBe` (0 :: Int)

spec_listSum :: Spec
spec_listSum = do
-- listSum
  describe "listSum" $ do
    it "sums a provided list of ints and returns the number" $ do
      listSum ([1..3]::[Int])  `shouldBe` (6 :: Int)
      listSum ([3,2,1]::[Int]) `shouldBe` (6 :: Int)
      listSum ([9,9,9]::[Int])  `shouldBe` (27 :: Int)

    context "if list is empty" $ do
      it "returns 0" $ do
        listSum ([]::[Int]) `shouldBe` (0 :: Int)

spec_myReverse :: Spec
spec_myReverse = do
-- myReverse
  describe "myReverse" $ do
    it "reverses a given list and returns the reversed list" $ do
      myReverse ([1..6]::[Int]) `shouldBe` ([6,5,4,3,2,1]::[Int])
      myReverse (["A","B","C","A"]::[[Char]]) `shouldBe` (["A","C","B","A"]::[[Char]])
      myReverse ([3,2,1]::[Int])  `shouldBe` ([1,2,3]::[Int])

    context "if list is empty" $ do
      it "returns an empty list []"  $ do
        myReverse ([]::[Int]) `shouldBe` ([]::[Int])

spec_listUp :: Spec
spec_listUp = do
-- listUp
  describe "listUp" $ do
    it "creates and returns a list of ints with elements from 1 to n" $ do
      listUp (6 :: Int) `shouldBe` ([1..6]::[Int])
      listUp (15 :: Int) `shouldBe` ([1..15]::[Int])

    context "if input is 0" $ do
      it "returns an empty list []" $ do
        listUp (0::Int) `shouldBe` ([]::[Int])

spec_palindrome :: Spec
spec_palindrome =  do
 -- palindrome
  describe "palindrome" $ do
    it "checks if string is palindrome" $ do
      palindrome ("racecar"::String) `shouldBe` (True :: Bool)
      palindrome ("kcock"::String)  `shouldBe` (True :: Bool)
      palindrome ("palindrome"::String)  `shouldBe` (False :: Bool)

    context "if input string is empty" $ do
      it "returns True" $ do
        palindrome (""::String) `shouldBe` (True :: Bool)

spec_rotate :: Spec
spec_rotate = do
-- rotate
  describe "rotate" $ do
    it "rotate a given list and returns it" $ do
      rotate (3::Int) ([1..6]::[Int]) `shouldBe` ([4,5,6,1,2,3]::[Int])
      rotate (2::Int) ([1..6]::[Int]) `shouldBe` ([3,4,5,6,1,2]::[Int])
      rotate (5::Int) ([1..6]::[Int]) `shouldBe` ([6,1,2,3,4,5]::[Int])

    context "if list is empty" $ do
      it "returns an empty list []"  $ do
        rotate 0 ([]::[Int]) `shouldBe` ([]::[Int])

spec_countDown :: Spec
spec_countDown = do
-- countDown
  describe "countDown" $ do
    it "counts down from a number and returns it as string" $ do
      countDown (3::Int)  `shouldBe` ("3 2 1"::String)
      countDown (2::Int)  `shouldBe` ("2 1"::String)
      countDown (5::Int)  `shouldBe` ("5 4 3 2 1"::String)

    context "if counting down from negative" $ do
      it "returns invalid input"  $ do
        countDown (-1::Int)  `shouldBe` ("invalid input"::String)
    context "if counting down from 0" $ do
      it "returns invalid an empty string"  $ do
        countDown (0::Int)  `shouldBe` (""::String)

spec_countUp :: Spec
spec_countUp = do
-- countUp
  describe "countUp" $ do
    it "counts up to a number and returns it as string" $ do
      countUp (10::Int)  `shouldBe` ("1 2 3 4 5 6 7 8 9 10"::String)
      countUp (5::Int)  `shouldBe` ("1 2 3 4 5"::String)
      countUp (1::Int)  `shouldBe` ("1"::String)

    context "if counting up to negative" $ do
      it "returns invalid input"  $ do
        countUp (-1::Int)  `shouldBe` ("invalid input"::String)
    context "if counting up to 0" $ do
      it "returns an empty string"  $ do
        countUp 0  `shouldBe` (""::String)

spec_mult :: Spec
spec_mult = do
-- mult
  describe "mult" $ do
    it "multiplication without using built-in multiplication" $ do
      mult (10::Int) (10::Int)   `shouldBe` (100::Int)
      mult (5::Int) (10::Int)  `shouldBe` (50::Int)
      mult (1::Int) (1::Int)   `shouldBe` (1::Int)

    context "multiplying with 0" $ do
      it "returns 0"  $ do
        mult (0::Int) (0::Int)   `shouldBe` (0::Int)

spec_power :: Spec
spec_power =  do
-- power  (using internal multiplication) 
  describe "power" $ do
    it "x to the power of n using built-in multiplication" $ do
      power (5::Int) (2::Int)   `shouldBe` (25::Int)
      power (1::Int) (10::Int)  `shouldBe` (1::Int)
      power (99::Int) (0::Int)   `shouldBe` (1::Int)

    context "x pow n with 0" $ do
      it "returns 0"  $ do
        power (0::Int) (0::Int)   `shouldBe` (1::Int)

spec_power' :: Spec
spec_power'= do
-- power' (using external multiplication)       
  describe "power'" $ do
    it "x to the power of n without using built-in multiplication" $ do
      power' (5::Int) (2::Int)   `shouldBe` (25::Int)
      power' (1::Int) (10::Int)  `shouldBe` (1::Int)
      power' (99::Int) (0::Int)   `shouldBe` (1::Int)

    context "x pow n with 0" $ do
      it "returns 1"  $ do
        power' (0::Int) (0::Int)   `shouldBe` (1::Int)


main :: IO ()
main = do 
  doctest ["-isrc", "src/Lib.hs"]

  hspec $ do
    spec_myLength
    spec_member
    spec_countOcc
    spec_listSum
    spec_myReverse
    spec_listUp
    spec_palindrome
    spec_rotate
    spec_countDown
    spec_countUp
    spec_mult
    spec_power
    spec_power'

