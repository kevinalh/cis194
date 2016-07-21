-- CIS 194, Spring 2015
--
-- Test cases for HW 01

module HW01Tests where

import HW01
import Testing

-- Exercise 1 -----------------------------------------

testLastDigit :: (Integer, Integer) -> Bool
testLastDigit (n, d) = lastDigit n == d

testDropLastDigit :: (Integer, Integer) -> Bool
testDropLastDigit (n, d) = dropLastDigit n == d

ex1Tests :: [Test]
ex1Tests = [ Test "lastDigit test" testLastDigit
             [(123, 3), (1234, 4), (5, 5), (10, 0), (0, 0)]
           , Test "dropLastDigit test" testDropLastDigit
             [(123, 12), (1234, 123), (5, 0), (10, 1), (0,0)]
           ]

-- Exercise 2 -----------------------------------------

testToRevDigits :: (Integer, [Integer]) -> Bool
testToRevDigits (n, ls) = toRevDigits n == ls

ex2Tests :: [Test]
ex2Tests = [ Test "toRevDigits test" testToRevDigits
            [(1234, [4,3,2,1]), (0, []), (-17, []), (93413, [3,1,4,3,9]),
             (5594589764218858, [8,5,8,8,1,2,4,6,7,9,8,5,4,9,5,5])]
            ]

-- Exercise 3 -----------------------------------------

testDoubleEveryOther :: ([Integer], [Integer]) -> Bool
testDoubleEveryOther (x, y) = doubleEveryOther x == y

ex3Tests :: [Test]
ex3Tests = [Test "doubleEveryOther test" testDoubleEveryOther 
            [([4,9,5,5],[4,18,5,10]), ([1,2,3],[1,4,3]), ([9, 8, 9],[9,16,9])]
            ]

-- Exercise 4 -----------------------------------------

testSumDigits :: ([Integer], Integer) -> Bool
testSumDigits (ls, x) = sumDigits ls == x

ex4Tests :: [Test]
ex4Tests = [Test "testSumDigits test" testSumDigits
            [([10,5,18,4], 19), ([321,2,0], 8), ([10,5,18,4], 19),
            ([8,10,8,16,1,4,4,12,7,18,8,10,4,18,5,10], 80)]
            ]

-- Exercise 5 -----------------------------------------

testLuhn :: (Integer, Bool) -> Bool
testLuhn (x,b) = luhn x == b

ex5Tests :: [Test]
ex5Tests = [Test "testLuhn test" testLuhn
            [(5594589764218858, True), (12345678987654321, False)]
            ]

-- Exercise 6 -----------------------------------------

ex6Tests :: [Test]
ex6Tests = []

-- All Tests ------------------------------------------

allTests :: [Test]
allTests = concat [ ex1Tests
                  , ex2Tests
                  , ex3Tests
                  , ex4Tests
                  , ex5Tests
                  , ex6Tests
                  ]
