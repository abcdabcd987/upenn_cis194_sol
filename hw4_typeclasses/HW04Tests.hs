-- CIS 194, Spring 2015
--
-- Test cases for HW 04

module HW04Tests where

import HW04
import Testing


-- Exercise 1 -----------------------------------------

ex1Tests :: [Test]
ex1Tests = []

-- Exercise 2 -----------------------------------------

ex2Tests :: [Test]
ex2Tests = [ testF2 "equality test" (==)
             [ (P [1, 2, 3], P [1, 2, 3], True)
             , (P [1, 2, 3], P [1, 2, 4], False)
             , (P [1, 2, 3], P [1, 2], False)
             ]
           , testF2 "non-equality test" (/=)
             [ (P [1, 2], P [1, 2, 3], True)
             ]
           ]

-- Exercise 3 -----------------------------------------

ex3Tests :: [Test]
ex3Tests = [ testF1 "show test" show
            [ (P [0], "0")
            , (P [0, 0, 0], "0")
            , (P [1, 0, 0], "1")
            , (P [-1, 0, 0], "-1")
            , (P [0, 1, 0, -1, 2, 0], "2x^4 + -x^3 + x")
            , (P [1, 1, 0, -1, 2, 0], "2x^4 + -x^3 + x + 1")
            ]
           ]

-- Exercise 4 -----------------------------------------

ex4Tests :: [Test]
ex4Tests = [ testF2 "plus test" (+)
             [ (P [5, 0, 1], P [1, 1, 2], P [6, 1, 3])
             , (P [1, 0, 1], P [1, 1],  P [2, 1, 1])
             ]
           ]

-- Exercise 5 -----------------------------------------

ex5Tests :: [Test]
ex5Tests = [ testF2 "times test" (*)
             [ (P [1, 1, 1], P [2, 2], P [2, 4, 4, 2])
             , (P [1, 2, 3], P [4, 5], P [4, 13, 22, 15])
             ]
           ]

-- Exercise 6 -----------------------------------------

ex6Tests :: [Test]
ex6Tests = [ testF1 "negate test" negate
             [ (P [1, 2, 3], P [-1, -2, -3])
             ]
           , testF1 "fromInteger test" fromInteger
             [ (5, P [5])
             ]
           ]

-- Exercise 7 -----------------------------------------

ex7Tests :: [Test]
ex7Tests = [ testF2 "apply test" applyP
             [ (x^2 + 2*x + 1, 1, 4)
             , (x^2 + 2*x + 1, 2, 9)
             ]
           ]

-- Exercise 8 -----------------------------------------

ex8Tests :: [Test]
ex8Tests = [ testF2 "nderiv test" nderiv
             [ (3, x^5 + x^4 + x^3 - 6, P [6, 24, 60])
             ]
           ]

-- Exercise 9 -----------------------------------------

ex9Tests :: [Test]
ex9Tests = [ testF1 "deriv test" deriv
             [ (x^5 + x^4 + x^3 - 6, P [0, 0, 3, 4, 5])
             ]
           ]

-- All Tests ------------------------------------------

allTests :: [Test]
allTests = concat [ ex1Tests
                  , ex2Tests
                  , ex3Tests
                  , ex4Tests
                  , ex5Tests
                  , ex6Tests
                  , ex7Tests
                  , ex8Tests
                  , ex9Tests
                  ]
