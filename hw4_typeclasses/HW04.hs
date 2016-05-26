{-# OPTIONS_GHC -Wall #-}
module HW04 where

import Data.List

newtype Poly a = P [a]

-- Exercise 1 -----------------------------------------

x :: Num a => Poly a
x = P [0, 1]

-- Exercise 2 ----------------------------------------

instance (Num a, Eq a) => Eq (Poly a) where
    (P [])     == (P [])     = True
    (P (y:ys)) == (P [])     = y == 0 && (P []) == (P ys)
    (P [])     == (P (z:zs)) = z == 0 && (P []) == (P zs)
    (P (y:ys)) == (P (z:zs)) = y == z && (P ys) == (P zs)
 
-- Exercise 3 -----------------------------------------

instance (Num a, Eq a, Show a) => Show (Poly a) where
    show (P p)
        | null ss   = "0"
        | otherwise = intercalate " + " $ reverse ss
        where 
        ss = filter (not . null) $ zipWith toString [0..] p

        toString :: (Num a, Eq a, Show a) => Integer -> a -> String
        toString _ 0 = ""
        toString 0 c = (show c)
        toString 1 c = (showCoefficient c) ++ "x"
        toString e c = (showCoefficient c) ++ "x^" ++ (show e)

        showCoefficient :: (Num a, Eq a, Show a) => a -> String
        showCoefficient 1    = ""
        showCoefficient (-1) = "-"
        showCoefficient y    = show y

-- Exercise 4 -----------------------------------------

plus :: Num a => Poly a -> Poly a -> Poly a
plus (P ys)     (P [])     = P ys
plus (P [])     (P zs)     = P zs
plus (P (y:ys)) (P (z:zs)) = P $ y+z : l where P l = plus (P ys) (P zs)

-- Exercise 5 -----------------------------------------

times :: Num a => Poly a -> Poly a -> Poly a
times (P a) (P b) = sum $ singleTimes 0 a b
    where singleTimes _ _ []      = []
          singleTimes n ys (z:zs) = (P $ replicate n 0 ++ map (z*) ys) : (singleTimes (n+1) ys zs)

-- Exercise 6 -----------------------------------------

instance Num a => Num (Poly a) where
    (+) = plus
    (*) = times
    negate (P ys) = P $ map negate ys
    fromInteger y = P [fromInteger y]
    -- No meaningful definitions exist
    abs    = undefined
    signum = undefined

-- Exercise 7 -----------------------------------------

applyP :: Num a => Poly a -> a -> a
applyP (P p) xx = horner xx $ reverse p
    where horner _ []     = 0
          horner y (c:cs) = (horner y cs) * y + c

-- Exercise 8 -----------------------------------------

class Num a => Differentiable a where
    deriv  :: a -> a
    nderiv :: Int -> a -> a
    nderiv 0 f = f
    nderiv n f = deriv $ nderiv (n-1) f

-- Exercise 9 -----------------------------------------

instance Num a => Differentiable (Poly a) where
    deriv (P p) = P $ zipWith mul [1..] $ tail p
        where mul :: Num a => Integer -> a -> a
              mul y z = (fromInteger y) * z

