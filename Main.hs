{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}


module Main where

import Spined

--- We demonstrate the algorithm using the category of Proposition 3.11 of https://arxiv.org/abs/2104.01841

divides :: Int -> Int -> Bool
divides a b = b `mod` a == 0

primes :: [Int]
primes = 2:3:[n | n <- [5,7..], all (\p -> n `mod` p /= 0) $ takeWhile (\p -> p*p <= n) primes]

factorizeWith :: [Int] -> Int -> [Int]
factorizeWith [] n = []
factorizeWith (p:ps) n
  | p `divides` n = p: factorizeWith (p:ps) (n `div` p)
  | otherwise     = factorizeWith ps n

factorize :: Int -> [Int]
factorize n = factorizeWith (takeWhile (<= n) primes) n

maxRun :: [Int] -> Int
maxRun [] = 0
maxRun (x:xs) = maximum [length (takeWhile (== x) (x:xs)), maxRun (dropWhile (== x) (x:xs))]

-- maximal exponent in prime factorization of the given number
maxExp :: Int -> Int
maxExp = maxRun . factorize

-- maximal prime factor of the given number
maxFactor :: Int -> Int
maxFactor 1 = 1
maxFactor x = maximum (factorize x)

data Nat = Nat { unnat :: Int } deriving (Eq, Show)
data NatMor = NatMor { dom :: Int, cod :: Int } deriving (Eq, Show)

instance CsCat Nat NatMor where
  hom (Nat a) (Nat b) = if a `divides` b then [NatMor a b] else []
  spine n = Nat $ product [p^n | p <- takeWhile (<= n) primes]
  order (Nat i) = maximum [maxFactor i, maxExp i]
  proxy (NatMor o1 a) (NatMor o2 b)
    | o1 == o2 = Nat (lcm a b)
    | otherwise = undefined
  sfun (Nat 1) = 1
  sfun (Nat 24300000) = 5 -- opt :)
  sfun (Nat a) = maxExp a

-- it's slow, but it does compute the first few terms
main :: IO ()
main = print [delta (Nat n) | n <- [1..6]]
