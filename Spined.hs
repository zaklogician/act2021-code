{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}


module Spined where

{-
A generic algorithm for computing the triangulation functor.

Consider a measurable spined category where
1. All Hom-sets are finite and enumerable;
2. Equality of morphisms is decidable and composition is computable;
3. Proxy pushouts are computable;
4. The order of elements is decidable;
5. There are finitely many non-isomorphic objects of each order.
We call such a measurable spined category a computable spined category (CsCat).

From the four assumptions above, decidability of isomorphism follows:
just enumerate Hom(A,B) and Hom(B,C), and see check if any of their
elements compose to the identity.

Thus, the code below will assume that the category is skeletal (isomorphism is
equality on objects), with decidable equality of objects, and will expunge all
notions (composition, etc.) that are unnecessary to compute the triangulation
functor under these assumptions from the type class defined below.
-}

class (Eq obj) => CsCat obj mor | obj -> mor where
  hom :: obj -> obj -> [mor]
  spine :: Int -> obj
  proxy :: mor -> mor -> obj
  order :: obj -> Int
  sfun :: obj -> Int

{-
We use the equivalence of chordal and pseudo-chordal triangulation, along with
the fact that an object S of order n cannot have a minimum-width chordal
decomposition H that embeds \Omega_m for m > n inside it. Otherwise, we would
have n < m <= \Delta[H] <= \Delta[\Omega_n] = n. Thus, a chordal completion
of S must be built recursively, out of the first n objects of the spine.
We call such objects n-subchordal.

Since there are finitely many objects of each order, we can enumerate the
n-subchordal objects for each n by starting with the first n elements of the
spine, then closing under proxy pushouts until we stabilize.
-}

subchord :: (CsCat obj mor) => Int -> [obj]
subchord n = subchord' s0 where
  s0 = [spine i | i <- [1..n]]
  omegaN = last s0
  expand ch =
    [ pfg
    | omega <- s0
    , a <- ch
    , b <- ch
    , f <- hom omega a
    , g <- hom omega b
    , let pfg = proxy f g
    , not.null $ hom pfg omegaN
    ]
  subchord' ch = if ch' ~= ch then ch else subchord' ch' where
    ch' = expand ch ++ ch
    (~=) xs ys = all (\x -> elem x ys) xs && all (\y -> elem y xs) ys

{-
The triangulation functor then assigns the width of the smallest
n-subchordal completion to each object.
-}

delta :: (CsCat obj mor) => obj -> Int
delta g = minimum [sfun h | h <- subchord (order g), not.null $ hom g h]
