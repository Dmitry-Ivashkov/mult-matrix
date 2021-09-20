module Lib
    ( someFunc
    ) where

import GHC.Base

someFunc :: IO ()
someFunc = putStrLn "someFunc"

type Data = Int

--q1 q2
--q4 q3
data QuadTree = Quad QuadTree QuadTree QuadTree QuadTree | Unique Data Int deriving(Show)

instance Eq QuadTree where
  (Unique d1 s1) == (Unique d2 s2) = (s1 == s2) && (d1 == d2)
  (Quad q1_1 q1_2 q1_3 q1_4) == (Quad q2_1 q2_2 q2_3 q2_4) = (q1_1 == q2_1) && (q1_2 == q2_2) && (q1_3 == q2_3) && (q1_4 == q2_4)
  (Quad q1 q2 q3 q4) == (Unique d2 s) = (q1 == u) && (q2 == u) && (q3 == u) && (q4 == u) where
    u = Unique d2 (divInt s 2)
  q1 == q2 = q2 == q1

-- TODO(Show)

size :: QuadTree -> Int
size (Quad q _ _ _) = 2 * (size q)
size (Unique _ int) = int

infixl 7  ***
(***) :: QuadTree -> QuadTree -> QuadTree
(Unique d1 s) *** (Unique d2 _) = Unique (d1 * d2 * s) s
(Quad q1 q2 q3 q4) *** (Unique d2 s) = (Quad (q1 *** u) (q2 *** u) (q3 *** u) (q4 *** u)) where
  u = Unique d2 (divInt s 2)
q1@(Unique _ _) *** q2 = q2 *** q1
(Quad q1_1 q1_2 q1_3 q1_4) *** (Quad q2_1 q2_2 q2_3 q2_4) = (Quad q3_1 q3_2 q3_3 q3_4) where
  q3_1 = q1_1 *** q2_1 +++ q1_2 *** q2_4
  q3_2 = q1_1 *** q2_2 +++ q1_2 *** q2_3
  q3_3 = q1_4 *** q2_2 +++ q1_3 *** q2_3
  q3_4 = q1_4 *** q2_1 +++ q1_3 *** q2_4

infixl 6  +++
(+++) :: QuadTree -> QuadTree -> QuadTree
(Unique d1 s) +++ (Unique d2 _) = Unique (d1 + d2) s
(Quad q1 q2 q3 q4) +++ (Unique d2 s) = (Quad (q1 +++ u) (q2 +++ u) (q3 +++ u) (q4 +++ u)) where
  u = Unique d2 (divInt s 2)
q1@(Unique _ _) +++ q2 = q2 +++ q1
(Quad q1_1 q1_2 q1_3 q1_4) +++ (Quad q2_1 q2_2 q2_3 q2_4) = (Quad (q1_1 +++ q2_1) (q1_2 +++ q2_2) (q1_3 +++ q2_3) (q1_4 +++ q2_4))