module Lib
    (
    QuadTree,
    createSimpleMatrix,
    concatMatrix,
    zeros,
    ones,
    size,
    (***),
    (+++)
    ) where

import GHC.Base
import GHC.Show
import Text.Printer
import Data.Foldable


type Data = Int

--q1 q2
--q4 q3
data QuadTree = Quad QuadTree QuadTree QuadTree QuadTree | Unique Data Int

instance Eq QuadTree where
  (Unique d1 s1) == (Unique d2 s2) = (s1 == s2) && (d1 == d2)
  (Quad q1_1 q1_2 q1_3 q1_4) == (Quad q2_1 q2_2 q2_3 q2_4) = (q1_1 == q2_1) && (q1_2 == q2_2) && (q1_3 == q2_3) && (q1_4 == q2_4)
  (Quad q1 q2 q3 q4) == (Unique d2 s) = (q1 == u) && (q2 == u) && (q3 == u) && (q4 == u) where
    u = Unique d2 (divInt s 2)
  q1 == q2 = q2 == q1

instance Show QuadTree where
  show q = buildString $ fold $ map (<> newLine) (helper q) where
    helper :: QuadTree -> [StringBuilder]
    helper (Unique d s) = replicate s $ Prelude.foldr (<+>) (head array) (tail array) where
      array = replicate s $ string $ show d
    helper (Quad q1 q2 q3 q4) = (helperConcat q1 q2) ++ (helperConcat q4 q3) where
      helperConcat :: QuadTree -> QuadTree -> [StringBuilder]
      helperConcat q1 q2 = map (\(a,b) -> a <+> b) $ zip (helper q1) (helper q2)

createSimpleMatrix :: Data -> Int -> QuadTree
createSimpleMatrix = Unique

concatMatrix :: QuadTree -> QuadTree -> QuadTree -> QuadTree -> QuadTree
concatMatrix = Quad

zeros :: Int -> QuadTree
zeros = createSimpleMatrix 0

ones :: Int -> QuadTree
ones = createSimpleMatrix 1

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





-- multidimensional implementation

-- Dim >= 2
type Dim = Int
type Size = Int

-- size [MultidimensionalMatrix] == 2^Dim
data MultidimensionalMatrix a = MQuad [MultidimensionalMatrix a] | MUnique Dim Size a

dim :: MultidimensionalMatrix a -> Dim
dim (MUnique d _ _) = d
dim (MQuad arr) = floor $ logBase 2 $ fromIntegral $ length arr

mSize :: MultidimensionalMatrix a -> Size
mSize (MQuad arr) = 2 * (mSize $ head arr)
mSize (MUnique _ s _) = s

add :: (a -> b -> c) -> MultidimensionalMatrix a -> MultidimensionalMatrix b -> MultidimensionalMatrix c
add addElement (MUnique d1 s1 a) (MUnique d2 s2 b) = (MUnique d1 s1 $ addElement a b)
add addElement (MQuad arrA) (MUnique d s b) = MQuad $ map (\mA-> add addElement mA (MUnique d (divInt s 2) b)) arrA
add addElement m1@(MUnique _ _ _) m2 = add (flip addElement) m2 m1
add addElement (MQuad arrA) (MQuad arrB) = MQuad $ zipWith (add addElement) arrA arrB

-- index: from binary array to number
index :: [Int] -> Int
index = indexBase 2

indexBase :: Int -> [Int] -> Int
indexBase base arr = foldr (\a total -> a + base * total) 0 arr

-- arr = fromIndex len n
-- n == index arr
fromIndex :: Int -> Int -> [Int]
fromIndex = fromIndexBase 2

fromIndexBase :: Int -> Int -> Int -> [Int]
fromIndexBase base 0 n = []
fromIndexBase base len n = (mod m base) : (fromIndexBase base (len-1) (divInt n base))

-- how multiply matrix 2^dim and 2^dim
-- (Int, Int) - what index multiply
-- [(Int, Int)] - what index adding and multiply, length [(Int, Int)] == 2 == base
-- [[(Int, Int)]] - for all index in [MultidimensionalMatrix a], length [[(Int, Int)]] == 2^Dim
generateMultiplyArray :: Int -> Int -> Int -> [[(Int, Int)]]
generateMultiplyArray dim a b = generateMultiplyArrayBase 2

generateMultiplyArrayBase :: Int -> Int -> Int -> Int -> [[(Int, Int)]]
generateMultiplyArrayBase base dim a b = undefined
  where dim2 = 2^dim

multiply :: (a -> b -> c) -> (c -> c -> d) -> Int -> Int -> MultidimensionalMatrix a -> MultidimensionalMatrix b -> MultidimensionalMatrix d
multiply multiplyElement addElement firstIndex secondIndex m1 m2 = undefined