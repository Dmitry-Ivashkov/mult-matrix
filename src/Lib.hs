module Lib
    (
    QuadTree,
    createSimpleMatrix,
    concatMatrix,
    zeros,
    ones,
    size,
    (***),
    (+++),
    MultidimensionalMatrix,
    mCreateSimpleMatrix,
    mConcatMatrix,
    mZeros,
    mOnes,
    mSize,
    add,
    multiply
    ) where

import GHC.Base
import GHC.Show
import Text.Printer
import Data.Foldable
import Data.List.Index


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
data MultidimensionalMatrix a = MQuad [MultidimensionalMatrix a] | MUnique Dim Size a deriving Show

mCreateSimpleMatrix :: Dim -> Size -> a -> MultidimensionalMatrix a
mCreateSimpleMatrix = MUnique

mConcatMatrix :: [MultidimensionalMatrix a] -> MultidimensionalMatrix a
mConcatMatrix = MQuad

mZeros :: Dim -> Size -> MultidimensionalMatrix a
mZeros dim size = createSimpleMatrix dim size 0

mOnes :: Dim -> Size -> MultidimensionalMatrix a
mOnes dim size = createSimpleMatrix dim size 1

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
indexBase base arr = Prelude.foldr (\a total -> a + base * total) 0 arr

-- arr = fromIndex len n
-- n == index arr
fromIndex :: Int -> Int -> [Int]
fromIndex = fromIndexBase 2

fromIndexBase :: Int -> Int -> Int -> [Int]
fromIndexBase base 0 n = []
fromIndexBase base len n = (mod n base) : (fromIndexBase base (len-1) (divInt n base))

allIndex :: Int -> Int -> [[Int]]
allIndex base dim = map (fromIndexBase base dim) [0..(base^dim-1)]

sumNotEmptyList :: (a -> a -> a) -> [a] -> a
sumNotEmptyList add (h:tail) = Prelude.foldr add h tail

-- how multiply matrix 2^dim and 2^dim
-- (Int, Int) - what index multiply
-- [(Int, Int)] - what index adding and multiply, length [(Int, Int)] == 2 == base
-- [[(Int, Int)]] - for all index in [MultidimensionalMatrix a], length [[(Int, Int)]] == 2^Dim
-- a a a
-- a a a
-- a a a dim = 2, base =3
generateMultiplyArray :: Int -> Int -> Int -> [[(Int, Int)]]
generateMultiplyArray = generateMultiplyArrayBase 2

generateMultiplyArrayBase :: Int -> Int -> Int -> Int -> [[(Int, Int)]]
generateMultiplyArrayBase base dim a b = [[(index $ setAt (a-1) i totalIndex, index $ setAt (b-1) i totalIndex) | i <- [0..(base-1)]] | totalIndex <- allIndex base dim]

-- in future: replace 2 to base
multiply :: (a -> b -> c) -> (c -> c -> c) -> Int -> Int -> MultidimensionalMatrix a -> MultidimensionalMatrix b -> MultidimensionalMatrix c
multiply multiplyElement addElement firstIndex secondIndex (MQuad arr1) (MQuad arr2) = MQuad [sumNotEmptyList (add addElement) [multiply multiplyElement addElement firstIndex secondIndex (arr1 !! indexMult1) (arr2 !! indexMult2) | (indexMult1, indexMult2) <- indexSum] | indexSum <- generateMultiplyArray (dim (MQuad arr1)) firstIndex secondIndex]
multiply multiplyElement addElement firstIndex secondIndex (MQuad arr) (MUnique d s a) = MQuad [sumNotEmptyList (add addElement) (replicate 2 $ multiply multiplyElement addElement firstIndex secondIndex mi (MUnique d (divInt s 2) a) ) | mi <- arr]
multiply multiplyElement addElement firstIndex secondIndex m1@(MUnique d s a) m2@(MQuad arr) = multiply (flip multiplyElement) addElement firstIndex secondIndex m2 m1
multiply multiplyElement addElement firstIndex secondIndex (MUnique d1 s1 a)  (MUnique d2 s2 b) = MUnique d1 s1 $ sumNotEmptyList addElement $ replicate s1 $ multiplyElement a b