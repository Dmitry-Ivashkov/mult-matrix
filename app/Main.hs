module Main where

import Lib

main :: IO ()
main = do
  print $ (multiply (*) (+) 2 3 m1 m1) == (mCreateSimpleMatrix 3 2 2) -- dim = 3, size = 2, matrix = [[[2,2],[2,2]],[[2,2],[2,2]]]
    where
      m1 = mConcatMatrix $ replicate 8 one -- dim = 3, size = 2, matrix = [[[1,1],[1,1]],[[1,1],[1,1]]]
      one = mOnes 3 1 -- dim = 3, size = 1, matrix = [[[1]]]
