module Main where

import Lib

main :: IO ()
main = do
  print m
  print $ m *** m
    where
      m = concatMatrix one one zero one
      one = ones 1
      zero = zeros 1
