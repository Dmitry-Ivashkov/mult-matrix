module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

type Data = Float

data QuadTree = Quad QuadTree QuadTree QuadTree QuadTree Int | Unique Data Int deriving(Show)

size :: QuadTree -> Int
size (Quad _ _ _ _ int) = int
size (Unique _ int) = int
