module Main where

import Data.Tree (drawTree)
import ML
import ML.Graph (forwardPass)

main :: IO ()
main = putStrLn result
  where
    x1 :: Value
    x1 = Value 2 "x1"
    w1 :: Value
    w1 = Value 0 "w1"
    x2 :: Value
    x2 = Value (-3.0) "x2"
    w2 :: Value
    w2 = Value 1 "w2"
    b :: Value
    b = Value 6.7 "b"

    expr :: Value
    expr = x1 * w1 + x2 * w2 + b

    result = drawTree (show <$> forwardPass expr)
