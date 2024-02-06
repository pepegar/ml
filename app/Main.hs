module Main where

import Data.Tree (drawTree)
import ML
import ML.Graph (forwardPass)

main :: IO ()
main = putStrLn result
  where
    x1 :: Value
    x1 = Value 2 "x1" 0
    w1 :: Value
    w1 = Value (-3) "w1" 0
    x2 :: Value
    x2 = Value 0 "x2" 0
    w2 :: Value
    w2 = Value 0 "w2" 0
    b :: Value
    b = Value 6.88137 "b" 0

    expr :: Value
    expr = tanh (x1 * w1 + x2 * w2 + b)

    result = drawTree (show <$> forwardPass expr)
