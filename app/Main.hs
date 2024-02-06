module Main where

import Data.Tree (drawTree)
import ML qualified
import ML.Backprop (calculateGradients)
import ML.Graph (toOp, toTree)

main :: IO ()
main = do
  putStrLn result
  print (ML.eval expr)
  where
    x1 :: ML.Value
    x1 = ML.Value 2 "x1" 0
    w1 :: ML.Value
    w1 = ML.Value (-3) "w1" 0
    x2 :: ML.Value
    x2 = ML.Value 0 "x2" 0
    w2 :: ML.Value
    w2 = ML.Value 0 "w2" 0
    b :: ML.Value
    b = ML.Value 6.88137 "b" 0

    expr :: ML.Value
    expr = calculateGradients $ tanh (x1 * w1 + x2 * w2 + b)

    result = drawTree (show <$> toOp expr)
