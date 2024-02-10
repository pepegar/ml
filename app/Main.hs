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
    x1 = ML.Value "x1" 0 2
    w1 :: ML.Value
    w1 = ML.Value "w1" 0 (-3)
    x2 :: ML.Value
    x2 = ML.Value "x2" 0 0
    w2 :: ML.Value
    w2 = ML.Value "w2" 0 1
    b :: ML.Value
    b = ML.Value "b" 0 6.8813735870195432

    expr :: ML.Value
    -- expr = calculateGradients $ sinh (x1 + w1)
    expr = calculateGradients $ tanh (x1 * w1 + x2 * w2 + b)

    result = drawTree (show <$> toOp expr)
