module Main where

import Data.Tree (drawTree)
import ML ((@=))
import ML qualified
import ML.Backprop qualified as ML
import ML.Graph (toOp)

main :: IO ()
main = do
  putStrLn result
  print (ML.eval expr)
  where
    x1 :: ML.Value Float
    x1 = "x1" @= 2
    w1 :: ML.Value Float
    w1 = "w1" @= (-3)
    x2 :: ML.Value Float
    x2 = "x2" @= 0
    w2 :: ML.Value Float
    w2 = "w2" @= 1
    b :: ML.Value Float
    b = "b" @= 6.8813735870195432

    expr :: ML.Value Float
    expr = ML.calculateGradients $ ML.forwardPass $ tanh (x1 * w1 + x2 * w2 + b)

    result = drawTree (show <$> toOp expr)
