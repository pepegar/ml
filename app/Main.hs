module Main where

import Data.Tensor qualified as T
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
    x1 :: ML.Value (T.Tensor '[3, 3] Float)
    x1 = "x1" @= 2
    w1 :: ML.Value (T.Tensor '[3, 3] Float)
    w1 = "w1" @= (-3)
    x2 :: ML.Value (T.Tensor '[3, 3] Float)
    x2 = "x2" @= 0
    w2 :: ML.Value (T.Tensor '[3, 3] Float)
    w2 = "w2" @= 1
    b :: ML.Value (T.Tensor '[3, 3] Float)
    b = "b" @= 6.8813735870195432

    expr :: ML.Value (T.Tensor '[3, 3] Float)
    expr = ML.calculateGradients $ ML.forwardPass $ tanh (x1 * w1 + x2 * w2 + b)

    result = drawTree (show <$> toOp expr)
