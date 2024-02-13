module Main where

import Control.Lens ((^.))
import Data.Tensor qualified as T
import Data.Tree (Tree, drawForest, drawTree)
import ML qualified
import ML.Backprop qualified as ML
import ML.Graph (Op, toOp)
import ML.NN qualified as NN

type X = T.Tensor '[3, 3] Float

main :: IO ()
main = do
  -- mlp <- NN.mkMLP [3, 4, 4, 1] :: IO (NN.MLP Float)
  -- let expr :: [ML.Value Float]
  --     expr = NN.forwardPassMLP [-1, -0.9, 0.8] mlp
  --     xx :: [Tree (Op Float)]
  --     xx = toOp <$> expr
  --     treeString :: [Tree String]
  --     treeString = (fmap . fmap) show xx
  --     result = drawForest treeString
  -- -- putStrLn result
  -- print ((^. ML.d . ML.value) <$> expr)

  putStrLn result
  print (ML.eval expr)
  where
    x1 :: ML.Value Double
    x1 = 2
    w1 :: ML.Value Double
    w1 = -3
    x2 :: ML.Value Double
    x2 = 0
    w2 :: ML.Value Double
    w2 = 1
    b :: ML.Value Double
    b = 6.8813735870195432

    expr :: ML.Value Double
    expr = ML.calculateGradients $ ML.forwardPass $ tanh (x1 * w1 + x2 * w2 + b)

    result = drawTree (show <$> toOp expr)
