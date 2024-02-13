module Main where

import Control.Lens ((^.))
import Data.Tree (Tree, drawForest, drawTree)
import ML qualified
import ML.Backprop qualified as ML
import ML.Graph (Op, toOp)
import ML.NN qualified as NN

xs :: (Floating a) => [[a]]
xs =
  [ [0, 1, 0],
    [-1, 0, -1],
    [-0.3, 1, -0.5],
    [0.1, 1, 0.4]
  ]

ys :: (Floating a) => [a]
ys = [1, -1, -1, 1]

gradientDescent :: NN.MLP Double -> NN.MLP Double
gradientDescent mlp = nudged
  where
    -- Forward pass
    ypred = concat [NN.forwardPassMLP x mlp | x <- xs]
    loss = ML.eval $ sum [(yout - ygt) ** 2 | (ygt, yout) <- zip ys ypred]

    -- Backward pass
    zerograded = NN.zeroGradMLP mlp

    nudged = NN.nudge 0.01 zerograded

main :: IO ()
main = do
  mlp <- NN.mkMLP [3, 4, 4, 1] :: IO (NN.MLP Double)
  let expr :: [ML.Value Double]
      expr = NN.forwardPassMLP [-1, -0.9, 0.8] mlp
      xx :: [Tree (Op Double)]
      xx = toOp <$> expr
      treeString :: [Tree String]
      treeString = (fmap . fmap) show xx
      result = drawForest treeString
  putStrLn result
  print ((^. ML.d . ML.value) <$> expr)

-- putStrLn result
-- print (ML.eval expr)
-- where
--   x1 :: ML.Value Double
--   x1 = 2
--   w1 :: ML.Value Double
--   w1 = -3
--   x2 :: ML.Value Double
--   x2 = 0
--   w2 :: ML.Value Double
--   w2 = 1
--   b :: ML.Value Double
--   b = 6.8813735870195432

--   expr :: ML.Value Double
--   expr = ML.backprop $ ML.forwardPass $ tanh (x1 * w1 + x2 * w2 + b)

--   result = drawTree (show <$> toOp expr)
