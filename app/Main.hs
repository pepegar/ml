module Main where

import Control.Lens ((^.))
import Data.Tensor qualified as T
import Data.Tree (Tree, drawForest)
import ML qualified
import ML.Graph (Op, toOp)
import ML.NN qualified as NN

type X = T.Tensor '[3, 3] Float

main :: IO ()
main = do
  mlp <- NN.mkMLP [3, 4, 4, 1] :: IO (NN.MLP Float)
  let expr :: [ML.Value Float]
      expr = NN.forwardPassMLP [-1, -0.9, 0.8] mlp
      xx :: [Tree (Op Float)]
      xx = toOp <$> expr
      treeString :: [Tree String]
      treeString = (fmap . fmap) show xx
      result = drawForest treeString
  -- putStrLn result
  print ((^. ML.d . ML.value) <$> expr)

-- putStrLn result
-- print (ML.eval expr)
-- where
--   x1 :: ML.Value (T.Tensor '[1] Float)
--   x1 = "x1" @= 2
--   w1 :: ML.Value (T.Tensor '[1] Float)
--   w1 = "w1" @= (-3)
--   x2 :: ML.Value (T.Tensor '[1] Float)
--   x2 = "x2" @= 0
--   w2 :: ML.Value (T.Tensor '[1] Float)
--   w2 = "w2" @= 1
--   b :: ML.Value (T.Tensor '[1] Float)
--   b = "b" @= 6.8813735870195432

--   expr :: ML.Value (T.Tensor '[1] Float)
--   expr = ML.calculateGradients $ ML.forwardPass $ tanh (x1 * w1 + x2 * w2 + b)

--   result = drawTree (show <$> toOp expr)
