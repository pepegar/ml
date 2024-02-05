{-# LANGUAGE TemplateHaskell #-}

module ML.Graph where

import Control.Comonad (Comonad (extract))
import Control.Comonad.Cofree (Cofree ((:<)), ComonadCofree (unwrap))
import Control.Lens (makeLenses)
import Control.Monad.State.Lazy
import Data.Functor ((<&>))
import Data.Functor.Foldable (Base, Corecursive (embed), Recursive (cata, para), hylo)
import Data.GraphViz.Printing (group)
import Data.Tree (Tree (Node, rootLabel, subForest))
import GHC.Base (Float)
import ML qualified

toTree :: ML.Value -> Tree String
toTree = cata go
  where
    go :: ML.ValueF (Tree String) -> Tree String
    go (ML.ValueF x) = Node {rootLabel = show x, subForest = []}
    go (ML.AddF x y) = Node {rootLabel = "Add", subForest = [x, y]}
    go (ML.SubF x y) = Node {rootLabel = "Sub", subForest = [x, y]}
    go (ML.MulF x y) = Node {rootLabel = "Mul", subForest = [x, y]}
    go (ML.DivF x y) = Node {rootLabel = "Div", subForest = [x, y]}
    go (ML.AbsF x) = Node {rootLabel = "Abs", subForest = [x]}
    go (ML.NegF x) = Node {rootLabel = "Neg", subForest = [x]}
    go (ML.ExpF x) = Node {rootLabel = "Exp", subForest = [x]}
    go (ML.SignumF x) = Node {rootLabel = "Signum", subForest = [x]}
    go (ML.ReluF x) = Node {rootLabel = "Relu", subForest = [x]}
    go (ML.LogF x) = Node {rootLabel = "Log", subForest = [x]}
    go (ML.SinF x) = Node {rootLabel = "Sin", subForest = [x]}
    go (ML.CosF x) = Node {rootLabel = "Cos", subForest = [x]}
    go (ML.AsinF x) = Node {rootLabel = "Asin", subForest = [x]}
    go (ML.AcosF x) = Node {rootLabel = "Acos", subForest = [x]}
    go (ML.AtanF x) = Node {rootLabel = "Atan", subForest = [x]}
    go (ML.SinhF x) = Node {rootLabel = "Sinh", subForest = [x]}
    go (ML.CoshF x) = Node {rootLabel = "Cosh", subForest = [x]}
    go (ML.AsinhF x) = Node {rootLabel = "Asinh", subForest = [x]}
    go (ML.AcoshF x) = Node {rootLabel = "Acosh", subForest = [x]}
    go (ML.AtanhF x) = Node {rootLabel = "Atanh", subForest = [x]}

data Op = Op
  { _op :: String,
    _value :: Float,
    _grad :: Float
  }
  deriving (Show, Eq)

makeLenses ''Op

forwardPass :: ML.Value -> Tree Op
forwardPass = para go
  where
    go :: ML.ValueF (ML.Value, Tree Op) -> Tree Op
    go (ML.ValueF x) = Node {rootLabel = Op "Value" x 0, subForest = []}
    go (ML.AddF (valL, l) (valR, r)) = Node {rootLabel = Op "Add" (ML.eval valL + ML.eval valR) 0, subForest = [l, r]}
    go (ML.SubF (valL, l) (valR, r)) = Node {rootLabel = Op "Sub" (ML.eval valL - ML.eval valR) 0, subForest = [l, r]}
    go (ML.MulF (valL, l) (valR, r)) = Node {rootLabel = Op "Mul" (ML.eval valL * ML.eval valR) 0, subForest = [l, r]}
    go (ML.DivF (valL, l) (valR, r)) = Node {rootLabel = Op "Div" (ML.eval valL / ML.eval valR) 0, subForest = [l, r]}
    go (ML.AbsF (val, x)) = Node {rootLabel = Op "Abs" (abs $ ML.eval val) 0, subForest = [x]}
    go (ML.NegF (val, x)) = Node {rootLabel = Op "Neg" (negate $ ML.eval val) 0, subForest = [x]}
    go (ML.ExpF (val, x)) = Node {rootLabel = Op "Exp" (exp $ ML.eval val) 0, subForest = [x]}
    go (ML.SignumF (val, x)) = Node {rootLabel = Op "Signum" (signum $ ML.eval val) 0, subForest = [x]}
    go (ML.ReluF (val, x)) = Node {rootLabel = Op "Relu" (ML.eval val) 0, subForest = [x]}
    go (ML.LogF (val, x)) = Node {rootLabel = Op "Log" (log $ ML.eval val) 0, subForest = [x]}
    go (ML.SinF (val, x)) = Node {rootLabel = Op "Sin" (sin $ ML.eval val) 0, subForest = [x]}
    go (ML.CosF (val, x)) = Node {rootLabel = Op "Cos" (cos $ ML.eval val) 0, subForest = [x]}
    go (ML.AsinF (val, x)) = Node {rootLabel = Op "Asin" (asin $ ML.eval val) 0, subForest = [x]}
    go (ML.AcosF (val, x)) = Node {rootLabel = Op "Acos" (acos $ ML.eval val) 0, subForest = [x]}
    go (ML.AtanF (val, x)) = Node {rootLabel = Op "Atan" (atan $ ML.eval val) 0, subForest = [x]}
    go (ML.SinhF (val, x)) = Node {rootLabel = Op "Sinh" (sinh $ ML.eval val) 0, subForest = [x]}
    go (ML.CoshF (val, x)) = Node {rootLabel = Op "Cosh" (cosh $ ML.eval val) 0, subForest = [x]}
    go (ML.AsinhF (val, x)) = Node {rootLabel = Op "Asinh" (asinh $ ML.eval val) 0, subForest = [x]}
    go (ML.AcoshF (val, x)) = Node {rootLabel = Op "Acosh" (acosh $ ML.eval val) 0, subForest = [x]}
    go (ML.AtanhF (val, x)) = Node {rootLabel = Op "Atanh" (atanh $ ML.eval val) 0, subForest = [x]}
