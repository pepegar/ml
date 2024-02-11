{-# LANGUAGE TemplateHaskell #-}

module ML.Graph where

import Control.Lens (makeLenses)
import Control.Monad.State.Lazy
import Data.Functor ((<&>))
import Data.Functor.Foldable (Recursive (cata))
import Data.Tree (Tree (Node, rootLabel, subForest))
import GHC.Base (Float)
import ML qualified

toTree :: ML.Value -> Tree String
toTree = cata go
  where
    go :: ML.ValueF (Tree String) -> Tree String
    go (ML.ValueF name (ML.Data _ x)) = Node {rootLabel = name ++ " " ++ show x, subForest = []}
    go (ML.AddF x y _) = Node {rootLabel = "Add", subForest = [x, y]}
    go (ML.SubF x y _) = Node {rootLabel = "Sub", subForest = [x, y]}
    go (ML.MulF x y _) = Node {rootLabel = "Mul", subForest = [x, y]}
    go (ML.DivF x y _) = Node {rootLabel = "Div", subForest = [x, y]}
    go (ML.AbsF x _) = Node {rootLabel = "Abs", subForest = [x]}
    go (ML.NegF x _) = Node {rootLabel = "Neg", subForest = [x]}
    go (ML.ExpF x _) = Node {rootLabel = "Exp", subForest = [x]}
    go (ML.SignumF x _) = Node {rootLabel = "Signum", subForest = [x]}
    go (ML.LogF x _) = Node {rootLabel = "Log", subForest = [x]}
    go (ML.SinF x _) = Node {rootLabel = "Sin", subForest = [x]}
    go (ML.CosF x _) = Node {rootLabel = "Cos", subForest = [x]}
    go (ML.AsinF x _) = Node {rootLabel = "Asin", subForest = [x]}
    go (ML.AcosF x _) = Node {rootLabel = "Acos", subForest = [x]}
    go (ML.AtanF x _) = Node {rootLabel = "Atan", subForest = [x]}
    go (ML.SinhF x _) = Node {rootLabel = "Sinh", subForest = [x]}
    go (ML.CoshF x _) = Node {rootLabel = "Cosh", subForest = [x]}
    go (ML.AsinhF x _) = Node {rootLabel = "Asinh", subForest = [x]}
    go (ML.AcoshF x _) = Node {rootLabel = "Acosh", subForest = [x]}
    go (ML.AtanhF x _) = Node {rootLabel = "Atanh", subForest = [x]}

data Op = Op
  { _op :: String,
    _v :: ML.Data
  }
  deriving (Show, Eq)

makeLenses ''Op

toOp :: ML.Value -> Tree Op
toOp = cata go
  where
    go :: ML.ValueF (Tree Op) -> Tree Op
    go (ML.ValueF name (ML.Data grad value)) = Node {rootLabel = Op name (ML.Data grad value), subForest = []}
    go (ML.AddF l r (ML.Data grad value)) = Node {rootLabel = Op "+" (ML.Data grad value), subForest = [l, r]}
    go (ML.SubF l r (ML.Data grad value)) = Node {rootLabel = Op "-" (ML.Data grad value), subForest = [l, r]}
    go (ML.MulF l r (ML.Data grad value)) = Node {rootLabel = Op "*" (ML.Data grad value), subForest = [l, r]}
    go (ML.DivF l r (ML.Data grad value)) = Node {rootLabel = Op "/" (ML.Data grad value), subForest = [l, r]}
    go (ML.AbsF x (ML.Data grad value)) = Node {rootLabel = Op "abs" (ML.Data grad value), subForest = [x]}
    go (ML.NegF x (ML.Data grad value)) = Node {rootLabel = Op "neg" (ML.Data grad value), subForest = [x]}
    go (ML.ExpF x (ML.Data grad value)) = Node {rootLabel = Op "exp" (ML.Data grad value), subForest = [x]}
    go (ML.SignumF x (ML.Data grad value)) = Node {rootLabel = Op "signum" (ML.Data grad value), subForest = [x]}
    go (ML.LogF x (ML.Data grad value)) = Node {rootLabel = Op "log" (ML.Data grad value), subForest = [x]}
    go (ML.SinF x (ML.Data grad value)) = Node {rootLabel = Op "sin" (ML.Data grad value), subForest = [x]}
    go (ML.CosF x (ML.Data grad value)) = Node {rootLabel = Op "cos" (ML.Data grad value), subForest = [x]}
    go (ML.AsinF x (ML.Data grad value)) = Node {rootLabel = Op "asin" (ML.Data grad value), subForest = [x]}
    go (ML.AcosF x (ML.Data grad value)) = Node {rootLabel = Op "acos" (ML.Data grad value), subForest = [x]}
    go (ML.AtanF x (ML.Data grad value)) = Node {rootLabel = Op "atan" (ML.Data grad value), subForest = [x]}
    go (ML.SinhF x (ML.Data grad value)) = Node {rootLabel = Op "sinh" (ML.Data grad value), subForest = [x]}
    go (ML.CoshF x (ML.Data grad value)) = Node {rootLabel = Op "cosh" (ML.Data grad value), subForest = [x]}
    go (ML.AsinhF x (ML.Data grad value)) = Node {rootLabel = Op "asinh" (ML.Data grad value), subForest = [x]}
    go (ML.AcoshF x (ML.Data grad value)) = Node {rootLabel = Op "acosh" (ML.Data grad value), subForest = [x]}
    go (ML.AtanhF x (ML.Data grad value)) = Node {rootLabel = Op "atanh" (ML.Data grad value), subForest = [x]}
