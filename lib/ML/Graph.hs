{-# LANGUAGE TemplateHaskell #-}

module ML.Graph where

import Control.Comonad (Comonad (extract))
import Control.Comonad.Cofree (Cofree ((:<)), ComonadCofree (unwrap))
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
    go (ML.ValueF x name _) = Node {rootLabel = name ++ " " ++ show x, subForest = []}
    go (ML.AddF x y _ _) = Node {rootLabel = "Add", subForest = [x, y]}
    go (ML.SubF x y _ _) = Node {rootLabel = "Sub", subForest = [x, y]}
    go (ML.MulF x y _ _) = Node {rootLabel = "Mul", subForest = [x, y]}
    go (ML.DivF x y _ _) = Node {rootLabel = "Div", subForest = [x, y]}
    go (ML.AbsF x _ _) = Node {rootLabel = "Abs", subForest = [x]}
    go (ML.NegF x _ _) = Node {rootLabel = "Neg", subForest = [x]}
    go (ML.ExpF x _ _) = Node {rootLabel = "Exp", subForest = [x]}
    go (ML.SignumF x _ _) = Node {rootLabel = "Signum", subForest = [x]}
    go (ML.LogF x _ _) = Node {rootLabel = "Log", subForest = [x]}
    go (ML.SinF x _ _) = Node {rootLabel = "Sin", subForest = [x]}
    go (ML.CosF x _ _) = Node {rootLabel = "Cos", subForest = [x]}
    go (ML.AsinF x _ _) = Node {rootLabel = "Asin", subForest = [x]}
    go (ML.AcosF x _ _) = Node {rootLabel = "Acos", subForest = [x]}
    go (ML.AtanF x _ _) = Node {rootLabel = "Atan", subForest = [x]}
    go (ML.SinhF x _ _) = Node {rootLabel = "Sinh", subForest = [x]}
    go (ML.CoshF x _ _) = Node {rootLabel = "Cosh", subForest = [x]}
    go (ML.AsinhF x _ _) = Node {rootLabel = "Asinh", subForest = [x]}
    go (ML.AcoshF x _ _) = Node {rootLabel = "Acosh", subForest = [x]}
    go (ML.AtanhF x _ _) = Node {rootLabel = "Atanh", subForest = [x]}

data Op = Op
  { _op :: String,
    _value :: Float,
    _grad :: Float
  }
  deriving (Show, Eq)

makeLenses ''Op

toOp :: ML.Value -> Tree Op
toOp = cata go
  where
    go :: ML.ValueF (Tree Op) -> Tree Op
    go (ML.ValueF x name grad) = Node {rootLabel = Op name x grad, subForest = []}
    go (ML.AddF l r grad value) = Node {rootLabel = Op "+" value grad, subForest = [l, r]}
    go (ML.SubF l r grad value) = Node {rootLabel = Op "-" value grad, subForest = [l, r]}
    go (ML.MulF l r grad value) = Node {rootLabel = Op "*" value grad, subForest = [l, r]}
    go (ML.DivF l r grad value) = Node {rootLabel = Op "/" value grad, subForest = [l, r]}
    go (ML.AbsF x grad value) = Node {rootLabel = Op "abs" value grad, subForest = [x]}
    go (ML.NegF x grad value) = Node {rootLabel = Op "neg" value grad, subForest = [x]}
    go (ML.ExpF x grad value) = Node {rootLabel = Op "exp" value grad, subForest = [x]}
    go (ML.SignumF x grad value) = Node {rootLabel = Op "signum" value grad, subForest = [x]}
    go (ML.LogF x grad value) = Node {rootLabel = Op "log" value grad, subForest = [x]}
    go (ML.SinF x grad value) = Node {rootLabel = Op "sin" value grad, subForest = [x]}
    go (ML.CosF x grad value) = Node {rootLabel = Op "cos" value grad, subForest = [x]}
    go (ML.AsinF x grad value) = Node {rootLabel = Op "asin" value grad, subForest = [x]}
    go (ML.AcosF x grad value) = Node {rootLabel = Op "acos" value grad, subForest = [x]}
    go (ML.AtanF x grad value) = Node {rootLabel = Op "atan" value grad, subForest = [x]}
    go (ML.SinhF x grad value) = Node {rootLabel = Op "sinh" value grad, subForest = [x]}
    go (ML.CoshF x grad value) = Node {rootLabel = Op "cosh" value grad, subForest = [x]}
    go (ML.AsinhF x grad value) = Node {rootLabel = Op "asinh" value grad, subForest = [x]}
    go (ML.AcoshF x grad value) = Node {rootLabel = Op "acosh" value grad, subForest = [x]}
    go (ML.AtanhF x grad value) = Node {rootLabel = Op "atanh" value grad, subForest = [x]}
