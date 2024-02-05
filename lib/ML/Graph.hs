module ML.Graph where

import Control.Comonad (Comonad (extract))
import Control.Comonad.Cofree (Cofree ((:<)), ComonadCofree (unwrap))
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

forwardPass :: ML.Value -> Tree String
forwardPass = para go
  where
    go :: ML.ValueF (ML.Value, Tree String) -> Tree String
    go (ML.ValueF x) = Node {rootLabel = show x, subForest = []}
    go (ML.AddF (valL, l) (valR, r)) = Node {rootLabel = "Add " ++ show (ML.eval valL + ML.eval valR), subForest = [l, r]}
    go (ML.SubF (valL, l) (valR, r)) = Node {rootLabel = "Sub " ++ show (ML.eval valL - ML.eval valR), subForest = [l, r]}
    go (ML.MulF (valL, l) (valR, r)) = Node {rootLabel = "Mul " ++ show (ML.eval valL * ML.eval valR), subForest = [l, r]}
    go (ML.DivF (valL, l) (valR, r)) = Node {rootLabel = "Div " ++ show (ML.eval valL / ML.eval valR), subForest = [l, r]}
    go (ML.AbsF (val, x)) = Node {rootLabel = "Abs " ++ show (abs $ ML.eval val), subForest = [x]}
    go (ML.NegF (val, x)) = Node {rootLabel = "Neg " ++ show (negate $ ML.eval val), subForest = [x]}
    go (ML.ExpF (val, x)) = Node {rootLabel = "Exp " ++ show (exp $ ML.eval val), subForest = [x]}
    go (ML.SignumF (val, x)) = Node {rootLabel = "Signum " ++ show (signum $ ML.eval val), subForest = [x]}
    go (ML.ReluF (val, x)) = Node {rootLabel = "Relu " ++ show (ML.eval val), subForest = [x]}
    go (ML.LogF (val, x)) = Node {rootLabel = "Log " ++ show (log $ ML.eval val), subForest = [x]}
    go (ML.SinF (val, x)) = Node {rootLabel = "Sin " ++ show (sin $ ML.eval val), subForest = [x]}
    go (ML.CosF (val, x)) = Node {rootLabel = "Cos " ++ show (cos $ ML.eval val), subForest = [x]}
    go (ML.AsinF (val, x)) = Node {rootLabel = "Asin " ++ show (asin $ ML.eval val), subForest = [x]}
    go (ML.AcosF (val, x)) = Node {rootLabel = "Acos " ++ show (acos $ ML.eval val), subForest = [x]}
    go (ML.AtanF (val, x)) = Node {rootLabel = "Atan " ++ show (atan $ ML.eval val), subForest = [x]}
    go (ML.SinhF (val, x)) = Node {rootLabel = "Sinh " ++ show (sinh $ ML.eval val), subForest = [x]}
    go (ML.CoshF (val, x)) = Node {rootLabel = "Cosh " ++ show (cosh $ ML.eval val), subForest = [x]}
    go (ML.AsinhF (val, x)) = Node {rootLabel = "Asinh " ++ show (asinh $ ML.eval val), subForest = [x]}
    go (ML.AcoshF (val, x)) = Node {rootLabel = "Acosh " ++ show (acosh $ ML.eval val), subForest = [x]}
    go (ML.AtanhF (val, x)) = Node {rootLabel = "Atanh " ++ show (atanh $ ML.eval val), subForest = [x]}
