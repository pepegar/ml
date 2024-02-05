module ML.Graph where

import Control.Comonad (Comonad (extract))
import Control.Comonad.Cofree (Cofree ((:<)), ComonadCofree (unwrap))
import Control.Monad.State.Lazy
import Data.Functor ((<&>))
import Data.Functor.Foldable (Base, Recursive (cata), hylo)
import Data.Tree (Tree (Node, rootLabel, subForest))
import ML
import ML qualified

toTree :: Value -> Tree String
toTree = cata go
  where
    go :: ValueF (Tree String) -> Tree String
    go (ValueF x) = Node {rootLabel = show x, subForest = []}
    go (AddF x y) = Node {rootLabel = "Add", subForest = [x, y]}
    go (SubF x y) = Node {rootLabel = "Sub", subForest = [x, y]}
    go (MulF x y) = Node {rootLabel = "Mul", subForest = [x, y]}
    go (DivF x y) = Node {rootLabel = "Div", subForest = [x, y]}
    go (AbsF x) = Node {rootLabel = "Abs", subForest = [x]}
    go (NegF x) = Node {rootLabel = "Neg", subForest = [x]}
    go (ExpF x) = Node {rootLabel = "Exp", subForest = [x]}
    go (SignumF x) = Node {rootLabel = "Signum", subForest = [x]}
    go (ReluF x) = Node {rootLabel = "Relu", subForest = [x]}
    go (LogF x) = Node {rootLabel = "Log", subForest = [x]}
    go (SinF x) = Node {rootLabel = "Sin", subForest = [x]}
    go (CosF x) = Node {rootLabel = "Cos", subForest = [x]}
    go (AsinF x) = Node {rootLabel = "Asin", subForest = [x]}
    go (AcosF x) = Node {rootLabel = "Acos", subForest = [x]}
    go (AtanF x) = Node {rootLabel = "Atan", subForest = [x]}
    go (SinhF x) = Node {rootLabel = "Sinh", subForest = [x]}
    go (CoshF x) = Node {rootLabel = "Cosh", subForest = [x]}
    go (AsinhF x) = Node {rootLabel = "Asinh", subForest = [x]}
    go (AcoshF x) = Node {rootLabel = "Acosh", subForest = [x]}
    go (AtanhF x) = Node {rootLabel = "Atanh", subForest = [x]}
