module ML.Graph where

import Control.Lens (makeLenses)
import Control.Monad.State.Lazy
import Data.Deriving (deriveEq1, deriveOrd1, deriveShow1)
import Data.Functor ((<&>))
import Data.Functor.Foldable (Recursive (cata))
import Data.Tree (Tree (Node, rootLabel, subForest))
import GHC.Base (Float)
import ML qualified

toTree :: (Show a) => ML.Value a -> Tree String
toTree = cata go
  where
    go :: (Show a) => ML.ValueF a (Tree String) -> Tree String
    go (ML.ValueF (ML.Data _ x)) = Node {rootLabel = show x, subForest = []}
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
    go (ML.AsinF x _) = Node {rootLabel = "Asin", subForest = [x]}
    go (ML.AcosF x _) = Node {rootLabel = "Acos", subForest = [x]}
    go (ML.AtanF x _) = Node {rootLabel = "Atan", subForest = [x]}

data Op a = Op
  { _op :: String,
    _v :: ML.Data a
  }

makeLenses ''Op

deriving instance (Eq a) => Eq (Op a)

deriving instance (Ord a) => Ord (Op a)

deriving instance (Show a) => Show (Op a)

toOp :: (Show a) => ML.Value a -> Tree (Op a)
toOp = cata go
  where
    go (ML.ValueF (ML.Data grad value)) = Node {rootLabel = Op (show value) (ML.Data grad value), subForest = []}
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
    go (ML.AsinF x (ML.Data grad value)) = Node {rootLabel = Op "asin" (ML.Data grad value), subForest = [x]}
    go (ML.AcosF x (ML.Data grad value)) = Node {rootLabel = Op "acos" (ML.Data grad value), subForest = [x]}
    go (ML.AtanF x (ML.Data grad value)) = Node {rootLabel = Op "atan" (ML.Data grad value), subForest = [x]}
