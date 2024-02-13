module ML.Graph where

import Control.Lens (makeLenses)
import Control.Monad.State.Lazy
import Data.Deriving (deriveEq1, deriveOrd1, deriveShow1)
import Data.Functor ((<&>))
import Data.Functor.Foldable (Recursive (cata))
import Data.Tree (Tree (Node, rootLabel, subForest))
import GHC.Base (Float)
import ML qualified

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
    go (ML.MulF l r (ML.Data grad value)) = Node {rootLabel = Op "*" (ML.Data grad value), subForest = [l, r]}
    go (ML.AbsF x (ML.Data grad value)) = Node {rootLabel = Op "abs" (ML.Data grad value), subForest = [x]}
    go (ML.NegF x (ML.Data grad value)) = Node {rootLabel = Op "neg" (ML.Data grad value), subForest = [x]}
    go (ML.ExpF x (ML.Data grad value)) = Node {rootLabel = Op "exp" (ML.Data grad value), subForest = [x]}
    go (ML.SignumF x (ML.Data grad value)) = Node {rootLabel = Op "signum" (ML.Data grad value), subForest = [x]}
    go (ML.LogF x (ML.Data grad value)) = Node {rootLabel = Op "log" (ML.Data grad value), subForest = [x]}
    go (ML.SinF x (ML.Data grad value)) = Node {rootLabel = Op "sin" (ML.Data grad value), subForest = [x]}
    go (ML.AsinF x (ML.Data grad value)) = Node {rootLabel = Op "asin" (ML.Data grad value), subForest = [x]}
