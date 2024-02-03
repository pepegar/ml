module ML.Graph where

import Control.Comonad.Cofree (Cofree ((:<)))
import Control.Monad.State.Lazy
import Data.Functor ((<&>))
import Data.Functor.Foldable (Recursive (cata))
import Data.Graph.Inductive qualified as G
import ML qualified

fresh :: State Int Int
fresh = state $ \i -> (i, i + 1)

toGraph :: ML.Value -> G.Gr String ()
toGraph = flip evalState 0 . cata go

annotateWithId :: ML.Value -> Cofree ML.ValueF Int
annotateWithId = cata go
  where
    go :: ML.ValueF (Cofree ML.ValueF Int) -> Cofree ML.ValueF Int
    go l@(ML.ValueF value) = 0 :< l
    go x@(ML.AddF (l :< _) (r :< _)) = max l r + 1 :< x
    go x@(ML.SubF (l :< _) (r :< _)) = max l r + 1 :< x
    go x@(ML.MulF (l :< _) (r :< _)) = max l r + 1 :< x
    go x@(ML.DivF (l :< _) (r :< _)) = max l r + 1 :< x
    go x@(ML.AbsF (val :< _)) = val + 1 :< x
    go x@(ML.NegF (val :< _)) = val + 1 :< x
    go x@(ML.ExpF (val :< _)) = val + 1 :< x
    go x@(ML.SignumF (val :< _)) = val + 1 :< x
    go x@(ML.ReluF (val :< _)) = val + 1 :< x
    go x@(ML.LogF (val :< _)) = val + 1 :< x
    go x@(ML.SinF (val :< _)) = val + 1 :< x
    go x@(ML.CosF (val :< _)) = val + 1 :< x
    go x@(ML.AsinF (val :< _)) = val + 1 :< x
    go x@(ML.AcosF (val :< _)) = val + 1 :< x
    go x@(ML.AtanF (val :< _)) = val + 1 :< x
    go x@(ML.SinhF (val :< _)) = val + 1 :< x
    go x@(ML.CoshF (val :< _)) = val + 1 :< x
    go x@(ML.AsinhF (val :< _)) = val + 1 :< x
    go x@(ML.AcoshF (val :< _)) = val + 1 :< x
    go x@(ML.AtanhF (val :< _)) = val + 1 :< x

createNode pos tag = ([], pos, tag, [])

go :: ML.ValueF (State Int (G.Gr String ())) -> State Int (G.Gr String ())
go (ML.ValueF value) = fresh <&> \x -> createNode x ("Value " ++ show value) G.& G.empty
go (ML.AddF l r) = fresh <&> \x -> createNode x "" G.& G.empty
go (ML.SubF l r) = fresh <&> \x -> createNode x "" G.& G.empty
go (ML.MulF l r) = fresh <&> \x -> createNode x "" G.& G.empty
go (ML.DivF l r) = fresh <&> \x -> createNode x "" G.& G.empty
go (ML.AbsF val) = fresh <&> \x -> createNode x "" G.& G.empty
go (ML.NegF val) = fresh <&> \x -> createNode x "" G.& G.empty
go (ML.ExpF val) = fresh <&> \x -> createNode x "" G.& G.empty
go (ML.SignumF val) = fresh <&> \x -> createNode x "" G.& G.empty
go (ML.ReluF val) = fresh <&> \x -> createNode x "" G.& G.empty
go (ML.LogF val) = fresh <&> \x -> createNode x "" G.& G.empty
go (ML.SinF val) = fresh <&> \x -> createNode x "" G.& G.empty
go (ML.CosF val) = fresh <&> \x -> createNode x "" G.& G.empty
go (ML.AsinF val) = fresh <&> \x -> createNode x "" G.& G.empty
go (ML.AcosF val) = fresh <&> \x -> createNode x "" G.& G.empty
go (ML.AtanF val) = fresh <&> \x -> createNode x "" G.& G.empty
go (ML.SinhF val) = fresh <&> \x -> createNode x "" G.& G.empty
go (ML.CoshF val) = fresh <&> \x -> createNode x "" G.& G.empty
go (ML.AsinhF val) = fresh <&> \x -> createNode x "" G.& G.empty
go (ML.AcoshF val) = fresh <&> \x -> createNode x "" G.& G.empty
go (ML.AtanhF val) = fresh <&> \x -> createNode x "" G.& G.empty
