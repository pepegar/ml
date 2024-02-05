module ML.Graph where

import Control.Comonad (Comonad (extract))
import Control.Comonad.Cofree (Cofree ((:<)), ComonadCofree (unwrap))
import Control.Monad.State.Lazy
import Data.Functor ((<&>))
import Data.Functor.Foldable (Recursive (cata), hylo)
import ML qualified

fresh :: State Int Int
fresh = state $ \i -> (i, i + 1)

repeatString :: String -> Int -> String
repeatString string n = concat $ replicate n string

toString :: ML.Value -> String
toString = printCofree . annotateWithDepth
  where
    printCofree :: Cofree ML.ValueF Int -> String
    printCofree x =
      repeatString "-" (extract x)
        ++ " "
        ++ name x
        ++ "\n"

name :: Cofree ML.ValueF a -> String
name (_ :< ML.ValueF x) = "Value" ++ show x
name (_ :< ML.AddF _ _) = "Add"
name (_ :< ML.SubF _ _) = "Sub"
name (_ :< ML.MulF _ _) = "Mul"
name (_ :< ML.DivF _ _) = "Div"
name (_ :< ML.AbsF _) = "Abs"
name (_ :< ML.NegF _) = "Neg"
name (_ :< ML.ExpF _) = "Exp"
name (_ :< ML.SignumF _) = "Signum"
name (_ :< ML.ReluF _) = "Relu"
name (_ :< ML.LogF _) = "Log"
name (_ :< ML.SinF _) = "Sin"
name (_ :< ML.CosF _) = "Cos"
name (_ :< ML.AsinF _) = "Asin"
name (_ :< ML.AcosF _) = "Acos"
name (_ :< ML.AtanF _) = "Atan"
name (_ :< ML.SinhF _) = "Sinh"
name (_ :< ML.CoshF _) = "Cosh"
name (_ :< ML.AsinhF _) = "Asinh"
name (_ :< ML.AcoshF _) = "Acosh"
name (_ :< ML.AtanhF _) = "Atanh"

annotateWithDepth :: ML.Value -> Cofree ML.ValueF Int
annotateWithDepth = cata depthAlg

depthAlg :: ML.ValueF (Cofree ML.ValueF Int) -> Cofree ML.ValueF Int
depthAlg l@(ML.ValueF value) = 0 :< l
depthAlg x@(ML.AddF (l :< _) (r :< _)) = max l r + 1 :< x
depthAlg x@(ML.SubF (l :< _) (r :< _)) = max l r + 1 :< x
depthAlg x@(ML.MulF (l :< _) (r :< _)) = max l r + 1 :< x
depthAlg x@(ML.DivF (l :< _) (r :< _)) = max l r + 1 :< x
depthAlg x@(ML.AbsF (val :< _)) = val + 1 :< x
depthAlg x@(ML.NegF (val :< _)) = val + 1 :< x
depthAlg x@(ML.ExpF (val :< _)) = val + 1 :< x
depthAlg x@(ML.SignumF (val :< _)) = val + 1 :< x
depthAlg x@(ML.ReluF (val :< _)) = val + 1 :< x
depthAlg x@(ML.LogF (val :< _)) = val + 1 :< x
depthAlg x@(ML.SinF (val :< _)) = val + 1 :< x
depthAlg x@(ML.CosF (val :< _)) = val + 1 :< x
depthAlg x@(ML.AsinF (val :< _)) = val + 1 :< x
depthAlg x@(ML.AcosF (val :< _)) = val + 1 :< x
depthAlg x@(ML.AtanF (val :< _)) = val + 1 :< x
depthAlg x@(ML.SinhF (val :< _)) = val + 1 :< x
depthAlg x@(ML.CoshF (val :< _)) = val + 1 :< x
depthAlg x@(ML.AsinhF (val :< _)) = val + 1 :< x
depthAlg x@(ML.AcoshF (val :< _)) = val + 1 :< x
depthAlg x@(ML.AtanhF (val :< _)) = val + 1 :< x
