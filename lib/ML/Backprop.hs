module ML.Backprop where

import Control.Lens
import Data.Functor.Foldable (Recursive (cata))
import ML qualified

calculateGradients :: ML.Value -> ML.Value
calculateGradients = flip (cata go) 1
  where
    go :: ML.ValueF (Float -> ML.Value) -> Float -> ML.Value
    go (ML.ValueF name _ value) = \n -> ML.Value name n value
    go (ML.AddF l r grad value) = \n -> ML.Add (l n) (r n) (grad + n) value
    go (ML.SubF l r grad value) = \n -> ML.Sub (l n) (r (-n)) (grad + n) value
    go (ML.MulF l r grad value) = \n -> ML.Mul (l (r n ^. ML.value * n)) (r (l n ^. ML.value * n)) (grad + n) value
    go (ML.DivF l r grad value) = \n -> ML.Div (l (1 / r n ^. ML.value)) (r (-(l n ^. ML.value) / (r n ^. ML.value) ** 2)) (grad + n) value
    go (ML.AbsF input grad value) = \n -> ML.Abs (input n) (grad + n) value
    go (ML.NegF input grad value) = \n -> ML.Neg (input n) (grad + n) value
    go (ML.ExpF input grad value) = \n -> ML.Exp (input n) (grad + n) value
    go (ML.SignumF input grad value) = \n -> ML.Signum (input n) (grad + n) value
    go (ML.LogF input grad value) = \n -> ML.Log (input n) (grad + n) value
    go (ML.SinF input grad value) = \n -> ML.Sin (input n) (grad + n) value
    go (ML.CosF input grad value) = \n -> ML.Cos (input n) (grad + n) value
    go (ML.AsinF input grad value) = \n -> ML.Asin (input n) (grad + n) value
    go (ML.AcosF input grad value) = \n -> ML.Acos (input n) (grad + n) value
    go (ML.AtanF input grad value) = \n -> ML.Atan (input n) (grad + n) value
    go (ML.SinhF input grad value) = \n -> ML.Sinh (input (n * cosh (input n ^. ML.value))) (grad + n) value
    go (ML.CoshF input grad value) = \n -> ML.Cosh (input (n * sinh (input n ^. ML.value))) (grad + n) value
    go (ML.AsinhF input grad value) = \n -> ML.Asinh (input n) (grad + n) value
    go (ML.AcoshF input grad value) = \n -> ML.Acosh (input n) (grad + n) value
    go (ML.AtanhF input grad value) = \n -> ML.Atanh (input n) (grad + n) value
