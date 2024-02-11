module ML.Backprop where

import Control.Lens
import Data.Functor.Foldable (Recursive (cata))
import ML (d, value)
import ML qualified

calculateGradients :: ML.Value -> ML.Value
calculateGradients = flip (cata go) 1
  where
    go :: ML.ValueF (Float -> ML.Value) -> Float -> ML.Value
    go (ML.ValueF name (ML.Data _ val)) = \n -> ML.Value name (ML.Data n val)
    go (ML.AddF l r (ML.Data grad val)) = \n -> ML.Add (l n) (r n) (ML.Data (grad + n) val)
    go (ML.SubF l r (ML.Data grad val)) = \n -> ML.Sub (l n) (r (-n)) (ML.Data (grad + n) val)
    go (ML.MulF l r (ML.Data grad val)) = \n -> ML.Mul (l (r n ^. d . value * n)) (r (l n ^. d . value * n)) (ML.Data (grad + n) val)
    go (ML.DivF l r (ML.Data grad val)) = \n -> ML.Div (l (1 / r n ^. d . value)) (r (-l n ^. d . value / (r n ^. d . value) ** 2)) (ML.Data (grad + n) val)
    go (ML.AbsF input (ML.Data grad val)) = \n -> ML.Abs (input n) (ML.Data (grad + n) val)
    go (ML.NegF input (ML.Data grad val)) = \n -> ML.Neg (input n) (ML.Data (grad + n) val)
    go (ML.ExpF input (ML.Data grad val)) = \n -> ML.Exp (input n) (ML.Data (grad + n) val)
    go (ML.SignumF input (ML.Data grad val)) = \n -> ML.Signum (input n) (ML.Data (grad + n) val)
    go (ML.LogF input (ML.Data grad val)) = \n -> ML.Log (input n) (ML.Data (grad + n) val)
    go (ML.SinF input (ML.Data grad val)) = \n -> ML.Sin (input n) (ML.Data (grad + n) val)
    go (ML.CosF input (ML.Data grad val)) = \n -> ML.Cos (input n) (ML.Data (grad + n) val)
    go (ML.AsinF input (ML.Data grad val)) = \n -> ML.Asin (input n) (ML.Data (grad + n) val)
    go (ML.AcosF input (ML.Data grad val)) = \n -> ML.Acos (input n) (ML.Data (grad + n) val)
    go (ML.AtanF input (ML.Data grad val)) = \n -> ML.Atan (input n) (ML.Data (grad + n) val)
    go (ML.SinhF input (ML.Data grad val)) = \n -> ML.Sinh (input (n * cosh (input n ^. d . value))) (ML.Data (grad + n) val)
    go (ML.CoshF input (ML.Data grad val)) = \n -> ML.Cosh (input (n * sinh (input n ^. d . value))) (ML.Data (grad + n) val)
    go (ML.AsinhF input (ML.Data grad val)) = \n -> ML.Asinh (input n) (ML.Data (grad + n) val)
    go (ML.AcoshF input (ML.Data grad val)) = \n -> ML.Acosh (input n) (ML.Data (grad + n) val)
    go (ML.AtanhF input (ML.Data grad val)) = \n -> ML.Atanh (input n) (ML.Data (grad + n) val)

zeroGrad :: ML.Value -> ML.Value
zeroGrad = cata go
  where
    go :: ML.ValueF ML.Value -> ML.Value
    go (ML.ValueF name (ML.Data grad value)) = ML.Value name (ML.Data 0 value)
    go (ML.AddF x y (ML.Data grad value)) = ML.Add x y (ML.Data 0 value)
    go (ML.SubF x y (ML.Data grad value)) = ML.Sub x y (ML.Data 0 value)
    go (ML.MulF x y (ML.Data grad value)) = ML.Mul x y (ML.Data 0 value)
    go (ML.DivF x y (ML.Data grad value)) = ML.Div x y (ML.Data 0 value)
    go (ML.AbsF x (ML.Data grad value)) = ML.Abs x (ML.Data 0 value)
    go (ML.NegF x (ML.Data grad value)) = ML.Neg x (ML.Data 0 value)
    go (ML.ExpF x (ML.Data grad value)) = ML.Exp x (ML.Data 0 value)
    go (ML.SignumF x (ML.Data grad value)) = ML.Signum x (ML.Data 0 value)
    go (ML.LogF x (ML.Data grad value)) = ML.Log x (ML.Data 0 value)
    go (ML.SinF x (ML.Data grad value)) = ML.Sin x (ML.Data 0 value)
    go (ML.CosF x (ML.Data grad value)) = ML.Cos x (ML.Data 0 value)
    go (ML.AsinF x (ML.Data grad value)) = ML.Asin x (ML.Data 0 value)
    go (ML.AcosF x (ML.Data grad value)) = ML.Acos x (ML.Data 0 value)
    go (ML.AtanF x (ML.Data grad value)) = ML.Atan x (ML.Data 0 value)
    go (ML.SinhF x (ML.Data grad value)) = ML.Sinh x (ML.Data 0 value)
    go (ML.CoshF x (ML.Data grad value)) = ML.Cosh x (ML.Data 0 value)
    go (ML.AsinhF x (ML.Data grad value)) = ML.Asinh x (ML.Data 0 value)
    go (ML.AcoshF x (ML.Data grad value)) = ML.Acosh x (ML.Data 0 value)
    go (ML.AtanhF x (ML.Data grad value)) = ML.Atanh x (ML.Data 0 value)
