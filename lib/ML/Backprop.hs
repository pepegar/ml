module ML.Backprop where

import Control.Lens
import Data.Functor.Foldable (Recursive (cata))
import ML (d, value)
import ML qualified

calculateGradients :: (Floating a) => ML.Value a -> ML.Value a
calculateGradients = flip (cata go) 1
  where
    go (ML.ValueF (ML.Data _ val)) = \n -> ML.Value (ML.Data n val)
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
    go (ML.AsinF input (ML.Data grad val)) = \n -> ML.Asin (input n) (ML.Data (grad + n) val)
    go (ML.AcosF input (ML.Data grad val)) = \n -> ML.Acos (input n) (ML.Data (grad + n) val)
    go (ML.AtanF input (ML.Data grad val)) = \n -> ML.Atan (input n) (ML.Data (grad + n) val)

zeroGrad :: (Floating a) => ML.Value a -> ML.Value a
zeroGrad = cata go
  where
    go (ML.ValueF (ML.Data grad value)) = ML.Value (ML.Data 0 value)
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
    go (ML.AsinF x (ML.Data grad value)) = ML.Asin x (ML.Data 0 value)
    go (ML.AcosF x (ML.Data grad value)) = ML.Acos x (ML.Data 0 value)
    go (ML.AtanF x (ML.Data grad value)) = ML.Atan x (ML.Data 0 value)
