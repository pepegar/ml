{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module ML where

import Control.Lens.TH (makeLenses, makePrisms)
import Data.Deriving
import Data.Functor.Foldable (Recursive (cata))
import Data.Functor.Foldable.TH (makeBaseFunctor)

data Value
  = Value {_value :: Float, _name :: String, _grad :: Float}
  | Add {_l :: Value, _r :: Value, _grad :: Float, _value :: Float}
  | Sub {_l :: Value, _r :: Value, _grad :: Float, _value :: Float}
  | Mul {_l :: Value, _r :: Value, _grad :: Float, _value :: Float}
  | Div {_l :: Value, _r :: Value, _grad :: Float, _value :: Float}
  | Abs {_input :: Value, _grad :: Float, _value :: Float}
  | Neg {_input :: Value, _grad :: Float, _value :: Float}
  | Exp {_input :: Value, _grad :: Float, _value :: Float}
  | Signum {_input :: Value, _grad :: Float, _value :: Float}
  | Log {_input :: Value, _grad :: Float, _value :: Float}
  | Sin {_input :: Value, _grad :: Float, _value :: Float}
  | Cos {_input :: Value, _grad :: Float, _value :: Float}
  | Asin {_input :: Value, _grad :: Float, _value :: Float}
  | Acos {_input :: Value, _grad :: Float, _value :: Float}
  | Atan {_input :: Value, _grad :: Float, _value :: Float}
  | Sinh {_input :: Value, _grad :: Float, _value :: Float}
  | Cosh {_input :: Value, _grad :: Float, _value :: Float}
  | Asinh {_input :: Value, _grad :: Float, _value :: Float}
  | Acosh {_input :: Value, _grad :: Float, _value :: Float}
  | Atanh {_input :: Value, _grad :: Float, _value :: Float}
  deriving (Eq, Show)

$(makePrisms ''Value)
$(makeLenses ''Value)
makeBaseFunctor ''Value

deriveEq1 ''ValueF
deriveOrd1 ''ValueF
deriveShow1 ''ValueF

eval :: Value -> Float
eval = cata go
  where
    go :: ValueF Float -> Float
    go (ValueF x _ _) = x
    go (AddF x y _ _) = x + y
    go (SubF x y _ _) = x - y
    go (MulF x y _ _) = x * y
    go (DivF x y _ _) = x / y
    go (AbsF x _ _) = abs x
    go (NegF x _ _) = negate x
    go (ExpF x _ _) = exp x
    go (SignumF x _ _) = signum x
    go (LogF x _ _) = log x
    go (SinF x _ _) = sin x
    go (CosF x _ _) = cos x
    go (AsinF x _ _) = asin x
    go (AcosF x _ _) = acos x
    go (AtanF x _ _) = atan x
    go (SinhF x _ _) = sinh x
    go (CoshF x _ _) = cosh x
    go (AsinhF x _ _) = asinh x
    go (AcoshF x _ _) = acosh x
    go (AtanhF x _ _) = atanh x

forwardPass :: Value -> Value
forwardPass = cata go
  where
    go :: ValueF Value -> Value
    go (ValueF value name grad) = Value value name grad
    go (AddF x y grad value) = Add x y grad $ eval (x + y)
    go (SubF x y grad value) = Sub x y grad $ eval (x - y)
    go (MulF x y grad value) = Mul x y grad $ eval (x * y)
    go (DivF x y grad value) = Div x y grad $ eval (x / y)
    go (AbsF x grad value) = Abs x grad $ eval (abs x)
    go (NegF x grad value) = Neg x grad $ eval (negate x)
    go (ExpF x grad value) = Exp x grad $ eval (exp x)
    go (SignumF x grad value) = Signum x grad $ eval (signum x)
    go (LogF x grad value) = Log x grad $ eval (log x)
    go (SinF x grad value) = Sin x grad $ eval (sin x)
    go (CosF x grad value) = Cos x grad $ eval (cos x)
    go (AsinF x grad value) = Asin x grad $ eval (asin x)
    go (AcosF x grad value) = Acos x grad $ eval (acos x)
    go (AtanF x grad value) = Atan x grad $ eval (atan x)
    go (SinhF x grad value) = Sinh x grad $ eval (sinh x)
    go (CoshF x grad value) = Cosh x grad $ eval (cosh x)
    go (AsinhF x grad value) = Asinh x grad $ eval (asinh x)
    go (AcoshF x grad value) = Acosh x grad $ eval (acosh x)
    go (AtanhF x grad value) = Atanh x grad $ eval (atanh x)

instance Num Value where
  x + y = Add x y 0 $ eval (x + y)
  x * y = Mul x y 0 $ eval (x * y)
  x - y = Sub x y 0 $ eval (x - y)
  abs x = Abs x 0 $ eval x
  signum x = Signum x 0 $ eval x
  negate x = Neg x 0 $ eval x
  fromInteger x = Value (fromInteger x) "fromInteger" 1

instance Fractional Value where
  fromRational :: Rational -> Value
  fromRational x = Value (fromRational x) "fromRational" 0
  (/) :: Value -> Value -> Value
  x / y = Div x y 0 $ eval (x / y)

instance Floating Value where
  pi = Value pi "pi" 0
  exp x = Exp x 0 $ eval $ exp x
  log x = Log x 0 $ eval $ log x
  sin x = Sin x 0 $ eval $ sin x
  cos x = Cos x 0 $ eval $ cos x
  asin x = Asin x 0 $ eval $ asin x
  acos x = Acos x 0 $ eval $ acos x
  atan x = Atan x 0 $ eval $ atan x
  sinh x = Sinh x 0 $ eval $ sinh x
  cosh x = Cosh x 0 $ eval $ cosh x
  asinh x = Asinh x 0 $ eval $ asinh x
  acosh x = Acosh x 0 $ eval $ acosh x
  atanh x = Atanh x 0 $ eval $ atanh x
