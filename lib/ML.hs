{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module ML where

import Control.Lens (over, set)
import Control.Lens.TH (makeLenses, makePrisms)
import Data.Deriving
import Data.Functor.Foldable (Recursive (cata))
import Data.Functor.Foldable.TH (makeBaseFunctor)

data Data = Data
  { _grad :: Float,
    _value :: Float
  }
  deriving (Eq, Show, Ord)

$(makeLenses ''Data)

data Value
  = Value {_name :: String, _d :: Data}
  | Add {_l :: Value, _r :: Value, _d :: Data}
  | Sub {_l :: Value, _r :: Value, _d :: Data}
  | Mul {_l :: Value, _r :: Value, _d :: Data}
  | Div {_l :: Value, _r :: Value, _d :: Data}
  | Abs {_input :: Value, _d :: Data}
  | Neg {_input :: Value, _d :: Data}
  | Exp {_input :: Value, _d :: Data}
  | Signum {_input :: Value, _d :: Data}
  | Log {_input :: Value, _d :: Data}
  | Sin {_input :: Value, _d :: Data}
  | Cos {_input :: Value, _d :: Data}
  | Asin {_input :: Value, _d :: Data}
  | Acos {_input :: Value, _d :: Data}
  | Atan {_input :: Value, _d :: Data}
  | Sinh {_input :: Value, _d :: Data}
  | Cosh {_input :: Value, _d :: Data}
  | Asinh {_input :: Value, _d :: Data}
  | Acosh {_input :: Value, _d :: Data}
  | Atanh {_input :: Value, _d :: Data}
  deriving (Eq, Show)

(@=) :: String -> Float -> Value
name @= value = ML.Value name (ML.Data 0 value)

$(makePrisms ''Value)
$(makeLenses ''Value)
$(makeBaseFunctor ''Value)

deriveEq1 ''ValueF
deriveOrd1 ''ValueF
deriveShow1 ''ValueF

eval :: Value -> Float
eval = cata go
  where
    go :: ValueF Float -> Float
    go (ValueF _ (Data _ value)) = value
    go (AddF x y _) = x + y
    go (SubF x y _) = x - y
    go (MulF x y _) = x * y
    go (DivF x y _) = x / y
    go (AbsF x _) = abs x
    go (NegF x _) = negate x
    go (ExpF x _) = exp x
    go (SignumF x _) = signum x
    go (LogF x _) = log x
    go (SinF x _) = sin x
    go (CosF x _) = cos x
    go (AsinF x _) = asin x
    go (AcosF x _) = acos x
    go (AtanF x _) = atan x
    go (SinhF x _) = sinh x
    go (CoshF x _) = cosh x
    go (AsinhF x _) = asinh x
    go (AcoshF x _) = acosh x
    go (AtanhF x _) = atanh x

forwardPass :: Value -> Value
forwardPass = cata go
  where
    go :: ValueF Value -> Value
    go (ValueF name d) = Value name d
    go (AddF x y d) = Add x y $ set value (eval (x + y)) d
    go (SubF x y d) = Sub x y $ set value (eval (x - y)) d
    go (MulF x y d) = Mul x y $ set value (eval (x * y)) d
    go (DivF x y d) = Div x y $ set value (eval (x / y)) d
    go (AbsF x d) = Abs x $ set value (eval (abs x)) d
    go (NegF x d) = Neg x $ set value (eval (negate x)) d
    go (ExpF x d) = Exp x $ set value (eval (exp x)) d
    go (SignumF x d) = Signum x $ set value (eval (signum x)) d
    go (LogF x d) = Log x $ set value (eval (log x)) d
    go (SinF x d) = Sin x $ set value (eval (sin x)) d
    go (CosF x d) = Cos x $ set value (eval (cos x)) d
    go (AsinF x d) = Asin x $ set value (eval (asin x)) d
    go (AcosF x d) = Acos x $ set value (eval (acos x)) d
    go (AtanF x d) = Atan x $ set value (eval (atan x)) d
    go (SinhF x d) = Sinh x $ set value (eval (sinh x)) d
    go (CoshF x d) = Cosh x $ set value (eval (cosh x)) d
    go (AsinhF x d) = Asinh x $ set value (eval (asinh x)) d
    go (AcoshF x d) = Acosh x $ set value (eval (acosh x)) d
    go (AtanhF x d) = Atanh x $ set value (eval (atanh x)) d

instance Num Value where
  x + y = Add x y (Data 0 0)
  x * y = Mul x y (Data 0 0)
  x - y = Sub x y (Data 0 0)
  abs x = Abs x (Data 0 0)
  signum x = Signum x (Data 0 0)
  negate x = Neg x (Data 0 0)
  fromInteger x = Value "fromInteger" (Data 0 0)

instance Fractional Value where
  fromRational :: Rational -> Value
  fromRational x = Value "fromRational" (Data 0 (fromRational x))
  (/) :: Value -> Value -> Value
  x / y = Div x y (Data 0 0)

instance Floating Value where
  pi = Value "pi" (Data 0 pi)
  exp x = Exp x (Data 0 0)
  log x = Log x (Data 0 0)
  sin x = Sin x (Data 0 0)
  cos x = Cos x (Data 0 0)
  asin x = Asin x (Data 0 0)
  acos x = Acos x (Data 0 0)
  atan x = Atan x (Data 0 0)
  sinh x = Sinh x (Data 0 0)
  cosh x = Cosh x (Data 0 0)
  asinh x = Asinh x (Data 0 0)
  acosh x = Acosh x (Data 0 0)
  atanh x = Atanh x (Data 0 0)
