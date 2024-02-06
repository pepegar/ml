{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module ML where

import Control.Lens.TH (makeLenses, makePrisms)
import Data.Deriving
import Data.Functor.Foldable (Recursive (cata))
import Data.Functor.Foldable.TH (makeBaseFunctor)

data Value
  = Value {_value :: Float, _name :: String, _grad :: Float}
  | Add {_l :: Value, _r :: Value, _grad :: Float}
  | Sub {_l :: Value, _r :: Value, _grad :: Float}
  | Mul {_l :: Value, _r :: Value, _grad :: Float}
  | Div {_l :: Value, _r :: Value, _grad :: Float}
  | Abs {_val :: Value, _grad :: Float}
  | Neg {_val :: Value, _grad :: Float}
  | Exp {_val :: Value, _grad :: Float}
  | Signum {_val :: Value, _grad :: Float}
  | Relu {_val :: Value, _grad :: Float}
  | Log {_val :: Value, _grad :: Float}
  | Sin {_val :: Value, _grad :: Float}
  | Cos {_val :: Value, _grad :: Float}
  | Asin {_val :: Value, _grad :: Float}
  | Acos {_val :: Value, _grad :: Float}
  | Atan {_val :: Value, _grad :: Float}
  | Sinh {_val :: Value, _grad :: Float}
  | Cosh {_val :: Value, _grad :: Float}
  | Asinh {_val :: Value, _grad :: Float}
  | Acosh {_val :: Value, _grad :: Float}
  | Atanh {_val :: Value, _grad :: Float}
  deriving (Show, Eq)

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
    go (AddF x y _) = x + y
    go (SubF x y _) = x - y
    go (MulF x y _) = x * y
    go (DivF x y _) = x / y
    go (AbsF x _) = abs x
    go (NegF x _) = negate x
    go (ExpF x _) = exp x
    go (SignumF x _) = signum x
    go (ReluF x _) = max 0 x
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

instance Num Value where
  x + y = Add x y 0
  x * y = Mul x y 0
  x - y = Sub x y 0
  abs = flip Abs 0
  signum = flip Signum 0
  negate = flip Neg 0
  fromInteger x = Value (fromInteger x) "fromInteger" 1

instance Fractional Value where
  fromRational :: Rational -> Value
  fromRational x = Value (fromRational x) "fromRational" 0
  (/) :: Value -> Value -> Value
  x / y = Div x y 0

instance Floating Value where
  pi = Value pi "pi" 0
  exp = flip Exp 0
  log = flip Log 0
  sin = flip Sin 0
  cos = flip Cos 0
  asin = flip Asin 0
  acos = flip Acos 0
  atan = flip Atan 0
  sinh = flip Sinh 0
  cosh = flip Cosh 0
  asinh = flip Asinh 0
  acosh = flip Acosh 0
  atanh = flip Atanh 0
