{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Control.Lens.TH (makeLenses)
import Data.Functor.Foldable (Recursive (cata))
import Data.Functor.Foldable.TH (makeBaseFunctor)

data Value
  = Value {_value :: Float}
  | Add {_l :: Value, _r :: Value}
  | Sub {_l :: Value, _r :: Value}
  | Mul {_l :: Value, _r :: Value}
  | Div {_l :: Value, _r :: Value}
  | Abs {_val :: Value}
  | Neg {_val :: Value}
  | Exp {_val :: Value}
  | Signum {_val :: Value}
  | Relu {_val :: Value}
  | Log {_val :: Value}
  | Sin {_val :: Value}
  | Cos {_val :: Value}
  | Asin {_val :: Value}
  | Acos {_val :: Value}
  | Atan {_val :: Value}
  | Sinh {_val :: Value}
  | Cosh {_val :: Value}
  | Asinh {_val :: Value}
  | Acosh {_val :: Value}
  | Atanh {_val :: Value}
  deriving (Show, Eq)

$(makeLenses ''Value)
makeBaseFunctor ''Value

eval :: Value -> Float
eval = cata go
  where
    go :: ValueF Float -> Float
    go (ValueF x) = x
    go (AddF x y) = x + y
    go (SubF x y) = x - y
    go (MulF x y) = x * y
    go (DivF x y) = x / y
    go (AbsF x) = abs x
    go (NegF x) = negate x
    go (ExpF x) = exp x
    go (SignumF x) = signum x
    go (ReluF x) = max 0 x
    go (LogF x) = log x
    go (SinF x) = sin x
    go (CosF x) = cos x
    go (AsinF x) = asin x
    go (AcosF x) = acos x
    go (AtanF x) = atan x
    go (SinhF x) = sinh x
    go (CoshF x) = cosh x
    go (AsinhF x) = asinh x
    go (AcoshF x) = acosh x
    go (AtanhF x) = atanh x

instance Num Value where
  (+) = Add
  (*) = Mul
  (-) = Sub
  abs = Abs
  signum = Signum
  negate = Neg
  fromInteger x = Value $ fromInteger x

instance Fractional Value where
  fromRational :: Rational -> Value
  fromRational = Value . fromRational
  (/) :: Value -> Value -> Value
  (/) = Div

instance Floating Value where
  pi = Value pi
  exp = Exp
  log = Log
  sin = Sin
  cos = Cos
  asin = Asin
  acos = Acos
  atan = Atan
  sinh = Sinh
  cosh = Cosh
  asinh = Asinh
  acosh = Acosh
  atanh = Atanh

main :: IO ()
main = print expr
  where
    expr :: Value
    expr = 5.5 * 4.4 - 2 ** 33

-- result = eval expr
