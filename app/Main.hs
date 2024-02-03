{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Control.Lens.TH (makeLenses)
import Data.Functor.Foldable.TH (makeBaseFunctor)

data Value
  = Value {_value :: Float}
  | Add {_l :: Value, _r :: Value}
  | Sub {_l :: Value, _r :: Value}
  | Mul {_l :: Value, _r :: Value}
  | Div {_l :: Value, _r :: Value}
  | Abs {_val :: Value}
  | Neg {_val :: Value}
  | Signum {_val :: Value}
  | Relu {_val :: Value}
  deriving (Show, Eq)

$(makeLenses ''Value)
makeBaseFunctor ''Value

instance Num Value where
  (+) = Add
  (*) = Mul
  (-) = Sub
  abs = Abs
  signum = Signum
  negate = Neg
  fromInteger x = Value $ fromInteger x

main :: IO ()
main = putStrLn "Hello, Haskell!"
