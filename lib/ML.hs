module ML where

import Control.Lens (Lens', lens, over, set)
import Control.Lens.TH (makeLenses, makePrisms)
import Control.Monad (MonadPlus (mzero))
import Data.Deriving
import Data.Functor.Foldable (Base, Corecursive (..), Recursive (..))
import System.Random (Random)

data Data a where
  Data :: (Floating a) => a -> a -> Data a

deriving instance (Eq a) => Eq (Data a)

deriving instance (Ord a) => Ord (Data a)

instance (Show a) => Show (Data a) where
  show :: (Show a) => Data a -> String
  show (Data grad value) = "Data { _grad = " ++ show grad ++ ", _value = " ++ show value ++ "}"

_value :: Data a -> a
_value (Data _ v) = v

_grad :: Data a -> a
_grad (Data g _) = g

updateA :: Data a -> a -> Data a
updateA (Data g _) = Data g

updateG :: Data a -> a -> Data a
updateG (Data _ a) g = Data g a

value :: Lens' (Data a) a
value = lens _value updateA

grad :: Lens' (Data a) a
grad = lens _grad updateG

data Value a where
  Value :: (Floating a) => Data a -> Value a
  Add :: Value a -> Value a -> Data a -> Value a
  Sub :: Value a -> Value a -> Data a -> Value a
  Mul :: Value a -> Value a -> Data a -> Value a
  Div :: Value a -> Value a -> Data a -> Value a
  Abs :: Value a -> Data a -> Value a
  Neg :: Value a -> Data a -> Value a
  Exp :: Value a -> Data a -> Value a
  Signum :: Value a -> Data a -> Value a
  Log :: Value a -> Data a -> Value a
  Sin :: Value a -> Data a -> Value a
  Asin :: Value a -> Data a -> Value a
  Acos :: Value a -> Data a -> Value a
  Atan :: Value a -> Data a -> Value a

updateD :: Value a -> Data a -> Value a
updateD (Value _) d = Value d
updateD (Add x y _) d = Add x y d
updateD (Sub x y _) d = Sub x y d
updateD (Mul x y _) d = Mul x y d
updateD (Div x y _) d = Div x y d
updateD (Abs x _) d = Abs x d
updateD (Neg x _) d = Neg x d
updateD (Exp x _) d = Exp x d
updateD (Signum x _) d = Signum x d
updateD (Log x _) d = Log x d
updateD (Sin x _) d = Sin x d
updateD (Asin x _) d = Asin x d
updateD (Acos x _) d = Acos x d
updateD (Atan x _) d = Atan x d

_d :: Value a -> Data a
_d (Value d) = d
_d (Add x y d) = d
_d (Sub x y d) = d
_d (Mul x y d) = d
_d (Div x y d) = d
_d (Abs x d) = d
_d (Neg x d) = d
_d (Exp x d) = d
_d (Signum x d) = d
_d (Log x d) = d
_d (Sin x d) = d
_d (Asin x d) = d
_d (Acos x d) = d
_d (Atan x d) = d

d :: Lens' (Value a) (Data a)
d = lens _d updateD

mkValue :: (Floating a) => a -> Value a
mkValue value = ML.Value (Data 0 value)

$(makePrisms ''Value)
$(makeLenses ''Value)

data ValueF a b where
  ValueF :: (Floating a) => Data a -> ValueF a b
  AddF :: b -> b -> Data a -> ValueF a b
  SubF :: b -> b -> Data a -> ValueF a b
  MulF :: b -> b -> Data a -> ValueF a b
  DivF :: b -> b -> Data a -> ValueF a b
  AbsF :: b -> Data a -> ValueF a b
  NegF :: b -> Data a -> ValueF a b
  ExpF :: b -> Data a -> ValueF a b
  SignumF :: b -> Data a -> ValueF a b
  LogF :: b -> Data a -> ValueF a b
  SinF :: b -> Data a -> ValueF a b
  AsinF :: b -> Data a -> ValueF a b
  AcosF :: b -> Data a -> ValueF a b
  AtanF :: b -> Data a -> ValueF a b

instance Functor (ValueF a) where
  fmap :: (a2 -> b) -> ValueF a a2 -> ValueF a b
  fmap f (ValueF d) = ValueF d
  fmap f (AddF x y d) = AddF (f x) (f y) d
  fmap f (SubF x y d) = SubF (f x) (f y) d
  fmap f (MulF x y d) = MulF (f x) (f y) d
  fmap f (DivF x y d) = DivF (f x) (f y) d
  fmap f (AbsF x d) = AbsF (f x) d
  fmap f (NegF x d) = NegF (f x) d
  fmap f (ExpF x d) = ExpF (f x) d
  fmap f (SignumF x d) = SignumF (f x) d
  fmap f (LogF x d) = LogF (f x) d
  fmap f (SinF x d) = SinF (f x) d
  fmap f (AsinF x d) = AsinF (f x) d
  fmap f (AcosF x d) = AcosF (f x) d
  fmap f (AtanF x d) = AtanF (f x) d

instance Foldable (ValueF a) where
  foldMap :: (Monoid m) => (b -> m) -> ValueF a b -> m
  foldMap f (ValueF d) = mempty
  foldMap f (AddF x y d) = f x <> f y
  foldMap f (SubF x y d) = f x <> f y
  foldMap f (MulF x y d) = f x <> f y
  foldMap f (DivF x y d) = f x <> f y
  foldMap f (AbsF x d) = f x
  foldMap f (NegF x d) = f x
  foldMap f (ExpF x d) = f x
  foldMap f (SignumF x d) = f x
  foldMap f (LogF x d) = f x
  foldMap f (SinF x d) = f x
  foldMap f (AsinF x d) = f x
  foldMap f (AcosF x d) = f x
  foldMap f (AtanF x d) = f x

instance Traversable (ValueF a) where
  traverse :: (Applicative f) => (a2 -> f b) -> ValueF a1 a2 -> f (ValueF a1 b)
  traverse f (ValueF d) = pure $ ValueF d
  traverse f (AddF x y d) = AddF <$> f x <*> f y <*> pure d
  traverse f (SubF x y d) = SubF <$> f x <*> f y <*> pure d
  traverse f (MulF x y d) = MulF <$> f x <*> f y <*> pure d
  traverse f (DivF x y d) = DivF <$> f x <*> f y <*> pure d
  traverse f (AbsF x d) = AbsF <$> f x <*> pure d
  traverse f (NegF x d) = NegF <$> f x <*> pure d
  traverse f (ExpF x d) = ExpF <$> f x <*> pure d
  traverse f (SignumF x d) = SignumF <$> f x <*> pure d
  traverse f (LogF x d) = LogF <$> f x <*> pure d
  traverse f (SinF x d) = SinF <$> f x <*> pure d
  traverse f (AsinF x d) = AsinF <$> f x <*> pure d
  traverse f (AcosF x d) = AcosF <$> f x <*> pure d
  traverse f (AtanF x d) = AtanF <$> f x <*> pure d

type instance Base (Value a) = ValueF a

instance Recursive (Value a) where
  project (Value d) = ValueF d
  project (Add x y d) = AddF x y d
  project (Sub x y d) = SubF x y d
  project (Mul x y d) = MulF x y d
  project (Div x y d) = DivF x y d
  project (Abs x d) = AbsF x d
  project (Neg x d) = NegF x d
  project (Exp x d) = ExpF x d
  project (Signum x d) = SignumF x d
  project (Log x d) = LogF x d
  project (Sin x d) = SinF x d
  project (Asin x d) = AsinF x d
  project (Acos x d) = AcosF x d
  project (Atan x d) = AtanF x d

instance Corecursive (Value a) where
  embed (ValueF d) = Value d
  embed (AddF x y d) = Add x y d
  embed (SubF x y d) = Sub x y d
  embed (MulF x y d) = Mul x y d
  embed (DivF x y d) = Div x y d
  embed (AbsF x d) = Abs x d
  embed (NegF x d) = Neg x d
  embed (ExpF x d) = Exp x d
  embed (SignumF x d) = Signum x d
  embed (LogF x d) = Log x d
  embed (SinF x d) = Sin x d
  embed (AsinF x d) = Asin x d
  embed (AcosF x d) = Acos x d
  embed (AtanF x d) = Atan x d

eval :: (Floating a) => Value a -> a
eval = cata go
  where
    go (ValueF (Data _ value)) = value
    go (AddF x y d) = x + y
    go (SubF x y _) = x - y
    go (MulF x y _) = x * y
    go (DivF x y _) = x / y
    go (AbsF x _) = abs x
    go (NegF x _) = negate x
    go (ExpF x _) = exp x
    go (SignumF x _) = signum x
    go (LogF x _) = log x
    go (SinF x _) = sin x
    go (AsinF x _) = asin x
    go (AcosF x _) = acos x
    go (AtanF x _) = atan x

forwardPass :: (Floating a) => Value a -> Value a
forwardPass = cata go
  where
    go :: (Floating a) => ValueF a (Value a) -> Value a
    go (ValueF d) = Value d
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
    go (AsinF x d) = Asin x $ set value (eval (asin x)) d
    go (AcosF x d) = Acos x $ set value (eval (acos x)) d
    go (AtanF x d) = Atan x $ set value (eval (atan x)) d

instance (Floating a) => Num (Value a) where
  x + y = Add x y (Data 0 0)
  x * y = Mul x y (Data 0 0)
  x - y = Sub x y (Data 0 0)
  abs x = Abs x (Data 0 0)
  signum x = Signum x (Data 0 0)
  negate x = Neg x (Data 0 0)
  fromInteger x = Value (Data 0 $ fromInteger x)

instance (Floating a) => Fractional (Value a) where
  fromRational :: Rational -> Value a
  fromRational x = Value (Data 0 (fromRational x))
  (/) :: Value a -> Value a -> Value a
  x / y = Div x y (Data 0 0)

instance (Floating a) => Floating (Value a) where
  pi = Value (Data 0 pi)
  exp x = Exp x (Data 0 0)
  log x = Log x (Data 0 0)
  sin x = Sin x (Data 0 0)
  cos x = sin ((pi / 2) - x)
  asin x = Asin x (Data 0 0)
  acos x = Acos x (Data 0 0)
  atan x = Atan x (Data 0 0)
  sinh x = (exp x - exp (negate x)) / 2
  cosh x = (exp x + exp (negate x)) / 2
  asinh x = log (x + sqrt (x ** 2 + 1))
  acosh x = log (x + sqrt (x ** 2 - 1))
  atanh x = log ((1 + x) / (1 - x)) / 2
