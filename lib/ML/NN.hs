module ML.NN where

import Control.Lens (Lens', lens, (^.))
import Control.Monad (replicateM)
import ML qualified
import System.Random (Random, randomRIO, uniform)

data Neuron a where
  Neuron :: (Floating a) => [ML.Value a] -> ML.Value a -> Neuron a

mkNeuron :: (Random a, Floating a) => Int -> IO (Neuron a)
mkNeuron nin = do
  let lb = -1
  let ub = 1
  bias <- ML.mkValue "bias" <$> randomRIO (lb, ub)
  weights <- replicateM nin (ML.mkValue "w" <$> randomRIO (lb, ub))
  pure $ Neuron weights bias

_weights :: Neuron a -> [ML.Value a]
_weights (Neuron w _) = w

updateWeights :: Neuron a -> [ML.Value a] -> Neuron a
updateWeights (Neuron _ bias) weights = Neuron weights bias

weights :: Lens' (Neuron a) [ML.Value a]
weights = lens _weights updateWeights

_bias :: Neuron a -> ML.Value a
_bias (Neuron _ b) = b

updateBias :: Neuron a -> ML.Value a -> Neuron a
updateBias (Neuron weights _) = Neuron weights

bias :: Lens' (Neuron a) (ML.Value a)
bias = lens _bias updateBias

forwardPass :: (Floating a) => [ML.Value a] -> Neuron a -> ML.Value a
forwardPass xs neuron =
  let w = neuron ^. weights
      b = neuron ^. bias
      act = sum $ [wi * xi | (wi, xi) <- zip w xs] ++ [b]
      o = tanh act
   in ML.forwardPass o

data Layer a where
  Layer :: (Floating a) => [Neuron a] -> Layer a

mkLayer :: (Random a, Floating a) => Int -> Int -> IO (Layer a)
mkLayer nin nout = do
  neurons <- replicateM nout $ mkNeuron nin
  return $ Layer neurons

forwardPassL :: (Floating a) => [ML.Value a] -> Layer a -> [ML.Value a]
forwardPassL xs (Layer neurons) = forwardPass xs <$> neurons

data MLP a where
  MLP :: (Floating a) => [Layer a] -> MLP a

windowed :: Int -> [a] -> [[a]]
windowed size ls =
  case ls of
    [] -> []
    x : xs ->
      if length ls >= size
        then take size ls : windowed size xs
        else windowed size xs

toLayer :: (Random a, Floating a) => [Int] -> IO (Layer a)
toLayer (x : y : xs) = mkLayer x y

mkMLP :: (Random a, Floating a) => [Int] -> IO (MLP a)
mkMLP sizes = do
  let windows = windowed 2 sizes
  layers <- traverse toLayer windows
  return $ MLP layers

forwardPassMLP :: (Floating a) => [ML.Value a] -> MLP a -> [ML.Value a]
forwardPassMLP xs (MLP layers) = foldl forwardPassL xs layers
