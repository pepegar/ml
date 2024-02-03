module Main where

import ML (Value)

main :: IO ()
main = print expr
  where
    expr :: Value
    expr = 5.5 * 4.4 - 2 ** 33

-- result = eval expr
