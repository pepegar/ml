module Main where

import ML (Value)
import ML.Graph (annotateWithId, toGraph)

main :: IO ()
main = print result
  where
    expr :: Value
    expr = 5.5 * 4.4 - 2 ** 33

    result = annotateWithId expr
