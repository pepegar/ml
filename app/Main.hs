module Main where

import ML (Value, eval)
import ML.Graph (annotateWithId, toString)

main :: IO ()
main = print result
  where
    expr :: Value
    expr = 5.5 * 4.4 - 2 ** 33

    result = toString expr
