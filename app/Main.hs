module Main where

import Data.Tree (drawTree)
import ML
import ML.Graph (forwardPass)

main :: IO ()
main = putStrLn result
  where
    expr :: Value
    expr = 4 * 2 - 44 * 323.2 + 2

    result = drawTree $ forwardPass expr
