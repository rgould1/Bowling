module Main where

import Scorecard
import Control.Monad

main :: IO ()
main = do
  let fs = [Regular 1 2, Regular 10 0, Regular 5 5]
      Right scorecard = foldM score empty fs

  print scorecard
