module Main where

import Scorecard
import Control.Monad

main :: IO ()
main = do
  let makeSC :: [Frame] -> IO (Scorecard)
      makeSC fs = either error return $ foldM score empty fs

  scorecard <- makeSC [Regular 1 2, Regular 10 0, Regular 5 5]
  perfect <- makeSC $ replicate 9 (Regular 10 0) ++ [Final 10 10 10]
  zero <- makeSC $ replicate 10 (Regular 0 0)
  thirty <- makeSC $ replicate 9 (Regular 0 0) ++ [Final 10 10 10]
  twenty <- makeSC $ replicate 9 (Regular 0 0) ++ [Final 5 5 10]

  print scorecard
  print perfect
  print zero
  print thirty
  print twenty

  print $ scoreGame scorecard
  print $ scoreGame perfect
  print $ scoreGame zero
  print $ scoreGame thirty
  print $ scoreGame twenty
