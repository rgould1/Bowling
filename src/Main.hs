module Main where

import Scorecard
import Control.Monad
import Control.Monad.Except

makeSC :: [Frame] -> Either String Scorecard
makeSC fs = foldM score empty fs

test :: String -> Maybe Integer -> [Frame] -> IO ()
test desc expected game = do
    either (\err -> putStrLn $ "FAIL: Couldn't construct scorecard: " ++ err)
           (\s -> let total = scoreGame s in
                  if total == expected
                    then putStrLn $ "SUCCESS: " ++ desc ++ " " ++ show s ++ " has a score of " ++ show expected
                    else putStrLn $ "FAILED: " ++ desc ++ " " ++ show s ++ " has a score of " ++ show total ++ ". Was expecting " ++ show expected) $ makeSC game
           
main :: IO ()
main = do
    --  Test scores are as expected
    putStrLn "Test some valid games"
    test "Incomplete game" Nothing [Regular 1 2, Regular 10 0, Regular 5 5]
    test "Perfect game" (Just 300) $ replicate 9 (Regular 10 0) ++ [Final 10 10 10]
    test "Nil game" (Just 0) $ replicate 10 (Regular 0 0)
    test "Final all strikes" (Just 30) $ replicate 9 (Regular 0 0) ++ [Final 10 10 10]
    test "Final spare" (Just 20) $ replicate 9 (Regular 0 0) ++ [Final 5 5 10]

    -- Test fail cases
    putStrLn ""
    putStrLn "Test some invalid games"
    test "Check validation function fails on unexpected score" (Just 30) $ replicate 9 (Regular 0 0) ++ [Final 5 5 10]
    test "Check invalid single ball" Nothing [Regular 20 0]
    test "Check invalid frame" Nothing [Regular 5 6]
    test "Check negative" Nothing [Regular (-5) 15]
    test "Check out of place final frame" Nothing [Final 5 5 5]
    test "Check too many frames" Nothing $ replicate 11 (Regular 5 5)
