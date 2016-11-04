module Scorecard
(
    Frame(..),
    Scorecard(),  -- don't expose constructors
    empty, frames, score,
    scoreGame
)
where

import Control.Monad.Except
import Data.List

data Frame = Regular Integer Integer | Final Integer Integer Integer

data Scorecard = Scorecard [Frame]

instance Show Frame where
    show (Regular a b) | a==10   = "X"
                       | a+b==10 = ball a ++ "/"
                       | otherwise = ball a ++ ball b
    show (Final a b c) | a==10   = "X" ++ ball b ++ ball c
                       | a+b==10 = ball a ++ "/" ++ ball c

ball x | x==0  = "-"
       | x==10 = "X"
       | otherwise = show x

instance Show Scorecard where
    show (Scorecard fs) = intercalate "|" (map show fs)

type Error = Either String

-- Construct empty scorecard
empty :: Scorecard
empty = Scorecard []

-- Show frames
frames :: Scorecard -> [Frame]
frames (Scorecard fs) = fs

-- Check individual score is between 0 and 10
checkBall :: Integer -> Error Integer
checkBall s | 0 <= s && s <=10 = return s
            | otherwise = throwError "Individual scores must be between 0 and 10"

-- Score a frame
score :: Scorecard -> Frame -> Either String Scorecard
score (Scorecard fs) f@(Regular a b) = do
    a' <- checkBall a
    b' <- checkBall b
    when (a' + b' > 10) $ throwError "Frame can't add to greater than 10"
    when (length fs>=10) $ throwError "Scorecard is complete"
    return $ Scorecard (fs ++ [f])
score (Scorecard fs) f@(Final a b c) = do
    a' <- checkBall a
    b' <- checkBall b
    c' <- checkBall c
    when (a'/=10 && a'+b'/=10) $ throwError "Final frame can only be used if a strike or spare is bowled inthe last frame of the game"
    when (length fs/=9) $ throwError "Final can only be used for last frame of game"
    return $ Scorecard (fs ++ [f])

data Carry = None | Spare | Strike | DoubleStrike

-- Score game if complete
scoreGame :: Scorecard -> Maybe Integer
scoreGame (Scorecard fs) | length fs<10 = Nothing
                         | otherwise = Just $ fst $ foldl scoreFrame (0,None) fs
    where
        carry c a b = case c of
                        None   -> 0
                        Spare  -> a
                        Strike -> a+b
                        DoubleStrike -> 2*a+b
        newC c a b | a==10     = case c of
                                    Strike       -> DoubleStrike
                                    DoubleStrike -> DoubleStrike
                                    _            -> Strike
                   | a+b==10   = Spare
                   | otherwise = None
        scoreFrame (s, c) (Regular b1 b2) = (s + b1 + b2 + carry c b1 b2, newC c b1 b2)
        scoreFrame (s, c) (Final b1 b2 b3) = (s + b1 + b2 + b3 + carry c b1 b2, None)
