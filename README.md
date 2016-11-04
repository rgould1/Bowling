## Bowling

API for creating and evaluting ten-pin bopwling scorecards (see [Ten-Pin Bowling Scoring](https://en.wikipedia.org/wiki/Ten-pin_bowling)).

# Running
Everything is written in Haskell. The API is all contained in `src/Scorecard.hs`. A simple test driver is in `src/Main.hs`. With [`stack`](http://www.haskellstack.org), running should be just be a matter of `stack build && stack exec Bowling` from the project directory.

# Requirements

1. Create an empty score card
2. Given a score card, score a frame
3. Determine if a game is complete - if so, provide the final score

# Stretch
1. Score intermediate frames, not just complete game
2. Support for fouls and splits
3. Functions to edit, e.g. replace a frame, remove last frame
4. Handle special named runs, e.g. turkey (three strikes in a row)
5. Read handler for parsing games represented as text, e.g. "10|5/|X|--|25"

# Considerations
1. Consider error handling (Either String a  - could easily generalise to arbitrary monad)
2. Avoid publishing constructor for Scorecard data type so that invariant can be maintained via scoring functions, e.g. each ball and frame total >=0 <=10, max ten frames, special treatment of final frame
