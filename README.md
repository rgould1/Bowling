# Bowling

API for creating and evaluting ten-pin bopwling scorecards (see [Ten-Pin Bowling Scoring](https://en.wikipedia.org/wiki/Ten-pin_bowling)).

Requirements:

1. Create an empty score card
2. Given a score card, score a frame
3. Determine if a game is complete - if so, provide the final score

Stretch:
1. Score intermediate frames, not jsut complete game
2. Support for fouls and splits
3. Functions to edit, e.g. replace a frame, remove last frame

Considerations:
1. Consider error handling (Either String a  - could easily generalise to arbitrary monad)
2. Avoid publishing constructor for Scorecard data type so that invariant can be maintained via scoring functions, e.g. each ball and frame total >=0 <=10, max ten frames, special treatment of final frame
