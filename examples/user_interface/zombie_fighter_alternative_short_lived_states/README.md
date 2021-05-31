Demo of Castle Game Engine states (TUIState) to define
various game states, like

- main menu
- playing the game
- dialog asking user for something

You can organize your game into such states, it is a nice way
of splitting your user interface code into manageable chunks.
See https://castle-engine.io/manual_2d_user_interface.php .

This is an alternative version of the demo in ../zombie_fighter/ .
It uses short-lived states, created using TUIState.CreateUntilStopped.
This approach to creating states has some advantages,
see TUIState.CreateUntilStopped documentation.
