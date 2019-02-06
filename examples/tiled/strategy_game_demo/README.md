Demo game using maps designed in Tiled and Castle Game Engine.

Shows also how to use TUIState together with designs done in
Castle Game Engine editor. Namely, in each overridden TUIState.Start,
just call "InsertUserInterface('castle-data:/xxxx.castle-user-interface', ...)".

The actual game logic is mostly inside GameStatePlay and GameUnit units.
