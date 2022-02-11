# Zombie fighter - game with multiple states (alternative version with short-lived TUIState instances)

Demo of Castle Game Engine states (`TUIState`) to define various game states, like

- main menu
- playing the game
- dialog asking user for something

You can organize your game into such states, it is a nice way of splitting your user interface code into manageable chunks. See https://castle-engine.io/states .

This is an alternative version of the demo in ../zombie_fighter/ . It uses short-lived states, created using TUIState.CreateUntilStopped. This approach to creating states has some advantages, see TUIState.CreateUntilStopped documentation.

Using [Castle Game Engine](https://castle-engine.io/).

## Building

Compile by:

- [CGE editor](https://castle-engine.io/manual_editor.php). Just use menu item _"Compile"_.

- Or use [CGE command-line build tool](https://castle-engine.io/build_tool). Run `castle-engine compile` in this directory.

- Or use [Lazarus](https://www.lazarus-ide.org/). Open in Lazarus `zombie_fighter_standalone.lpi` file and compile / run from Lazarus. Make sure to first register [CGE Lazarus packages](https://castle-engine.io/documentation.php).
