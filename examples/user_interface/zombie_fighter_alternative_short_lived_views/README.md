# Zombie fighter - game with multiple views (alternative version with short-lived TCastleView instances)

Demo of Castle Game Engine views (`TCastleView`) to define various game views, like

- main menu
- playing the game
- dialog asking user for something

You can organize your game into such views, it is a nice way of splitting your user interface code into manageable chunks. See https://castle-engine.io/views .

This is an alternative version of the demo in ../zombie_fighter/ . It uses short-lived views, created using `TCastleView.CreateUntilStopped`. This approach to creating views has some advantages, see `TCastleView.CreateUntilStopped` documentation.

Using [Castle Game Engine](https://castle-engine.io/).

## Building

Compile by:

- [CGE editor](https://castle-engine.io/editor). Just use menu items _"Compile"_ or _"Compile And Run"_.

- Or use [CGE command-line build tool](https://castle-engine.io/build_tool). Run `castle-engine compile` in this directory.

- Or use [Lazarus](https://www.lazarus-ide.org/). Open in Lazarus `zombie_fighter_standalone.lpi` file and compile / run from Lazarus. Make sure to first register [CGE Lazarus packages](https://castle-engine.io/lazarus).

- Or use [Delphi](https://www.embarcadero.com/products/Delphi). Open in Delphi `zombie_fighter_standalone.dproj` file and compile / run from Delphi. See [CGE and Delphi](https://castle-engine.io/delphi) documentation for details.
