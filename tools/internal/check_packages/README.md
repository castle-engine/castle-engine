# Check that Castle Game Engine Lazarus (.lpk) and Delphi (.dpk, .dproj) packages are correct

In particular check that they contain all files they should, and none of the files they shouldn't. This checks:

- The packages are complete. Missing files from packages, while not critical, is bothersome E.g. Lazarus will not auto-recompile package if you change a file that wasn't explicitly listed in .lpk.

- The package doesn't use something it should not. E.g. castle_base.lpk should not use any files from src/window/, as they depend on CastleWindow, and castle_base.lpk deliberately doesn't depend on CastleWindow.

See https://castle-engine.io/units_map about CGE subdirectories.

Using [Castle Game Engine](https://castle-engine.io/).

## Building

Compile by:

- [CGE editor](https://castle-engine.io/editor). Just use menu items _"Compile"_ or _"Compile And Run"_.

- Or use [CGE command-line build tool](https://castle-engine.io/build_tool). Run `castle-engine compile` in this directory.

- Or use [Lazarus](https://www.lazarus-ide.org/). Open in Lazarus `check_packages.lpi` file and compile / run from Lazarus. Make sure to first register [CGE Lazarus packages](https://castle-engine.io/lazarus).

- Or use [Delphi](https://www.embarcadero.com/products/Delphi). Open in Delphi `check_packages.dproj` file and compile / run from Delphi. See [CGE and Delphi](https://castle-engine.io/delphi) documentation for details.