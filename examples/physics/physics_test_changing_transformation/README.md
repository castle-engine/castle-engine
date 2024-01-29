# Physics Test Changing Transformation

Test that you can change from code transformation of an object affected by physics. You can change it by setting `Translation` or (more physically-correct) updating `LinearVelocity`.

You can change transformation on
- dynamic rigid bodies (`Dynamic` = `true`, `Animated` = doesn't matter)
- animated rigid bodies (`Dynamic` = `false`, `Animated` = `true`)

Using [Castle Game Engine](https://castle-engine.io/).

## Building

Compile by:

- [CGE editor](https://castle-engine.io/editor). Just use menu items _"Compile"_ or _"Compile And Run"_.

- Or use [CGE command-line build tool](https://castle-engine.io/build_tool). Run `castle-engine compile` in this directory.

- Or use [Lazarus](https://www.lazarus-ide.org/). Open in Lazarus `physics_test_changing_transformation_standalone.lpi` file and compile / run from Lazarus. Make sure to first register [CGE Lazarus packages](https://castle-engine.io/lazarus).

- Or use [Delphi](https://www.embarcadero.com/products/Delphi). Open in Delphi `physics_test_changing_transformation_standalone.dproj` file and compile / run from Delphi. See [CGE and Delphi](https://castle-engine.io/delphi) documentation for details.
