# Test of various cross-platform CGE capabilities, esp. to check we support everything on mobile

This is a cross-platform CGE application that tests various engine features:

- shows some 2D controls (buttons, images),
- shows a viewport with 3D scene and screen effect,
- allows to navigate in the viewport even on mobile, using TCastleTouchNavigation,
- plays some sounds,
- makes vibrations (on mobile),
- tests modal boxes (OK on desktops and Android, but not on iOS).

See https://castle-engine.io/manual_cross_platform.php for information how to compile and run it on various devices.

Using [Castle Game Engine](https://castle-engine.io/).

## Building

Compile by:

- [CGE editor](https://castle-engine.io/editor). Just use menu items _"Compile"_ or _"Compile And Run"_.

- Or use [CGE command-line build tool](https://castle-engine.io/build_tool). Run `castle-engine compile` in this directory.

- Or use [Lazarus](https://www.lazarus-ide.org/). Open in Lazarus `simple_3d_demo_standalone.lpi` file and compile / run from Lazarus. Make sure to first register [CGE Lazarus packages](https://castle-engine.io/lazarus).

- Or use [Delphi](https://www.embarcadero.com/products/Delphi). Open in Delphi `simple_3d_demo_standalone.dproj` file and compile / run from Delphi. See [CGE and Delphi](https://castle-engine.io/delphi) documentation for details.
