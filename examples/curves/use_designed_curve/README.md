# Reading curve from XML

Castle Game Engine contains a simple tool to design curves (piecewise cubic Bezier curves), called `castle-curves`, documented on 

  https://castle-engine.io/curves_tool

The created curves can be saved into a simple XML format and loaded into your own Castle Game Engine programs and used for any purpose (for example as a track along which something moves).

This example show how to read an XML file with a curve, display such curve, and use it to follow something (in this case -- a sphere) along the curve.

TODO:
- invent extension, like `.castle-curves`, for such files
- allow to automatically load and display them by TCastleScene

Using [Castle Game Engine](https://castle-engine.io/).

## Building

Compile by:

- [CGE editor](https://castle-engine.io/editor). Just use menu items _"Compile"_ or _"Compile And Run"_.

- Or use [CGE command-line build tool](https://castle-engine.io/build_tool). Run `castle-engine compile` in this directory.

- Or use [Lazarus](https://www.lazarus-ide.org/). Open in Lazarus `cars_demo_standalone.lpi` file and compile / run from Lazarus. Make sure to first register [CGE Lazarus packages](https://castle-engine.io/lazarus).
