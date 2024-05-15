# Using space-filling curves

Demo using PeanoCurve and HilbertCurve. They are space-filling curves, useful to implement ray-tracers, if you want to best use caching on the ray-tracer side (that works best if each rendered pixel is adjacent to previous).

This is deprecated -- because ray-tracers, esp. CPU ray-tracers, are not the focus of Castle Game Engine. And these space-filling curves didn't actually achieve much noticeable benefit anyway.

Using [Castle Game Engine](https://castle-engine.io/).

## Building

Compile by:

- [CGE editor](https://castle-engine.io/editor). Just use menu items _"Compile"_ or _"Compile And Run"_.

- Or use [CGE command-line build tool](https://castle-engine.io/build_tool). Run `castle-engine compile` in this directory.

- Or use [Lazarus](https://www.lazarus-ide.org/). Open in Lazarus `cars_demo_standalone.lpi` file and compile / run from Lazarus. Make sure to first register [CGE Lazarus packages](https://castle-engine.io/lazarus).
