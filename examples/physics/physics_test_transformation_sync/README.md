# Physics Test Transformation Synchronization

Trivial test that you can change from code transformation of an object affected by physics, by trivial

```
Sphere.Translation := Vector3(300, 500, 0);
```

And the `Sphere` still continues to be affected by gravity.

Using [Castle Game Engine](https://castle-engine.io/).

## Building

Compile by:

- [CGE editor](https://castle-engine.io/editor). Just use menu items _"Compile"_ or _"Compile And Run"_.

- Or use [CGE command-line build tool](https://castle-engine.io/build_tool). Run `castle-engine compile` in this directory.

- Or use [Lazarus](https://www.lazarus-ide.org/). Open in Lazarus `Fizyka2D_standalone.lpi` file and compile / run from Lazarus. Make sure to first register [CGE Lazarus packages](https://castle-engine.io/lazarus).
