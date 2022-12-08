# Physics 3D Shooter

Demo of shooting things with physics in 3D:

- Walk of level with static mesh collider.
- Drop stacks of boxes (each box is a box collider).
- Shoot bullet (each bullet is just another box collider with some initial speed).

Using [Castle Game Engine](https://castle-engine.io/).

## Building

Compile by:

- [CGE editor](https://castle-engine.io/manual_editor.php). Just use menu item _"Compile"_.

- Or use [CGE command-line build tool](https://castle-engine.io/build_tool). Run `castle-engine compile` in this directory.

- Or use [Lazarus](https://www.lazarus-ide.org/). Open in Lazarus `physics_3d_shooter_standalone.lpi` file and compile / run from Lazarus. Make sure to first register [CGE Lazarus packages](https://castle-engine.io/documentation.php).
