# First-Person Shooter example

**TODO: This example is in the middle of a rework.** It not yet fully functional:

- Not all creatures have AI. This is in progress, see `TCastleMoveAttack`, https://castle-engine.io/wp/2024/10/21/demo-game-using-blender-sketchfab-quaternius-models-tcastlemoveattack-early-preview-of-tcastlemoveattack-merged-slides-from-conference-last-week/ . On creatures that have AI, you can shoot them.

- No items pickup and inventory.

Example of a fully-working 3D FPS game using _Castle Game Engine_.

With level designed using _Castle Game Engine_.

![Screenshot working in editor](screenshot_editor.png)
![Screenshot 2](screenshot_2.png)
![Screenshot](screenshot.png)

We have:

- level,
- creatures,
- items (medkit and weapon),
- inventory,
- player can be hurt and die,
- player can shoot and kill the creatures,
- bump mapping,
- move limit,
- sectors/waypoints (for AI),
- buttons,
- notifications,
- custom viewport,
- sounds,
- sky,
- water,
- footsteps
- and a *lot* of comments.

## Inputs

Attack (with currently equipped weapon) by clicking with left mouse button.

Move and rotate:

- use AWSD or arrow keys,
- you can activate "Mouse Look" by F4 or holding the right mouse button,
- hold Shift to run.

TODO: Inventory:

- use current item by Enter,
- change current item by [ ] or mouse wheel,
- drop item by R.

Misc:

- screenshot: F5.
- fake win: P,
- fake death: O.

A lot more keys and mouse shortcuts are instantly working
(and they all are configurable, too).

The `MapViewport` in right-bottom part of the screen shows a map view top.
"Examine" view is available and you can zoom in/out using scroll.

## Building

Using [Castle Game Engine](https://castle-engine.io/).

Compile by:

- [CGE editor](https://castle-engine.io/editor). Just use menu items _"Compile"_ or _"Compile And Run"_.

- Or use [CGE command-line build tool](https://castle-engine.io/build_tool). Run `castle-engine compile` in this directory.

- Or use [Lazarus](https://www.lazarus-ide.org/). Open in Lazarus `fps_game.lpi` file and compile / run from Lazarus. Make sure to first register [CGE Lazarus packages](https://castle-engine.io/lazarus).

- Or use [Delphi](https://www.embarcadero.com/products/Delphi). Open in Delphi `fps_game.dproj` file and compile / run from Delphi. See [CGE and Delphi](https://castle-engine.io/delphi) documentation for details.
