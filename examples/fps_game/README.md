# First-Person Shooter example

Example of a fully-working 3D FPS game using "Castle Game Engine". We have:

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

## TODO

This example doesn't yet use _Castle Game Engine_ editor to setup level and creatures on it.
See https://castle-engine.io/castle_game_engine_for_unity_developers#_why_does_editor_template_3d_fps_game_show_a_different_approach_than_examplesfps_game .
Define `UPCOMING_FPS_GAME_REDESIGN` within `code/gameinitialize.pas` to see the (unfinished) fps_game version using properly CGE editor features.
But note that it doesn't (yet) support creature AI or items pickup.

## Keys

Move and rotate:

- use AWSD or arrow keys,
- you can activate "Mouse Look" by F4 (or click a suitable button on the screen).

Inventory:

- use current item by Enter,
- change current item by [ ] or mouse wheel,
- drop item by R,
- attack (with currently equipped weapon) by Ctrl key.

A lot more keys and mouse shortcuts are instantly working
(and they all are configurable, too).

All input shortcuts are TInputShortcut instances.
The CastleInputs unit has a list of all global shortcuts (useful to allow
users to configure game keymap) in InputsAll variable.
Of course you can use it to present to user some "Configure controls" menu,
see castle-game for example.

The ExtraViewport in right-bottom part of the screen uses Examine camera,
so the view there is fully editable. Try dragging with mouse,
"Home" key restores comfortable initial view,
and see e.g. view3dscene docs for full key/mouse controls list.

## Building

Using [Castle Game Engine](https://castle-engine.io/).

Compile by:

- [CGE editor](https://castle-engine.io/manual_editor.php). Just use menu item _"Compile"_.

- Or use [CGE command-line build tool](https://castle-engine.io/build_tool). Run `castle-engine compile` in this directory.

- Or use [Lazarus](https://www.lazarus-ide.org/). Open in Lazarus `animate_transform_by_code_standalone.lpi` file and compile / run from Lazarus. Make sure to first register [CGE Lazarus packages](https://castle-engine.io/documentation.php).
