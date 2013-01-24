Example of a fully-working 3D FPS game using "Castle Game Engine".
We have level, creatures, items (medkit and weapon), inventory,
player can be hurt and die, player can shoot and kill the creatures,
bump mapping, move limit, sectors/waypoints (for AI),
buttons, notifications, custom viewport, sounds, sky, water, footsteps
and a *lot* of comments.

You can compile this example (and any other example here, for that matter)
using Lazarus (http://www.lazarus.freepascal.org/):

- If you haven't done it yet, open and compile Lazarus packages of our engine:

  castle_game_engine/packages/castle_base.lpk
  castle_game_engine/packages/castle_window.lpk
  castle_game_engine/packages/castle_components.lpk

  For the purpose of this example fps_game, you only need to open
  these packages and compile them, no need to install them in Lazarus.
  But you may want to eventually install castle_components.lpk
  (with castle_base.lpk as dependency) to have some components that you
  can drop on normal Lazarus form (first of all, TCastleControl),
  see examples/lazarus/ for examples using Lazarus forms.

- Open Lazarus project fps_game.lpi, and compile + run :)

The example code, as well as the data files in data/ subdirectory,
are full of (hopefully) helpful comments :)
You will probably want to read engine tutorial, see
  http://castle-engine.sourceforge.net/tutorial_intro.php
along with browsing this code. And the engine reference
  http://castle-engine.sourceforge.net/apidoc/html/index.html
should be helpful too.

If you have any questions, see our website on
http://castle-engine.sourceforge.net/
and in particular our forum
https://sourceforge.net/p/castle-engine/discussion/ .

------------------------------------------------------------------------------
Keys: move using AWSD or arrow keys, and you can activate "Mouse Look"
by a suitable button on the screen.

Inventory:
use current item by Enter,
change current item by [ ] or mouse wheel,
drop item by R,
attack (with currently equipped weapon) by Ctrl key.

A lot more keys and mouse shortcuts are instantly working
(and they all are configurable, too).
All input shortcuts are TInputShortcut instances.
Currently the following units of our engine define some shortcuts:

- CastleSceneManager (Input_Xxx global properties)
- Cameras unit (Input_Xxx properties specific to each camera class)
- CastlePlayer unit (PlayerInput_Xxx global properties, these override
  some camera shortcuts when Player is used)

The CastleInputs unit has a list of all global shortcuts (useful to allow
users to configure game keymap) in InputsAll variable.
Of course you can use it to present to user some "Configure controls" menu,
see castle1 game for example.

Oh, and the ExtraViewport in right-bottom part of the screen uses Examine camera,
so the view there is fully editable. Try dragging with mouse,
"Home" key restores comfortable initial view,
and see e.g. view3dscene docs for full key/mouse controls list.
