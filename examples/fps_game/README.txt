Example of a fully-working 3D FPS game using "Castle Game Engine".
We have a level, creatures, items, inventory, player can die,
there are buttons, notifications, custom viewport and more.

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
  DRAFT.engine_tutorial.txt
along with browsing this code. And the engine reference
  http://castle-engine.sourceforge.net/apidoc/html/index.html
should be helpful too.

If you have any questions, see our website on
http://castle-engine.sourceforge.net/
and in particular our forum
https://sourceforge.net/p/castle-engine/discussion/ .
