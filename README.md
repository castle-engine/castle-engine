Castle Game Engine
==================

"Castle Game Engine" ( https://castle-engine.sourceforge.io/ )
is an open-source 3D and 2D game engine.

We have many graphic features (shadows, mirrors, screen effects...)
and support many data formats for game assets (X3D, VRML, Collada, Spine...).
We have a nice scene manager, with many optional components
(like instant creatures with simple walking and attacking intelligence).
We're cross-platform (desktop, mobile, web browser plugin).

See https://castle-engine.sourceforge.io/features.php
for the complete list of engine features.

Documentation:

- The "Getting Started" page on
  https://castle-engine.sourceforge.io/documentation.php
  describes what to install and how to compile. See also the section below.

- Manual: See https://castle-engine.sourceforge.io/manual_intro.php .

- API reference:
  Online on https://castle-engine.sourceforge.io/apidoc/html/index.html .
  Offline in doc/reference/ subdirectory of the engine archive
  (if you downloaded the zip or tar.gz engine release),
  just open the file doc/reference/index.html in your WWW browser.

- Numerous examples are provided in the examples/ subdirectory.
  For example take a look at a simple FPS game example in
  the examples/fps_game/ subdirectory.

- Guide to creating game data:
  https://castle-engine.sourceforge.io/creating_data_intro.php

Questions? Ask on our forum: https://castle-engine.sourceforge.io/forum.php .

Support us on http://patreon.com/castleengine .

Compiling
---------

Get Lazarus ( http://www.lazarus.freepascal.org/ )
and install in Lazarus two packages:
- packages/castle_base.lpk and
- packages/castle_components.lpk
Also, compile (but don't install) this package:
- packages/castle_window.lpk
Then you can compile all the examples by opening their .lpi files
and running them as usual from Lazarus.

If you use bare FPC and the command-line, you can:

- Use xxx_compile.sh scripts to compile particular programs using FPC.
  Use "make examples" in the top engine directory to simply compile
  all the examples.

- Use the build tool to compile various examples and your own games
  for many plaforms:
  https://github.com/castle-engine/castle-engine/wiki/Build-Tool

See the "Getting Started" page on
https://castle-engine.sourceforge.io/documentation.php
for more information.

License
-------

The engine is available on the terms of LGPL >= 2 license
with "static linking exception". This is the same license
as used by FPC RTL and Lazarus LCL. In short, you *can* make
commercial and closed-source games using the engine,
you only have to share your modifications to the engine core.
See COPYING.txt for details.

Have fun!

Michalis Kamburelis
