Castle Game Engine
==================

"Castle Game Engine" ( https://castle-engine.io/ )
is an open-source 3D and 2D game engine.

We have many graphic features (shadows, mirrors, screen effects...)
and support many data formats for game assets (X3D, glTF, VRML, Collada, Spine...).
We have a nice scene manager, with many optional components
(like instant creatures with simple walking and attacking intelligence).
We're cross-platform (desktop, mobile).

See https://castle-engine.io/features.php
for the complete list of engine features.

Documentation:

- The "Getting Started" page on
  https://castle-engine.io/documentation.php
  describes what to install and how to compile.
  See also the "Compiling" section below for a short version.

- Manual: See https://castle-engine.io/manual_intro.php .

- API reference:
  Online on https://castle-engine.io/apidoc/html/index.html .
  Offline in doc/reference/ subdirectory of the engine archive
  (if you downloaded the zip or tar.gz engine release),
  just open the file doc/reference/index.html in your WWW browser.

- Numerous examples are provided in the examples/ subdirectory.
  For example take a look at a simple FPS game example in
  the examples/fps_game/ subdirectory.

- Guide to creating game data:
  https://castle-engine.io/creating_data_intro.php

Questions? Talk to us on forum or chat: https://castle-engine.io/talk.php .

Support us on https://www.patreon.com/castleengine .

Compiling
---------

Get Lazarus ( https://www.lazarus-ide.org/ )
and install in Lazarus two packages:

- packages/castle_base.lpk and
- packages/castle_components.lpk

Also, compile (but don't install) this package:

- packages/castle_window.lpk

Now you can compile and run all the examples by opening their .lpi files
and running them as usual from Lazarus.

Alternative:

If you prefer to use bare FPC and the command-line
you can use our build tool (called simply `castle-engine`)
to compile various examples and your own games for many plaforms:
https://github.com/castle-engine/castle-engine/wiki/Build-Tool .
Lazarus and LCL are not necessary in this case.

The build tool relies on the existence of [CastleEngineManifest.xml](https://github.com/castle-engine/castle-engine/wiki/CastleEngineManifest.xml-examples)
file inside your project. The build tool allows to comfortably
compile and package your game for various platforms,
including Android and iOS.

See the "Getting Started" page on
https://castle-engine.io/documentation.php
for more information.

License
-------

The engine is available on the terms of LGPL >= 2 license
with "static linking exception". This is the same license
as used by FPC RTL and Lazarus LCL. In short, you *can* make
commercial and closed-source games using the engine,
you only have to share your modifications to the engine core.
See COPYING.md for details.

Have fun!

Michalis Kamburelis
