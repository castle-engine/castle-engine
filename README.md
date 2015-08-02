Castle Game Engine
==================

"Castle Game Engine" ( http://castle-engine.sourceforge.net/ ) is an open-source 3D and 2D engine for games and other applications. We have many graphic features (shaders, shadows, screen effects...) and support many 3D formats (most notably VRML/X3D). We have a flexible manager of 3D objects, including ready creatures with AI. See http://castle-engine.sourceforge.net/engine.php for a list of features and documentation for developers.

- Tutorial: See http://castle-engine.sourceforge.net/tutorial_intro.php .

- Examples: See examples/ subdirectory. In particular take a look at simple FPS game example in castle_game_engine/examples/fps_game/ .

- API documentation:
  Online on http://castle-engine.sourceforge.net/reference.php .
  Offline in doc/pasdoc/html/ subdirectory of this archive
  (just open doc/pasdoc/html/index.html in your WWW browser).

Questions? Ask on our forum or mailing list, see http://castle-engine.sourceforge.net/forum.php .

Compiling
---------

Use xxx_compile.sh scripts to compile particular examples using FPC.
Use "make examples" in this directory to simply compile all examples.

If you use Lazarus (http://www.lazarus.freepascal.org/),
you'll be interested in installing in Lazarus
packages/castle_base.lpk and packages/castle_components.lpk
(see packages/README.txt for more info).
Then you can compile all the examples by opening their .lpi files
and running them as usual from Lazarus.

License
-------

The engine is available on the terms of LGPL >= 2 license
with static linking exception. This is the same license
as used by FPC RTL and Lazarus LCL.
See COPYING.txt for details.

Have fun!
Michalis Kamburelis, aka Kambi.
