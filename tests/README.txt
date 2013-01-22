Automatic tests of Castle Game Engine.

In the spirit of
<a href="http://www.extremeprogramming.org/">Extreme Programming</a>,
using
<a href="http://camelos.sourceforge.net/fpcUnit.html">fpcunit</a>.

Compile and run them from Lazarus to see a result in a nice GUI window.

You can also compile and run a console version, that doesn't require
Lazarus (LCL), only pure FPC is needed:

  cd tests/
  ./compile_console.sh
  ./test_castle_game_engine -a

--
Michalis Kamburelis
