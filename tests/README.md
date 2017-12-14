Automatic tests of Castle Game Engine.

Using [FPCUnit](http://camelos.sourceforge.net/fpcUnit.html).

Compile and run them from Lazarus to see a result in a nice GUI window.

Or compile and run a console version (this doesn't require Lazarus LCL):

```
cd tests/
./compile_console.sh
# runs all
./test_castle_game_engine -a
# runs only CastleTransform tests
./test_castle_game_engine --suite=TTestCastleTransform
```
