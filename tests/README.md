# Castle Tester

Using [Castle Game Engine](https://castle-engine.io/).

## Building

Compile by:

- [CGE editor](https://castle-engine.io/manual_editor.php). Just use menu item _"Compile"_.

- Or use [CGE command-line build tool](https://github.com/castle-engine/castle-engine/wiki/Build-Tool). Run `castle-engine compile` in this directory.

- Or use [Lazarus](https://www.lazarus-ide.org/). Open in Lazarus `castle_tester_standalone.lpi` file and compile / run from Lazarus. Make sure to first register [CGE Lazarus packages](https://castle-engine.io/documentation.php).

## Using

By default castle tester shows window. Add --console to run in terminal. 

To test one test case use --suite=xx eg.:
`castle-tester --console --suite=TTestRectangles`

## fpcunit

Castle tester is compatibile with fpcunit so you can still use test_castle_game_engine.lpi but compile_console.sh now builds castle-tester.

Running all test in console:

`./test_castle_game_engine -a`

Running only CastleTransform tests:

`./test_castle_game_engine --suite=TTestCastleTransform`



