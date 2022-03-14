# Castle Tester

Using [Castle Game Engine](https://castle-engine.io/).

This application runs a number of automatic tests defined in `code/testcases/`.

## Default testing framework (Castle) - GUI

By default the tester shows a window using `TCastleWindow`. Build like a usual CGE project:

- From [CGE editor](https://castle-engine.io/manual_editor.php). Just use menu item _"Compile"_.

- Or use [CGE command-line build tool](https://github.com/castle-engine/castle-engine/wiki/Build-Tool). Run `castle-engine compile` in this directory.

- Or use [Lazarus](https://www.lazarus-ide.org/). Open in Lazarus `castle_tester_standalone.lpi` file and compile / run from Lazarus. Make sure to first register [CGE Lazarus packages](https://castle-engine.io/documentation.php).

Run by just running `castle-tester` binary. Press "Start" button to run all tests.

## Default testing framework (Castle) - console

Build like above, but add `--console` to use a console output instead.

```
castle-engine compile --mode=debug
./castle-tester --console
# or:
# castle-engine run -- --console
```

You can also recompile with `NO_WINDOW_SYSTEM` to avoid making tests that require any graphic environment. This is good for testing inside e.g. a server without GUI, or a Docker container.

```
castle-engine clean # make sure to recompile everything
castle-engine compile --mode=debug --compiler-option=-dNO_WINDOW_SYSTEM
castle-engine run -- --console
```

To limit testing to just one test case use `--suite=xx` e.g.:

```
./castle-tester --console --suite=TTestRectangles
```

## Alternative testing framework (FpcUnit) - console

We also maintain a version of the tester using FPC's FpcUnit. Compile it with console UI (passes `TEXT_RUNNER` define) like this:

```
castle-engine clean # clean compilation with default CGE testing framework
castle-engine compile --manifest-name=CastleEngineManifest.xml.fpcunit
```

From the command-line run all the tests like this:

```
./test_castle_game_engine -a
```

Running specific tests like this:

```
./test_castle_game_engine --suite=TTestCastleTransform
```

## Alternative testing framework (FpcUnit) - GUI

You can also open in Lazarus and compile + run `test_castle_game_engine.lpi` project. Or build it using command-line

```
lazbuild test_castle_game_engine.lpi
```

This produces a binary `test_castle_game_engine` using LCL UI. Run it and click appropriate button to run all tests.
