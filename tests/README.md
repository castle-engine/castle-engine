# Castle Tester

Using [Castle Game Engine](https://castle-engine.io/).

## Castle Game Engine testing framework

This application runs a number of automatic tests defined in `code/testcases/`.

It uses _Castle Game Engine_ testing framework, which is based on https://wiki.freepascal.org/fpcunit[FPCUnit] which is in turn inspired by DUnit, JUnit. Features above FPCUnit:

- We add some CGE-specific assertions (to compare vectors, matrices),
- We add a way to display test output using `TCastleWindow`,
- We work with both FPC and Delphi,
- The test application runs on all platforms we support (desktop, mobile, consoles, soon also web). So you can run tests e.g. on a real Android device.

## Building

Build it like a usual CGE project:

- From [CGE editor](https://castle-engine.io/manual_editor.php). Just use menu item _"Compile"_.

- Or use [CGE command-line build tool](https://github.com/castle-engine/castle-engine/wiki/Build-Tool). Run `castle-engine compile` in this directory.

- Or use [Lazarus](https://www.lazarus-ide.org/). Open in Lazarus `castle_tester_standalone_standalone.lpi` file and compile / run from Lazarus. Make sure to first register [CGE Lazarus packages](https://castle-engine.io/lazarus).

- Or use [Delphi](https://www.embarcadero.com/products/Delphi). Open in Delphi `castle_tester_standalone_standalone.dproj` file and compile / run from Delphi. See [CGE and Delphi](https://castle-engine.io/delphi) documentation for details.

## Running tests from GUI

By default the tester shows a window using `TCastleWindow`. Run by just running `castle-tester` binary. Press _"Start"_ button to run all the tests.

## Running tests from console (command-line)

Run with `--console` option to output to the console instead. Aside from changing the output, it also changes a bit how tests are executed: on a first uncaught exception, the tests stop, with a backtrace pointing to the exception. Like this:

```
castle-engine compile --mode=debug
./castle-tester --console
# or:
# castle-engine run -- --console
```

You can also run with `--no-window-create` to avoid making tests that require to create a new window, thus making it possible to run tests when no graphic environment is available. This is good for testing inside e.g. a server without GUI, or a Docker container.

```
./castle-tester --console --no-window-create
```

## Filtering

You can pass command-line parameter `--filter` to filter the initially enabled tests. Given filter can use wildcards (`*` and `?`) and is compared with the test name, which is the test case class name + test method name, separated by dot, like `TTestCastleClassUtils.TestRleCompression`. The comparison ignores case (since in Pascal identifiers, case is ignored). E.g.

- use `"--filter=TTestCastleClassUtils.*"` to run all tests from the `TTestCastleClassUtils` test case.
- use `--filter=TTestCastleClassUtils.TestRleCompression` to run exactly this particular test.
- use `"--filter=*image*"` to run all tests that have `image` in their name.

Note that it makes sense to add quotes (single or double) around the command-line option, to avoid shell from interpreting the `*` and `?` characters. Though you are unlikely to have any files matching the `--filter...`, so it should not be a problem. Unless you use space to separate `--filter` to option argument like `*image*`, then the quotes matter.

This filtering is applied to both console and GUI operation.
