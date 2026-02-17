# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

_Developers_: if you plan to use AI (like Claude Code) with _Castle Game Engine_ codebase, or use it with your own applications using _Castle Game Engine_, we recommend our guidelines on https://castle-engine.io/ai . In short: _Use AI as a tool to be smarter / faster, not to replace your own critical thinking. Do not commit code you did not fully review and understand._

## Project Overview

Castle Game Engine (sometimes referred to as "CGE" or castle-engine) is a cross-platform 3D and 2D game engine. It is written in modern Object Pascal.

It supports multiple platforms:

- desktop (Windows, Linux, macOS, FreeBSD),
- mobile (Android, iOS),
- console (Nintendo Switch, though not in this open-source version; this repository only contains the open-source part of the engine),
- web (through WebAssembly using FPC + WebAssembly and JavaScript using Pas2js).

Our main webpage https://castle-engine.io/ .

- Our features are on https://castle-engine.io/features
- News on https://castle-engine.io/wp/

See [README.md](README.md) for more information.

## Build Commands

### Use Makefile to Build Everything (Tools + Editor)

```bash
make # Builds tools, registers Lazarus packages, builds editor
```

### Build Our "Build Tool" Directly

```bash
./tools/build-tool/castle-engine_compile.sh    # Unix/Bash
./tools/build-tool/castle-engine_compile.ps1   # Windows PowerShell
```

More information about building the engine on https://castle-engine.io/compiling_from_source .

### Build Our Editor Directly

```bash
cd tools/castle-editor/
lazbuild castle_editor.lpi
# executing "castle-engine compile" will also work, it will execute just "lazbuild ..." in this case, because the tools/castle-editor/CastleEngineManifest.xml contains build_using_lazbuild="true"
```

### Compile All Examples

```bash
make examples           # Compile using CGE build tool
make examples-laz       # Compile using Lazarus (lazbuild)
make examples-delphi    # Compile using Delphi
```

### Compile and run a Single Example

```bash
cd examples/3d_games/explore_impressive_castle/
castle-engine compile
castle-engine run
```

### Using the Build Tool (after building)

```bash
cd <any-project-directory-with-CastleEngineManifest.xml>/
castle-engine compile                     # Compile current project (in release mode by default)
castle-engine compile --mode=debug        # Compile in debug mode
castle-engine compile --mode=release      # Compile in release mode
castle-engine run                         # Run the project
castle-engine clean                       # Clean build artifacts
```

## Running Tests

```bash
make tests              # Full test suite (debug, release, various configs)
```

### More Manual Test Building and Execution

```bash
# Build and run tests
cd tests/
castle-engine compile
./castle-tester --console --no-window-create
# if running inside a terminal that is within GUI, you can omit --no-window-create, let it create GUI windows for testing

# Filter specific tests
./castle-tester --console "--filter=TTestCastleClassUtils.*"
./castle-tester --console "--filter=*image*"
```

More information about tests in [tests/README.md](tests/README.md).

### Build Tool Tests

These are automated tests specifically of our "build tool" (`castle-engine` binary, code in `tools/build-tool`):

```bash
cd tools/build-tool/tests/
castle-engine compile
castle-engine run -- --all
```

## Building for web and checking environment

We support building for web, which means that one can execute `castle-engine compile --target=web` and get a working web application.

Under the hood, this uses
- pas2js (small glue code)
- FPC with WebAssembly target (main application code + engine code).

You can check is the FPC compiler capable of making web builds available on `$PATH` by doing

```shell
fpc -Pwasm32 -TwasiP1 -iTP
```

The above should answer just `wasm32`, not with any error.

You can also just compile a "hello world" Pascal application with FPC. Create a file `hello_wasm.lpr` with the following content:

```pascal
begin
  Writeln('Hello from WebAssembly!');
end.
```

And execute this:

```shell
fpc -Twasip1 -Pwasm32 hello_wasm.lpr
```

If everything went well, you should get `hello_wasm.wasm` in the same directory.

If this works, then executing `castle-engine compile --target=web` should work and be capable of building web version for most of our tools and examples. Like `../castle-model-viewer-mobile` (see about "other repos" lower in this file).

See https://castle-engine.io/web for more information about the web target and its requirements.

## Source Architecture

### Core Source (`src/`)

| Directory | Purpose |
|-----------|---------|
| `base/` | Core utilities: vectors, matrices, colors, strings, time, logging, class utils |
| `base_rendering/` | Low-level rendering: shaders, textures, OpenGL context |
| `scene/` | 3D scene graph, TCastleScene rendering, X3D nodes, lighting, shadows |
| `scene/load/` | Model loaders: glTF, Spine, Collada, MD3, IFC |
| `ui/` | UI components: buttons, labels, viewports, dialogs |
| `window/` | Window management, input handling, platform backends |
| `images/` | Image loading/saving and manipulation |
| `audio/` | Sound system with OpenAL and FMOD backends |
| `fonts/` | Font rendering and text display |
| `transform/` | 3D transformations and scene hierarchy |
| `physics/` | Physics engine integration (Kraft) |
| `files/` | File I/O, URLs, data loading |
| `castlescript/` | Built-in scripting language |
| `lcl/` | Lazarus Component Library integration |
| `delphi/` | Delphi-specific code |

More information about units layout in https://castle-engine.io/units_map .

### Tools (`tools/`)

| Directory | Purpose |
|-----------|---------|
| `build-tool/` | Command-line build utility (invokes FPC/Delphi). More information in https://castle-engine.io/build_tool |
| `castle-editor/` | Visual editor (LCL-based). More information in https://castle-engine.io/editor |
| `castle-curves/` | Curve design tool (not much usage) |
| `image-to-pascal/` | Convert images to Pascal code (deprecated, we prefer to just put images in project's data now) |
| `texture-font-to-pascal/` | Convert fonts to Pascal code (deprecated, we prefer to just put fonts in project's data now) |

### Key Files

- `CastleEngineManifest.xml` - Project configuration file for build tool. Every tool, example, test has its own `CastleEngineManifest.xml` in its directory.
- `packages/lazarus/` - Lazarus package files
- `packages/delphi/` - Delphi package files

## Units paths and options passed to the compilers

`castle-fpc.cfg` is FPC compiler configuration, used only by `tools/build-tool/castle-engine_compile.sh` and `tools/build-tool/castle-engine_compile.ps1` to compile the build tool itself. This file is rarely used. See the `castle-fpc.cfg` comments for more explanation when it's used / when it's not used.

In most situations, when building projects like examples and most engine tools, the build tool (`castle-engine` binary) provides necessary command-line options and paths to compilers.

- The options are hardcoded in `tools/build-tool/code/toolcompile.pas`

- The paths are hardcoded as `EnginePaths` constant in `src/files/tools/castleinternaltools.pas`

## Coding Conventions

- **Indentation**: 2 spaces, no tabs

    The `begin` and `end` keywords for a given block start on the same column. This usually means that `begin` is on a line of it's own, not "glued" on the same line after `then` or `end` keywords. This is an example of correct indentation:

```pascal
if X > 1 then
begin
  Y := 2;
  Writeln('X is greater than 1');
end else
begin
  Y := 3;
  if Z > 4 then
    Writeln('Z is greater than 4')
  else
    Writeln('Z is not greater than 4');
end;
```

- **Mode**: Adjust to ObjFpc mode (not Delphi mode) for FPC but also use `{$ifndef FPC}` as necessary to provide Delphi alternative.
- Supports older FPC and Delphi versions, so e.g. we cannot use "inline variables" (`var X := 1;` inside `begin`...`end`).
- Keep all identifiers PascalCase. Even `String` and `Boolean` (although lots of existing code doesn't follow it and has `string` and `boolean`, but write `String` and `Boolean` in all new code). Even single-letter variables like `I` for loop iteration follow this rule.
- **Documentation**: PasDoc comments before each public identifier (see "PasDoc Documentation Style" below)
- **Backward Compatibility**: Use `deprecated` to maintain old APIs
- **Warnings**: Fix all compiler warnings
- **Never use**: `with` statement, assembler
- **Optimizations**: Prefer high-level optimizations (GPU work) over low-level micro-optimizations

More information about coding conventions in [CONTRIBUTING.md](CONTRIBUTING.md). Even more in https://castle-engine.io/coding_conventions .

### Unit File Structure

Every CGE unit **must** include `{$I castleconf.inc}` right after the unit declaration. This is the central configuration include file defining compiler modes, platform detection, and feature flags.

Unit names are PascalCase prefixed with `Castle` (e.g., `CastleVectors`). The filename is the lowercase version of the unit name (e.g., `castlevectors.pas`).

Standard unit structure:

```pascal
{
  Copyright 20XX-20XX Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Short unit description. }
unit CastleMyUnit;

{$I castleconf.inc}

interface

uses CastleVectors, CastleUtils;

// ...

implementation

// ...

end.
```

### Include File Pattern

Large units are often split across include (`.inc`) files using `read_interface` / `read_implementation` defines:

```pascal
interface

{$define read_interface}
{$I castlefoo_bar.inc}
{$undef read_interface}

implementation

{$define read_implementation}
{$I castlefoo_bar.inc}
{$undef read_implementation}
```

### PasDoc Documentation Style

We use [PasDoc](https://pasdoc.github.io/) with the `--auto-abstract` option, so the first sentence is critical. Guidelines:

- **Do not start with "This..."** — just describe what it does. Write `Calculate the sensor value.`, not `This function calculates the sensor value.`
- **Do not start with "Simple..." or "Trivial..."**.
- **Always document units** (important for PasDoc's unit list).

### Writing Tests

Tests use `CastleTester` framework (based on FPCUnit, compatible with FPC and Delphi). Test files go in `tests/code/testcases/` and follow this pattern:

```pascal
unit TestCastleMyUnit;

{$I ../../../src/common_includes/castleconf.inc}

interface

uses
  Classes, SysUtils,
  CastleTester, CastleMyUnit;

type
  TTestCastleMyUnit = class(TCastleTestCase)
  published
    procedure TestSomething;
  end;

implementation

procedure TTestCastleMyUnit.TestSomething;
begin
  AssertEquals(42, MyFunction);
end;

initialization
  RegisterTest(TTestCastleMyUnit);
end.
```

Run specific tests with `./castle-tester --console "--filter=TTestCastleMyUnit.*"`.

## CI/CD

GitHub Actions workflows are in `.github/workflows/`. Key ones include compilation tests across platforms, Delphi builds, API reference generation, and example verification. Ensure all CI checks pass on pull requests.

## Compiler Support

- **FPC** (Free Pascal Compiler) - Primary compiler, compiles all the engine examples and tools.
- **Delphi** - Supported. Compiles all the engine examples and some tools. The editor is an LCL application, and it cannot be compiled with Delphi.
- **Lazarus IDE** - IDE we are compatible with. Lazarus is an IDE (editor, debugger, and form designer for the LCL components) not a compiler. It is calling FPC compiler under the hood. People sometimes say they "compile with Lazarus" -- they actually mean that they use Lazarus to compile, but strictly speaking they "compile" with FPC, since Lazarus IDE just invokes FPC. Sometimes we write "Lazarus/FPC" to mean "compile with FPC, and you can use Lazarus IDE to edit the code".

## Required tools to run some shell scripts and CI/CD workflows (Platform-Specific)

Some scripts require:

- **macOS**: Requires `gsed` and `ginstall` from Homebrew (`brew install gnu-sed coreutils`)
- **Windows**: Requires Cygwin with `make` package (Cygwin must be before FPC/Delphi in PATH, to not use Embarcadero `make` (which is not GNU make) or `make` from FPC (which is old))
- **FreeBSD**: Use `gmake` instead of `make`

## Important Compiler Symbols

Defined in `castleconf.inc` or by the build tool — useful when writing platform-specific code:

- `FPC` / `DELPHI` - Compiler detection
- `MSWINDOWS`, `UNIX`, `LINUX`, `DARWIN` - OS detection
- `ANDROID`, `CASTLE_IOS` - Mobile platform detection
- `OpenGLES` - Use OpenGL ES instead of desktop OpenGL
- `CASTLE_NINTENDO_SWITCH` - Nintendo Switch platform

Use `{$ifdef FPC}` / `{$ifndef FPC}` for compiler-specific code. Use `{$ifdef ANDROID}`, `{$ifdef CASTLE_IOS}`, etc. for platform-specific code.

## Project Structure for CGE Applications

Every CGE project should have:
- `CastleEngineManifest.xml` - Project manifest defining name, dependencies, build settings
- `data/` directory - Game assets (models, textures, sounds). Unless it's some utility without data, but this is an exception. All normal applications and games have `data/` directory.
- Main `.dpr`/`.lpr` program file
- `code/` directory - Source code (by convention)
- `castleautogenerated.pas` - Auto-generated by the build tool; do not manually edit. Regenerate with `castle-engine generate-program`.

## Other repos

Note that this repository contains the engine sources, examples, tools. This is the engine (source code, examples, necessary tools - editor, build tool called `castle-engine`).

In a typical developer environment, there are other repositories as siblings to this, like [../castle-model-viewer](../castle-model-viewer), which provide additional code related to the engine. In particular:

### ../castle-model-viewer with GUI model viewer and command-line model converter and validator

[../castle-model-viewer](../castle-model-viewer) is a "Castle Model Viewer" tool, our 3D and 2D model viewer (only for desktops), that utilizes Castle Game Engine.

It also features command-line "Castle Model Converter", a command-line tool to convert models between many formats. Building and running this tool is a nice test for various model loading / saving work around the engine, an example:

```
cd ../castle-model-viewer/
castle-engine compile --manifest-name=CastleEngineManifest.converter.xml
./castle-model-converter input.x3d output.gltf
```

### ../castle-model-viewer-mobile with GUI model viewer for everything (mobile, desktop, web)

[../castle-model-viewer-mobile](../castle-model-viewer-mobile) is a variation of "Castle Model Viewer" tool that is absolutely portable, works on all platforms supported by the engine. It started as a mobile version, but it's fully functional on desktop and web as well.

It's a good test for building for web or mobile, because it's a non-trivial application that uses many engine features and builds for web or mobile. Use commands like `castle-engine compile --target=web` (see above "Building for web and checking environment" section) or `castle-engine compile --target=android` in this directory.

### ../cge-www with website documentation

[../cge-www](../cge-www) is the source code of our website https://castle-engine.io/ . It contains additional documentation (mostly in AsciiDoctor format in `htdocs/doc/`), like a manual. (In contrast, detailed API docs are available as comments in the source code itself, in this repository `castle-engine`, and are processed using PasDoc.)

## Origin of this CLAUDE.md file

Initial version of this file was generated by Claude Code (at Opus 4.5), then it was refined manually to be optimal (using build tool, running auto-tests, web target, other repos as siblings; various things that, when testing agent work, were not clear to Claude initially). Then we iterated more, Claude and Michalis.