# Castle Game Engine build tool

Build, package Castle Game Engine projects from the command-line.

See the https://castle-engine.io/build_tool for documentation.

- This tool is used by the GUI editor, https://castle-engine.io/editor

- It is also useful in all environments when only command-line is available, e.g. in CI jobs (Jenkins, GitHub actions, GitLab CI -- see https://castle-engine.io/manual_automatic_builds.php ).

- You can call it from own editors (e.g. to integrate [VS Code](https://castle-engine.io/vscode) or Emacs with CGE).

- Or just use it directly from command-line, if you like to use CLI :)

## Building

To compile the build tool, all you need is an FPC installed.

- Unix:

    ```
    cd castle-engine/tools/build-tool/ # first enter the build tool directory
    ./castle-engine_compile.sh
    ```

- Windows (execute this in PowerShell):

    ```
    cd castle-engine/tools/build-tool/ # first enter the build tool directory
    Set-ExecutionPolicy Bypass -Scope Process
    ./castle-engine_compile.ps1
    ```

See https://castle-engine.io/compiling_from_source.php about compiling CGE.

You can also compile the build tool using standard methods for CGE applications:

- From [CGE editor](https://castle-engine.io/editor). Just use menu item _"Compile"_.

- Use [CGE command-line build tool](https://castle-engine.io/build_tool) to "bootstrap" it. Run `castle-engine compile` in this directory.

- Or use [Lazarus](https://www.lazarus-ide.org/). Open in Lazarus `castle-engine.lpi` file and compile / run from Lazarus. Make sure to first register [CGE Lazarus packages](https://castle-engine.io/lazarus).

## Delphi TODOs

The build tool cannot be compiled with Delphi for now.

It can only be compiled with FPC. Never the less, it can execute Delphi compiler to compile your projects (if instructed by `--compiler` option or `compiler` attribute in the [CastleEngineManifest.xml](https://castle-engine.io/project_manifest#_compiler_options_and_paths)).

To make the build tool compile with Delphi, we miss:

- `Process` unit, part of FPC, to run external programs with
  nice cross-platform API.

- Ability to enumerate *all* environment variables (not only query by name),
  like FPC `GetEnvironmentVariableCount` and `GetEnvironmentString(Integer)`.

- Understand the issue of `castleconf.inc` reporting problems with `CompilerVersion`
  when trying to compile the build tool with Delphi.
  In general `castleconf.inc` works with Delphi -- we include it in every CGE unit,
  but it fails when trying to compile the build tool.
  A temporary workaround is to comment out `{$if CompilerVersion < ...}`
  checks when compiling build tool with Delphi.


## Dev notes: CASTLE_STRICT_CLI symbol

The `CASTLE_STRICT_CLI` symbol is used when building the CGE for build tool. It means we know this is only a command-line application and we avoid linking any unit that may (indirectly) reference OpenGL units, sound or the big X3D/transform/scenes units.

- Major practical advantage is that it fixes build tool working in GitLab CI shared runner on Windows. See https://castle-engine.io/gitlab_ci https://gitlab.com/castle-engine/test-gitlab-ci/ .

    Such machines don't have OpenGL dynamic libraries, and GL unit initialization would fail.

    (Moreover it seems there are problems when X3DNodes is included in build tool on GitLab CI, build tool can fail without any error message. Never debugged it to the end, but it seemed *additional* issue on top of using GL. But it doesn't matter anymore, as this fix avoids using X3DNodes too.)

- Minor advantage: It is good that we're aware of how build tool is (accidentally) using OpenGL (or other unwanted) units. In general it is most elegant if it doesn't -- since build tool should never need to initialize OpenGL context. So all units used by build tool should be independent from OpenGL.

- Minor practical advantage: make build tool compilation even faster.
