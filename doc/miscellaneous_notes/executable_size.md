# Executable size

The size of the compiled executable is by default a bit large. Here are some pointers how to reduce it.

Note: Tests below focus on _Linux/x86_64_ and FPC 3.2.2, but conclusions should apply to all platforms and compilers.

Note: Tests on Linux, done below, assume you are using the latest _Castle Game Engine_, with [this commit](https://github.com/castle-engine/castle-engine/commit/95f0d9d791aa117dc4ea65af30b4d2a8fc45a21a) included. (It will soon be part of [downloads](https://castle-engine.io/download).) This commit allows to reduce the size of the executable (only on Linux) more, so if you care about executable size and use Linux -> please use the latest engine version, and regenerate your program file by _"Code -> Regenerate Project"_ menu item (in editor) or `castle-engine generate-program` (from command-line).

## Build in release mode to judge size and speed

Don't judge the size (or [speed](https://castle-engine.io/optimization)) of the executable compiled in a _debug mode_. Compile in _release mode_ for measurements. In the editor, choose _"Run -> Release Mode"_. On the command-line, release mode is the default now, but to be sure you can pass `--mode=release`.

We tested on Linux/x86_64 with FPC 3.2.2 with the [play_animation](https://github.com/castle-engine/castle-engine/tree/master/examples/animations/play_animation) example. This example uses `TCastleScene`, `TCastleWindow`, pulling most of the engine dependencies used in a typical game. Switching to release mode reduces the exe size from 129 MB to 23 MB.

Note that this 23 MB executable is compressed when placed in a zip / gzip file, and you usually provide your game to users in a compressed form. (Our _"Run -> Package (Release to Users)"_ makes a ready zip / tar.gz to distribute.) Default gzip compression makes this 23 MB drop to 5.3 MB. That's a reasonable size for a game executable :)

Command-line test:

```
$ cd examples/animations/play_animation/

$ castle-engine compile --mode=debug
Compiling project "play_animation" for OS / CPU "linux / x86_64" in mode "debug".
...
$ ls -lh play_animation
... 129M ... play_animation

$ castle-engine clean
$ castle-engine compile --mode=release
Compiling project "play_animation" for OS / CPU "linux / x86_64" in mode "release".
....

$ ls -lh play_animation
... 23M ... play_animation

$ gzip play_animation
$ ls -lh play_animation.gz
... 5.3M ... play_animation.gz
# Note: with gzip -9 compression, it is 5.2 MB
```

## Disable unused formats

The engine supports a number of [formats](https://castle-engine.io/model_formats) for 3D and 2D models.

They contribute to the executable size, and you likely don't need all of them.

In particular, [IFC](https://castle-engine.io/ifc) format is often not useful for game models, but it defines a lot of classes and thus contributes to the exe size. TODO: In the future it may be excluded by default, for now you can disable it manually.

You can disable support for formats by defining symbols like `CASTLE_xxx_SUPPORT_DISABLE` when building the application. There are a few ways to define these symbols, we recommend to [add <define> elements to the CastleEngineManifest.xml](https://castle-engine.io/project_manifest#_compiler_options_and_paths). In effect, the editor and build tool will define these symbols when compiling your game. (And if you use _"Code -> Regenerate Project"_ or `castle-engine generate-program`, then we will also update symbols in LPI and DPROJ files, so they will also be used when building from Lazarus IDE or Delphi.)

These are symbols you can define, roughly in order from _likely to have the most impact on size_:

- `CASTLE_IFC_SUPPORT_DISABLE`
- `CASTLE_GLTF_SUPPORT_DISABLE`
- `CASTLE_SPINE_SUPPORT_DISABLE`
- `CASTLE_TILED_MAP_SUPPORT_DISABLE`
- `CASTLE_COLLADA_SUPPORT_DISABLE`
- `CASTLE_MD3_SUPPORT_DISABLE`
- and a few more, see [list of all CASTLE_xxx_SUPPORT_DISABLE symbols in x3dload.pas](https://github.com/castle-engine/castle-engine/blob/master/src/scene/load/x3dload.pas#L67), but practically speaking disabling most other formats will not have a measurable impact on size.

Testing again on the [play_animation](https://github.com/castle-engine/castle-engine/tree/master/examples/animations/play_animation) example, we disabled 4 formats by adding this to the [CastleEngineManifest.xml, inside <compiler_options>](https://github.com/castle-engine/castle-engine/blob/master/examples/animations/play_animation/CastleEngineManifest.xml):

```xml
<defines>
  <define>CASTLE_IFC_SUPPORT_DISABLE</define>
  <define>CASTLE_TILED_MAP_SUPPORT_DISABLE</define>
  <define>CASTLE_COLLADA_SUPPORT_DISABLE</define>
  <define>CASTLE_MD3_SUPPORT_DISABLE</define>
</defines>
```

- Debug build size went down (from 129 MB) to 103 MB.
- Release build size went down (from 23 MB) to 19 MB.
- Gzip-compressed release build went down (from 5.3 MB) to 4.7 MB.

## Use `-XX` option (smartlinking) to reduce size further

Pass `-XX` to FPC to enable smartlinking. This removes unused code from the executable, and can reduce the size of the executables even more.

TODO: In the future, we may do this automatically, at least in _release_ mode. We just need to test it doesn't break anything on all our supported platforms.

For now, you can add `-XX` to the `<custom_options>` in [your `CastleEngineManifest.xml`](https://castle-engine.io/project_manifest#_compiler_options_and_paths).

Making a final test on the [play_animation](https://github.com/castle-engine/castle-engine/tree/master/examples/animations/play_animation) example, with `-XX` enabled and 4 unused formats disabled, so that `CastleEngineManifest.xml` looks like this:

```xml
<?xml version="1.0" encoding="utf-8"?>

<!--
Specify basic information about the project, including how this project is build.
See https://castle-engine.io/project_manifest for documentation of this file.
-->

<project name="play_animation"
  standalone_source="play_animation_standalone.dpr"
  game_units="GameInitialize"
  qualified_name="io.castleengine.play.animation"
  caption="Play Animation"
>
  <compiler_options>
    <custom_options>
      <option>-XX</option>
    </custom_options>
    <search_paths>
      <path value="code/" />
    </search_paths>
    <defines>
      <define>CASTLE_IFC_SUPPORT_DISABLE</define>
      <define>CASTLE_TILED_MAP_SUPPORT_DISABLE</define>
      <define>CASTLE_COLLADA_SUPPORT_DISABLE</define>
      <define>CASTLE_MD3_SUPPORT_DISABLE</define>
    </defines>
  </compiler_options>
</project>
```

Results:
- Debug build size: doesn't change in this test, as [DWARF debug information cannot be used with smart linking](https://www.freepascal.org/docs-html/3.2.2/user/userse70.html), so `-XX` has no effect.
- Release build size went down (from initial 23 MB) to 11 MB.
- Gzip-compressed release build went down (from initial 5.3 MB) to 2.9 MB.

