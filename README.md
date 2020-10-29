Castle Game Engine
==================

["Castle Game Engine"](https://castle-engine.io/) is an open-source cross-platform 3D and 2D game engine.

We have many graphic features (physically-based rendering, shadows, mirrors, screen effects, gamma correction...) and support many data formats for game assets ([glTF, X3D, Spine JSON, Collada, ...](https://castle-engine.io/creating_data_model_formats.php)).
We have many user-interface components,
with a powerful viewport to display 3D or 2D content.
We're cross-platform (desktop, mobile, Nintendo Switch).

See https://castle-engine.io/features.php
for the complete list of engine features.

Documentation:

- The [Getting Started](https://castle-engine.io/documentation.php) page describes what to install and how to compile. See also the "Compiling" section below for a short version.

- [Manual](https://castle-engine.io/manual_intro.php) is the most recommended way to learn the engine.

- [API reference](https://castle-engine.io/apidoc-unstable/html/index.html).

  It is also available offline in the engine archive (if you downloaded the zip engine release), just open the file `doc/reference/index.html` in your WWW browser.

- Numerous examples are provided in the `examples/` subdirectory. For example take a look at a simple FPS game example in the `examples/fps_game/` subdirectory.

- [Guide to creating game data](https://castle-engine.io/creating_data_intro.php).

Questions? Talk to us on [forum or Discord chat](https://castle-engine.io/talk.php).

[Support us on Patreon](https://www.patreon.com/castleengine).

Compiling
---------

There are a couple of options. See the ["Getting Started" page](https://castle-engine.io/documentation.php) for details. In short:

- Use our Castle Game Engine Editor to design and build your applications.
  See https://castle-engine.io/documentation.php for a short introduction
  and https://castle-engine.io/manual_editor.php for details.

- Use our Castle Game Engine command-line build tool to build your applications.
  See https://github.com/castle-engine/castle-engine/wiki/Build-Tool .

    The build tool and editor use the project settings
    from the [CastleEngineManifest.xml](https://github.com/castle-engine/castle-engine/wiki/CastleEngineManifest.xml-examples)
    file.

- Use Lazarus ( https://www.lazarus-ide.org/ ).

    Install in Lazarus two packages:

    - packages/castle_base.lpk and
    - packages/castle_components.lpk

    Also, compile (but don't install) this package:

    - packages/castle_window.lpk

- Use FPC fpmake / fppkg.
  See https://github.com/castle-engine/castle-engine/wiki/FpMake

License
-------

The engine is available on the terms of LGPL >= 2 license
with "static linking exception". This is the same license
as used by FPC RTL and Lazarus LCL. In short, you *can* make
commercial and closed-source games using the engine,
you only have to share your modifications to the engine core.
See [COPYING.md](https://github.com/castle-engine/castle-engine/blob/master/COPYING.md) for details.

Have fun!

Authors
-------

This is the life project of _Michalis Kamburelis_.

Thank you to [all the contributors](https://github.com/castle-engine/castle-engine/graphs/contributors) for developing the engine with me throughout the years. Keep it going please :)
