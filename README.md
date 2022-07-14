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

- [Manual](https://castle-engine.io/manual_intro.php) is the most recommended way to learn the engine.

- [API reference](https://castle-engine.io/apidoc/html/index.html).

    It is also available offline in the engine archive (if you downloaded the binary engine release), just open the file `doc/reference/index.html` in your WWW browser.

- Numerous examples are provided in the `examples/` subdirectory.

- [Guide to creating game data](https://castle-engine.io/creating_data_intro.php).

Questions? Talk to us on [forum or Discord chat](https://castle-engine.io/talk.php).

[Support us on Patreon](https://www.patreon.com/castleengine).

Installation and building your first application
---------

See the [Installation and building your first application](https://castle-engine.io/manual_install_run_first.php) manual page.

If you got this source code straight from our [GitHub repository](https://github.com/castle-engine/castle-engine/) then read first [Compiling from source](https://castle-engine.io/compiling_from_source.php).

In short:

- Our [editor](https://castle-engine.io/manual_editor.php) is used to design and build your applications.

- Our [build tool](https://castle-engine.io/build_tool) is used to build your applications from the command-line.

    Both the build tool and editor use the project settings
    from the [CastleEngineManifest.xml](https://castle-engine.io/project_manifest)
    file.

- You can also use Lazarus ( https://www.lazarus-ide.org/ ).

    Install in Lazarus two packages:

    - packages/castle_base.lpk and
    - packages/castle_components.lpk

    Also, compile (but don't install) this package:

    - packages/castle_window.lpk

- You can also use FPC fpmake / fppkg.
  See https://castle-engine.io/fpmake

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
