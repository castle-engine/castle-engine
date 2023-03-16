Castle Game Engine
==================

["Castle Game Engine"](https://castle-engine.io/) is a cross-platform (desktop, mobile, console) 3D and 2D game engine.

![Castle Game Engine - editor with terrain](https://castle-engine.io/images/not_resized_original/combined_cge_logo_game.png)

We feature:

* Powerful visual editor to design 3D and 2D games.

* A lot of components to design viewport contents (3D and 2D world, using scenes, cameras, navigation, primitives, lights...) and user inteface (buttons, images, labels...).

* Support for glTF, X3D, Spine and more formats.

* Composable shader effects, shadows, mirrors, physically based rendering, bump mapping, gamma correction...

* Fast clean code using modern Pascal.

* We are free and open-source.

See https://castle-engine.io/features for the complete list of engine features.

Installation and building your first application
---------

See the [Installation](https://castle-engine.io/install) manual page.

We recommend you download the engine from [our downloads](https://castle-engine.io/download), unpack the release and then run `bin/castle-editor` executable inside.

If you got the source code straight from our [GitHub repository](https://github.com/castle-engine/castle-engine/) then read first [Compiling from source](https://castle-engine.io/compiling_from_source.php).

Usage in short:

- Our [editor](https://castle-engine.io/manual_editor.php) is used to design and build your applications.

- Our [build tool](https://castle-engine.io/build_tool) is used to build your applications from the command-line.

    Both the build tool and editor use the project settings
    from the [CastleEngineManifest.xml](https://castle-engine.io/project_manifest)
    file.

- You can also use [Lazarus](https://www.lazarus-ide.org/).

    Make sure to register in Lazarus our packages. It's easiest to do this using the button _"Register Lazarus Packages"_ in CGE editor _"Preferences -> FPC and Lazarus"_ (see https://castle-engine.io/install ).

    You can install the `castle_components.lpk` package in Lazarus, to have LCL component `TCastleControl` (see https://castle-engine.io/control_on_form ).

- You can also use [Visual Studio Code](https://castle-engine.io/vscode). We feature a Pascal LSP server that can do code completion for Pascal and CGE API.

Documentation
-------

- [Manual](https://castle-engine.io/manual_intro.php) is the most recommended way to learn the engine.

- [API reference](https://castle-engine.io/apidoc/html/index.html).

    It is also available offline in the engine archive (if you downloaded the binary engine release), just open the file `doc/reference/index.html` in your WWW browser.

- Numerous examples are provided in the `examples/` subdirectory.

- [Guide to creating game data](https://castle-engine.io/creating_data_intro.php).

Support
-------

Questions? Talk to us on [forum or Discord chat](https://castle-engine.io/talk.php).

[Support us on Patreon](https://www.patreon.com/castleengine).

License
-------

The engine is available on the terms of LGPL >= 2 license with "static linking exception". This is the same license as used by FPC RTL and Lazarus LCL. In short, you *can* make commercial and closed-source games using the engine, you only have to share your modifications to the engine core.

See [license](https://castle-engine.io/license) for details.

Have fun!

Authors
-------

This is the life project of _Michalis Kamburelis_.

Thank you to [all the contributors and supporters](https://castle-engine.io/credits) for making the engine with me throughout the years. Keep it going please :)
