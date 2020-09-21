# Castle Game Engine Editor

Allows to manage projects,
where a "project" is a directory containing `CastleEngineManifest.xml` file.

Note: [more concise description of the editor is also in the manual](https://castle-engine.io/manual_editor.php).

## Features

### Create, build, run projects

You can create a new project (from a number of templates) or open an existing one.

You can compile and run the project on various platforms,
using the [Castle Game Engine Build Tool](https://github.com/castle-engine/castle-engine/wiki/Build-Tool)
underneath (which in turn calls Pascal compiler, like FPC or Delphi, underneath).

The "project" is simply a directory containing a
[CastleEngineManifest.xml](https://github.com/castle-engine/castle-engine/wiki/CastleEngineManifest.xml-examples) file.
The CGE editor has deliberately the same "idea" for a project as our "build tool".
You can really open any
[CastleEngineManifest.xml](https://github.com/castle-engine/castle-engine/wiki/CastleEngineManifest.xml-examples)
(that you use with the current "build tool") with CGE editor.
In this sense, CGE editor may serve as just a GUI wrapper around our "build tool".

### Visual designer

You can visually design:

* a hierarchy of user-interface controls. Anything descending from `TCastleUserInterface`, like a button, label, or a powerful viewport (that contains a hierarchy of 3D / 2D scenes and transformations inside).

    Saved as `xxx.castle-user-interface` files. Load in your game using `UserInterfaceLoad` from `CastleComponentSerialize` unit.

* a hierachy of 3D / 2D scenes and transformations. Anything descending from `TCastleTransform`, so `TCastleTransform`, `TCastleScene` classes, that form a piece of 3D / 2D game world. You can add (using code) such hierarchy into an existing `TCastleViewport.Items`.

    Saved as `xxx.castle-transform` files. Load in your game using `TransformLoad` from `CastleComponentSerialize` unit.

The `xxx.castle-user-interface` and `xxx.castle-transform` are simple text files (JSON, using FPC FpJsonRtti). You should commit them to the version control, just like your source code. You can have as many such files inside your project as you need to. You load them from code using `CastleComponentSerialize` unit. You can instantiate them whenever you want, as many times as you want etc.

Let me emphasize that *when using the CGE editor, you still code using Pascal, using the same CGE API you already know (TCastleScene, TCastleUserInterface, TCastleWindow and so on)*. It's just that now, as an additional (optional) feature, you can load a designed instance of `TCastleUserInterface` or `TCastleTransform` using the `CastleComponentSerialize` unit. You can use this feature as much or as little as you want.

TODO: `TCastleEditor` component, discussed below and covering 3 use-cases, is not ready yet. Our current editor covers 1st use-case.

The visual editor is available as a component (`TCastleEditor`) that works in 3 use-cases:

1. It allows to design inside the "Castle Game Engine Editor" here.

2. It allows to design during the running game. This way you can inspect and even edit a live game!

    It is automatically compiled-in by the "Castle Game Engine Editor" if you enable "Live Designer" in the menu. The application is then compiled with LCL backend of TCastleWindow and the F12 key automatically shows the editor (in a separate window, maybe dockable or always-on-top?).

    This should provide an experience similar to running your game in game engines like Unity3d.

3. It allows to use the editor inside Lazarus IDE, to edit the contents of TCastleControl.

    This is quite like GLScene or FireMonkey experience — a RAD tool to edit your game right inside the environment you know and love.

### Include custom (project-specific) components in the visual designer

Larger projects may define custom components (descendants of the `TCastleUserInterface` or `TCastleTransform`). It is possible to include your custom components within the _Castle Game Engine Editor_, so that they can be used at design-time, just like standard CGE components. To do this:

1. In the initialization section of some unit (it may be the same unit where you define your custom component), register it.
    * Use unit `CastleComponentSerialize`.
    * In the `initialization` section add a call like this: `RegisterSerializableComponent(TMyButton, 'My Button');`

2. Inside your [CastleEngineManifest.xml](https://github.com/castle-engine/castle-engine/wiki/CastleEngineManifest.xml-examples), set the attribute `editor_units` to list all the units that call the mentioned `RegisterSerializableComponent`. It is a comma-separated list, like `editor_units="MyButtonUnit, MyMenuUnit"`.

3. Make sure:

    - Lazarus location is correctly set. You can set it in the editor "Preferences" window (or by adjusting `$PATH`). We need to execute `lazbuild` from Lazarus, to rebuild an editor with custom components.
    - Make sure the CGE location is correctly set. It should be detected automatically if you use the engine package (but you can always customize it using the environment variable `$CASTLE_ENGINE_PATH`).

4. Click menu item _"Project -> Restart Editor (With Custom Components)"_ in the editor (once you open a project).

    Alternatively, use the command-line [build tool](https://github.com/castle-engine/castle-engine/wiki/Build-Tool) command: `castle-engine editor`.

    Both ways will rebuild and run a customized version of the editor that includes your custom components.

    You can confirm you are running an editor with custom components by looking at the window title, it should include "_(With Custom Components)_".

### Open and run source code with external applications

You can open a text editor to edit source code (configurable; by default, we open Lazarus or Delphi, whichever is installed, since they offer advanced code completion for Pascal code).

We automatically set up project files such that you can run the game from Lazarus or Delphi (to use their built-in debugger). So, you can either compile/run from the CGE editor (which will use our build tool, that wraps Lazarus/Delphi) or you can compile/run from Lazarus or Delphi (for desktop platforms).

TODO: For now, we only work with Lazarus. Delphi support is planned.

### File browser

You can browse the application files. Our "Files Browser" just displays the files inside your project directory.

* It only omits some known unimportant things, like temporary `castle-engine-output` directory. But it displays everything else.

* Note that the `data/` subdirectory, that you will usually create in every non-trivial CGE project, is somewhat special. It is automatically detected (by it's name `data`), it is automatically packaged (e.g. in Android apk), and it is used by `ApplicationData` function or `castle-data:/xxx` URL (see https://castle-engine.io/manual_data_directory.php ). You will place there 3D models, 2D images and everything else you load in game.

    It is some equivalent to Unity3d `Assets/` subdirectory.

* Note that your Pascal source code should be outside the `data/` subdirectory. Actually, your source code can be anywhere within the project, we don't have any requirement here. You can put it in `code/` subdirectory, `src/` subdirectory, no subdirectory (top level), wherever you like. Just remember to list this subdirectory in `&lt;search_paths&gt;` in [CastleEngineManifest.xml](https://github.com/castle-engine/castle-engine/wiki/CastleEngineManifest.xml-examples#compiler-options-and-paths) file (for now, just edit this file in any text editor; in the future CGE editor can allow to edit it through a GUI dialog).

    If you really want, you can of course place source code in the `data/` subdirectory, but it usually doesn't make sense. Unless you really want to distribute to end-users your source code this way (but there are better ways to distribute source code, e.g. use _"Package Source"_).

    This is in contrast to Unity3d (that requires putting source code also inside `Assets/` directory).

* Clicking on various files runs a CGE tool suitable to preview/edit them:

    * On 3D and 2D models you can run view3dscene.
    * On images you can run castle-view-image.
    * On text files you can run a text editor (see above -- Lazarus or Delphi or anything else you configure).
    * TODO: On audio files, you can open them with `examples/audio/audio_player_scrubber/` (should this be moved to tools directory? probably!)
    * On other files, we can run the default OS application for them (`OpenDocument`)

* We also show interactive previews of models/images inside the CGE editor.
* TODO: Dragging files from the "File browser" onto the visual designer should automatically create the appropriate class instance.

    * TCastleScene to load a 3D model,
    * TCastle2DScene to load a Spine JSON model,
    * TCastleImageControl to show a 2D image.
    * This has some requirements (TCastleScene can only be inside a TCastleRootTransform, TCastleImageControl only inside UI hierarchy).

### Distributed in a binary form too

The editor is distributed as part of Castle Game Engine, also in binary form (for typical platforms -- Windows, Linux, macOS), for easy usage by everyone. This includes binaries (exe) to run:

- castle-editor
- castle-engine (our build tool)
- view3dscene
- castle-view-image
- other tools from castle-engine/tools/ directory
- TODO: In the future we may add external open-source tools to generate compressed textures, see https://castle-engine.io/creating_data_auto_generated_textures.php

The idea is that you only install FPC/Lazarus, then you run precompiled CGE editor and it all just works. Maybe in the future we could even bundle FPC/Lazarus with CGE editor, but this is not something I want to do initially (as packaging FPC/Lazarus is non-trivial, and I also would always want to have a version "unbundled" for people who prefer to install FPC/Lazarus themselves, or use Delphi).

## Documentation

You use modern Pascal language to code your games.
The Castle Game Engine is documented on https://castle-engine.io/ ,
in particular see our manual: https://castle-engine.io/manual_intro.php .

## License

The Castle Game Engine editor is provided on the GNU GPL >= 2 license terms.
See the file ../../COPYING.md for details.
In short:
You can use "Castle Game Engine" to create your own closed-source programs,
but you cannot fork "Castle Game Engine Editor" into a closed-source program.

When contributing (sending pull requests etc.) to the castle-editor source code,
you agree that your contributions may be used under either GPL
or a more permissive "LGPL with static linking exception" terms,
at the discretion of _Castle Game Engine Developers_.
_Castle Game Engine Developers_ are defined as _people with write (commit) access
to the official CGE version control repository_
(referred to from https://castle-engine.io/ , currently
https://github.com/castle-engine/castle-engine/ ).
The idea is that we sometimes want to move code from castle-editor to
the engine core, for technical reasons, and we want the freedom to do so.
Still, the editor stays GPL for the general public.

## Contributing

- Use desktop settings with 125% font scaling. Unfortunately, your personal desktop settings, at design-time, affect what is saved in LFM files, so it is best if we all use the same scaling, otherwise diffs to LFM files wildly change everything.

    You can set such scaling e.g. by GNOME 3 _"Large fonts"_ accessibilty option, or by adjusting Xorg dpi to 120 (96 * 1.25), Windows also allows to set 125% scaling.

### Contributing: When creating a new Lazarus form, remember to...

- Save form class `TFooForm` (so it will have singleton `FooForm`) in unit name `FormFoo`.
- Adjust form's `Caption`.
- Adjust `TabStop` of all the controls inside, to make it comfortable to use keyboard.
- Use `AutoSize` and anchoring on all controls, to work regardless of theme font size. Do not assume that a text will have the same size as you have designed --- people use various themes and font types. Lazarus applications have a native look, and are expected to adjust to user's theme preferences.
- Consider using `AutoSize` on the form itself too.
- Adjust `BorderStyle` from `bsSizeable` to `bsSingle` if it's a small form that doesn't need to be resized (for larger forms, it's safer to allow resizing, even if you think you know the best size -- in case user will view it on a smaller monitor).
- Adjust `Position` from "as designed" (usually "default" or "main form center" is more sensible).
- Make sure closing the form with "X" (Alt + F4) works OK.
- For a form you create manually, make sure it is freed at some point (preferably, not only at the end of application, if you can free it earlier; e.g. we don't want to have 100 of TProjectForm instances in memory after using the editor for a long time).
