# Castle Game Engine Editor

Allows to manage projects,
where a "project" is a directory containing `CastleEngineManifest.xml` file.

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

* a hierarchy of user-interface controls. Anything descending from `TCastleUserInterface`, like a button, label, or a powerful scene manager (that contains a hierarchy of 3D / 2D scenes and transformations inside).

    Saved as `xxx.castle-user-interface` files. Load in your game using `UserInterfaceLoad` from `CastleComponentSerialize` unit.

* a hierachy of 3D / 2D scenes and transformations. Anything descending from `TCastleTransform`, so `TCastleTransform`, `TCastleScene`, `TCastle2DScene` classes, that form a piece of 3D / 2D game world. You can add (using code) such hierarchy into an existing `TCastleSceneManager` world.

    Saved as `xxx.castle-transform` files. Load in your game using `TransformLoad` from `CastleComponentSerialize` unit.

The `xxx.castle-user-interface` and `xxx.castle-transform` are simple text files (JSON, using FPC FpJsonRtti). You should commit them to the version control, just like your source code. You can have as many such files inside your project as you need to. You load them from code using `CastleComponentSerialize` unit. You can instantiate them whenever you want, as many times as you want etc.

Let me emphasize that *when using the CGE editor, you still code using Pascal, using the same CGE API you already know (TCastleScene, TCastleUserInterface, TCastleWindow and so on)*. It's just that now, as an additional (optional) feature, you can load a designed instance of `TCastleUserInterface` or `TCastleTransform` using the `CastleComponentSerialize` unit. You can use this feature as much or as little as you want.

The visual editor is available as a component (`TCastleEditor`) that works in 3 use-cases:

1. It allows to design inside the "Castle Game Engine Editor" here.

2. It allows to design during the running game. This way you can inspect and even edit a live game!

    It is automatically compiled-in by the "Castle Game Engine Editor" if you enable "Live Designer" in the menu. The application is then compiled with LCL backend of TCastleWindow and the F12 key automatically shows the editor (in a separate window, maybe dockable or always-on-top?).

    This should provide an experience similar to running your game in game engines like Unity3d.

3. It allows to use the editor inside Lazarus IDE, to edit the contents of TCastleControl.

    This is quite like GLScene or FireMonkey experience — a RAD tool to edit your game right inside the environment you know and love.

### Open and run source code with external applications

You can open a text editor to edit source code (configurable; by default, we open Lazarus or Delphi, whichever is installed, since they offer advanced code completion for Pascal code).

We automatically set up project files such that you can run the game from Lazarus or Delphi (to use their built-in debugger). So, you can either compile/run from the CGE editor (which will use our build tool, that wraps Lazarus/Delphi) or you can compile/run from Lazarus or Delphi (for desktop platforms).

### File browser

You can browse the application files. Our "Files Browser" just displays the files inside your project directory.

* It only omits some known unimportant things, like temporary `castle-engine-output` directory. But it displays everything else.

* Note that the `data/` subdirectory, that you will usually create in every non-trivial CGE project, is somewhat special. It is automatically detected (by it's name `data`), it automatically packaged (e.g. in Android apk), and it is used by `ApplicationData` function. You will place there 3D models, 2D images and everything else you load in game.

    It is some equivalent to Unity3d `Assets/` subdirectory.

* Note that your Pascal source code should be outside the `data/` subdirectory. Actually, your source code can be anywhere within the project, we don't have any requirement here. You can put it in `code/` subdirectory, `src/` subdirectory, no subdirectory (top level), wherever you like. If you really want, you can also place it in `data/` subdirectory, but it usually doesn't make sense (unless you really want to distribute to end-users your source code this way).

    This is in contrast to Unity3d (that requires putting source code also inside `Assets/` directory).

* On various 3D and 2D assets you can run view3dscene. On 2D images you can run glViewImage. On text files you can run a text editor (see above). We also want to auto-generate and show a quick previews of them inside the CGE editor.

### Distributed in a binary form too

The editor is distributed as part of Castle Game Engine, also in binary form (for typical platforms -- Windows, Linux, macOS), for easy usage by everyone. This includes binaries (exe) to run:

- castle-editor
- castle-engine (our build tool)
- view3dscene
- glViewImage
- other tools from castle-engine/tools/ directory
- maybe external open-source tools to generate compressed textures, see https://castle-engine.io/creating_data_auto_generated_textures.php

The idea is that you only install FPC/Lazarus, then you run precompiled CGE editor and it all just works. Maybe in the future we could even bundle FPC/Lazarus with CGE editor, but this is not something I want to do initially (as packaging FPC/Lazarus is non-trivial, and I also would always want to have a version "unbundled" for people who prefer to install FPC/Lazarus themselves, or use Delphi).

## Documentation

You use modern Pascal language to code your games.
The Castle Game Engine is documented on https://castle-engine.io/ ,
in particular see our manual: https://castle-engine.io/manual_intro.php .
Note that it's not yet updated to describe this visual editor.

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

## TODO

Now:
* Visual inspector. designer etc.
    * Component wrapper will also need info about what is default, to know what to save...
      to json,
      and for lfm -- se TCastleColorPersistent.DefineProperties todo
    * UI of designer needs to be improved to clearly communicate what is happening:
	* File->Open should by default show our project directory,
	  and warn when opening/saving xx.castle-xxx outside of our project
	  ("You are saving or loading a hierarchy (.castle-user-interface or .castle-transform file) outside of the project "data" subdirectory. This is possible, but for typical games not adviced.

	  Typical games should keep all the designs (.castle-user-interface and .castle-transform files) inside the "data" subdirectory to have them packaged in the application on all platforms (desktop, mobile). And you should always load them using the ApplicationData function or the castle-data:/xxx URL.
	  )
	* before exiting ask whether to save.
	* special menu "Designer" with new, open, close. Separate from "File"
	* rest of "File" rename to "Project"
	* initially nothing should be open,
	  and top part of editor should show
	  "Open or create a new
	   - user interface (.castle-user-interface file) or
	   - transformation (.castle-transform file)
	   using the "Designer" menu
	* Designer -> Close (Ctrl + W) menu
    * open last scene in the project,
      open the only scene in the project, if only one exists?
    * does recursive saving work when Tcastletransform is present multiple times in graph?
    * (in-progress) Allow editing at least most important properties:
        * (done) Name
        * (done) TCastleScene.URL
	* (but fix URL to set castle-data:)
	* initial animation? along with TimePlayingSpeed, ProcessEvents
        * TCastleTransform position, rotation, scale (using gizmos)
        * TCastleUserInterface anchors (self, parent -- together in simple ver, as 3x3 grid) and (using gizmo) delta to anchor
    * save also vectors, colors.
      Like position, rotation, scale as TVector3 properties should be fixed --- need to expose them as published, see TODOs, probably.
      And colors, like
        RectangleGroup.Color := Vector4(0.5, 0.5, 1, 0.2); // transparent light-blue

      See /home/michalis/common/TODO/castle-engine/editor/castlevectors_components.inc
      and /home/michalis/common/TODO/castle-engine/editor/cge-editor-older-notes-published-vectors.txt
      Make a minimal test of this, with Delphi and FPC/Lazarus.
      Possibly we can publish records now?

    * Store only non-default with stored=true.
      https://stackoverflow.com/questions/30352756/delphi-how-to-get-default-value-for-property-using-rtti
      http://docwiki.embarcadero.com/Libraries/Berlin/en/System.TypInfo.TPropInfo
      See how normal TWriter does it, using TypInfo.

      yes, we can copy from
      /home/michalis/installed/fpclazarus/3.0.4/fpcsrc/rtl/objpas/classes/writer.inc

      cleanup then data/project_templates/empty/files/data/main.castle-user-interface
      to not contain defaults

    * Allow adding new, deleting, moving around
    * UI controls improvements:
	* more should descend from TCastleUserInterfaceRect, e.g. TCastleButton/Label//ImageControl too.
	  At TCastleUserInterfaceRect document:

	    Some descendants support auto-sizing, which means that the control's size
	    set by these properties is ignored, and instead the calculated size
	    (CalculatedWidth, CalculatedHeight, CalculatedRect) depends on some core
	    values of the control. E.g. TCastleImageControl adjusts to image,
	    TCastleLabel adjusts to caption, TCastleButton adjusts to button and icon,
	    TCastleVerticalGroup adjusts to all children and so on.
	    Consult the documentation of each descendant for the exact
	    specification of behavior, usually a property called @code(AutoSize) controls it. }

	* virtual functions to say VerticalResizingEffective
	  (at TCastleUserInterface or only TCastleUserInterfaceRect)
    * remove _Children once processed
    * ask before closing project when HierarchyModified - save? cancel?
    * ask before overriding saved file
    * mark Width, Height as stored=false when FloatWidth, FloatHeight available
    * force non-empty Name on all, to have wokring streaming?
    * show checkerboard instead of Background.Color := Vector4(0.5, 0.5, 0.5, 1);, to make it clear it's undefind
    * MainScene cannot be changed
      (we disabled in object inspector some types, maybe we should not?)
      Is it deserialized OK? Unsure, as headlight with hlMainScene doesn't shine
    * publish and save SceneManager.NavigationType
      and last camera
      { Use initial camera settings stored in
        InitialCameraPosition,
	InitialCameraDirection,
	InitialCameraUp
	values. They are used if you create a camera using one of the
	TCastleAbstractViewport methods, like RequiredCamera or WalkCamera
	or ExamineCamera. They will not be used if you assign to @link(Camera)
	your own camera instance. }
      StoreInitialCamera: Boolean
      InitialCamera

    * TCastleButton:
      - Pressed must be editable even when not toggle?
	or maybe not published
      - EnableParentDragging should not be published (and should be read-only?)
      - we need a way to adjust various images of tcastlebutton
        See /home/michalis/common/TODO/castle-engine/editor/castleimages_components.inc
      	Also special descendant for 3x3 images, with corners property (or maybe it should always have 3x3 information?)

    * castle-data:/ finish
      - support castlefindfiles.pas too
      - document at ApplicationData
        See /home/michalis/common/TODO/castle-engine/editor/castle-data-url.txt

    * unpublish KeepInFront, since switching it at runtime is not supported
    * unpublish HeadlightFromViewport, since unsure (deprecated even, or planned to be deprecated?)
    * TLabel.Text setting is ignored
      (we should react to Text.Assign maybe?)
    * saving TCastleColorPersistent to LFM for now doesn't work?

+  // TODO: Why these are necessary to expand in castle-editor,
+  // but in Lazarus at least TCastleVector3Persistent in test project was
+  // expanded without this?
+  RegisterPropertyEditor(TypeInfo(TCastleColorPersistent), nil, '',
+    TSubPropertiesEditor);
+  RegisterPropertyEditor(TypeInfo(TCastleColorRGBPersistent), nil, '',
+    TSubPropertiesEditor);
+  RegisterPropertyEditor(TypeInfo(TCastleVector3Persistent), nil, '',
+    TSubPropertiesEditor);
+  RegisterPropertyEditor(TypeInfo(TCastleVector4Persistent), nil, '',
+    TSubPropertiesEditor);

    * opening other data (like 3d models) should also default to data dir,
      and warn if opening outside.

Lower priority:
* ugly button in example? new ui for internal controls?
* Files browser as above
* templates:
    * Create other than "empty" project templates
    * Proper screenshots of all project templates
    * Templates should load by default the visually-designed world. The goal: you should be able to modify and run the game in the editor without writing code.
    * Some (or all?) templates should show using TUIState. This is our ultimate flexible architecture to develop “pure games” applications (where OpenGL context is your only user-interface): TCastleWindow with a number of TUIState instances using TCastleUserInterface inside.
* build tool integration:
    * when running, provide CGE libs on path for Windows? Should this maybe be done by build tool, actually?
    * For "run", colorized CastleLog warnings
    * For "compile", colorize FPC warnings, errors
    * Shorter compile output:
        * lines "compiling..", "writing resource string table...", are displayed, but then replaced by a next AddLine. This way they serve as "progress indicator" but do not eat so much output space.
        * also do not show FPC "logo", do not repeat information about FPC version, Os/CPU 2 times, debug mode,...
        * remove the "separator" lines. The bold lines already separate them nicely?
        * "command finished with status 0" -> "Command finished successfully."
    * show count of warnings/errors if non-zero on tab header, allow to filter by them
    * Smartly detect CASTLE_ENGINE_PATH, and set it for subprocesses, see Michalis ~/common/TODO
    * Smartly detect castl-engine exe (look in CASTLE_ENGINE_PATH/bin etc.), see Michalis ~/common/TODO
    * Detect lack of FPC / Delphi and make a nice error message
    * Allow to choose platform
    * rerun generate-program each time? (mark them as some *DO NOT MODIFY THIS, THIS IS ONLY FOR LAZARUS* comment)
        Not really OK, in case we open program with hand-crafted program file.
        Maybe only auto-generate in castle-engine-output,
          before opening Lazarus,
          if lpr not yet present?
        Best: change to use planned CastleEngineConfig.pas unit,
          that is always auto-generated without warning.
          Do not overwrite lpr each time.
    * checkbox in menu for verbose output from the build tool
    * use machine-readble format format for communication with build tool and CastleLog when CASTLE_ENGINE_EDITOR_INSIDE=true

        Causes build tool some lines (e.g. in verbose fpc command line) to use special format, and actual program uses CastleLog that has special output (and always goes to console even on Windows)
        - Avoid xml tags here (would require quoting rest).
        - Just tags like Cge-output, bytes=xxx:
        - Cge-output,multiline,...
        - Cge-output, warning,...
        - bytes are always required and allow reliably waiting reading up to message end, without the need to quote/unquote it

    * Detect multilibe logs and show as one list item in output that can be expanded,, only category initially visible. E.g. useful for
      - "OpenGL Information" or
      - command-line of FPC in build tool verbose mode

* small GUI stuff:
    * Show on recent list %20 as spaces, use URICaption or such ready function?
    * on NewProject form AutoSize?
    * allow switching list/icon/etc. view on "Files"
    * allow configuring command output "word wrap"
    * filter out stuff in "Files" (castle-engine-output, *~, created binaries)
      (need to use custom draw for this? grep, search code)
    * "Files" showroot=false doesn't work?
    * TEditDirectory use at "new project"
    * Output ListBox has some width (and horiz scrollbar) on Windows, unrelated to anything?
    * remember ProjectForm state of maximized/not

Lowest priority (OK if not in 1st release)
* Project options:
    * Allow to configure project qualified name from "Project Options" in editor
    * Icon, other stuff from CastleEngineManifest.xml could be configuirable in editor
* Desing also X3D nodes inside TCastleScene. This would be powerful... But not for now. I also deliberately do not want to turn CGE editor into Blender :) For creating 3D models, the recommended workflow will remain to use external editor (like Blender), and only e.g. adjust materials in CGE (override material properties using material_properties.xml). An editor for X3D nodes would be great to add stuff not possible in Blender, though (Background, clip planes, primitives...).

## Contributing: When creating a new Lazarus form, remember to...

- Save form class `TFooForm` (so it will have singleton `FooForm`) in unit name `FormFoo`.
- Adjust form's `Caption`.
- Adjust `TabStop` of all the controls inside, to make it comfortable to use keyboard.
- Use `AutoSize` and anchoring on all controls, to work regardless of theme font size. Do not assume that a text will have the same size as you have designed --- people use various themes and font types. Lazarus applications have a native look, and are expected to adjust to user's theme preferences.
- Consider using `AutoSize` on the form itself too.
- Adjust `BorderStyle` from `bsSizeable` to `bsSingle` if it's a small form that doesn't need to be resized (for larger forms, it's safer to allow resizing, even if you think you know the best size -- in case user will view it on a smaller monitor).
- Adjust `Position` from "as designed" (usually "default" or "main form center" is more sensible).
- Make sure closing the form with "X" (Alt + F4) works OK.
- For a form you create manually, make sure it is freed at some point (preferably, not only at the end of application, if you can free it earlier; e.g. we don't want to have 100 of TProjectForm instances in memory after using the editor for a long time).