# Castle Game Engine Editor

Allows to manage projects,
where a "project" is a directory containing `CastleEngineManifest.xml` file.

## Features

You can create a new project (from a number of templates) or open an existing one.

You can compile and run the project on various platforms,
using the [Castle Game Engine Build Tool](https://github.com/castle-engine/castle-engine/wiki/Build-Tool)
underneath (which in turn calls Pascal compiler, like FPC or Delphi, underneath).

You can visually design:

* a hierarchy of Castle Game Engine user-interface controls. Anything descending from TUIControl, like a button, label, or a scene manager (that contains 2D or 3D scenes inside, that also can be designed in the same editor).

* or a hierachy of 3D or 2D scenes. Anything descending from TCastleTransform. This allows to design a 3D or 2D entity that you can add (using code) into your own TCastleSceneManager descendants.

Each such design is saved to a file `xxx.cge-control`. You can load it from code, and use however you like in the game (instantiate it whenever you want etc.).

You can open a text editor to edit source code (configurable; by default, we open Lazarus or Delphi, whichever is installed, since they offer advanced code completion for Pascal code).

We automatically set up project files such that you can run the game from Lazarus or Delphi (to use their built-in debugger). So, you can either compile/run from the CGE editor (which will use our build tool, that wraps Lazarus/Delphi) or you can compile/run from Lazarus or Delphi (for desktop platforms).

You can browse the application files. Our "Files Browser" just displays the files inside your project directory.

* It only omits some known unimportant things, like temporary `castle-engine-output` directory. But it displays everything else.

* Note that the `data/` subdirectory, that you will usually create in every non-trivial CGE project, is somewhat special. It is automatically detected (by it's name `data`), it automatically packaged (e.g. in Android apk), and it is used by `ApplicationData` function. You will place there 3D models, 2D images and everything else you load in game.

    It is some equivalent to Unity3d `Assets/` subdirectory.

* Note that your Pascal source code should be outside the `data/` subdirectory. Actually, your source code can be anywhere within the project, we don't have any requirement here. You can put it in `code/` subdirectory, `src/` subdirectory, no subdirectory (top level), wherever you like. If you really want, you can also place it in `data/` subdirectory, but it usually doesn't make sense (unless you really want to distribute to end-users your source code this way).

    This is in contrast to Unity3d (that requires putting source code also inside `Assets/` directory).

* On various 3D and 2D assets you can run view3dscene. On 2D images you can run glViewImage. On text files you can run a text editor (see above). We also want to auto-generate and show a quick previews of them inside the CGE editor.

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

## TODO

Now:
* build tool integration:
    * show build tool progress at bottom, do not hang editor while waiting for build tool
      Colorize
      - For "run", always show log (on all platform), colorized (warnings at least)
      - For "compile", detect FPC warnings, errors
      What to use to colorize? http://wiki.freepascal.org/RichMemo
      Or see what Lazarus does for messages list?
* Files browser as above
* Visual inspector. designer etc.
* All the plans from https://castle-engine.io/wp/2017/12/23/plans-6-4-release-asap-visual-editor-soon-2018-roadmap/

Lower priority:
* templates:
    * Create other than "empty" project templates
    * Proper screenshots of all project templates
* build tool integration:
    * Smartly detect CASTLE_ENGINE_PATH, and set it for subprocesses, see Michalis ~/common/TODO
    * Smartly detect castl-engine exe (look in CASTLE_ENGINE_PATH/bin etc.), see Michalis ~/common/TODO
    * Detect lack of FPC / Delphi and make a nice error message
    * Allow to auto-generate-textures, any other build tool command remains useful?
    * Allow to choose platform
    * rerun generate-program each time? (mark them as some *DO NOT MODIFY THIS, THIS IS ONLY FOR LAZARUS* comment)
        Not really OK, in case we open program with hand-crafted program file.
	Maybe only auto-generate in castle-engine-output,
	  before opening Lazarus,
	  if lpr not yet present?
	Best: change to use planned CastleEngineConfig.pas unit,
  	  that is always auto-generated without warning.
	  Do not overwrite lpr each time.
* small GUI stuff:
    * Show on recent list %20 as spaces, use URICaption or such ready function?
    * on NewProject form AutoSize?
    * allow switching list/icon/etc. view on "Files"
    * allow configuring command output "word wrap"
    * filter out stuff in "Files" (castle-engine-output, *~, created binaries)
      (need to use custom draw for this? grep, search code)
    * "Files" showroot doesn't work

Lowest priority (OK if not in 1st release)
* Project options:
    * Allow to configure project qualified name from "Project Options" in editor
    * Icon, other stuff from CastleEngineManifest.xml could be configuirable in editor

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