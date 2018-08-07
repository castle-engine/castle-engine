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

You can browse the apllication data (in the "data/" subdirectory). On various 3D and 2D assets you can run view3dscene. On 2D images you can run glViewImage. On text files you can run a text editor (see above). We also want to auto-generate and show a quick previews of them inside the CGE editor.

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

* build tool integration:
    * In TChooseProject.ButtonNewClick, create lpi/lpr using build tool commands to generate
    * Smartly detect CASTLE_ENGINE_PATH, and set it for subprocesses, see Michalis ~/common/TODO
    * Smartly detect castl-engine exe (look in CASTLE_ENGINE_PATH/bin etc.), see Michalis ~/common/TODO
    * Detect lack of FPC / Delphi and make a nice error message
    * Allow to compile, run, package, choose platform
    * rerun generate-program each time? (mark them as some *DO NOT MODIFY THIS, THIS IS ONLY FOR LAZARUS* comment)
        Not really OK, in case we open program with hand-crafted program file.
	Maybe only auto-generate in castle-engine-output,
	  before opening Lazarus,
	  if lpr not yet present?
	Best: change to use planned CastleEngineConfig.pas unit,
  	  that is always auto-generated without warning.
	  Do not overwrite lpr each time.
* templates:
    * Create other than "empty" project templates
    * Proper screenshots of all project templates
* Visual inspector. designer etc.
* All the plans from https://castle-engine.io/wp/2017/12/23/plans-6-4-release-asap-visual-editor-soon-2018-roadmap/

Lower priority:
* Project options:
    * Allow to configure project qualified name from "Project Options" in editor
    * Icon, other stuff from CastleEngineManifest.xml could be configuirable in editor
* Show on recent list %20 as spaces, use URICaption or such ready function?