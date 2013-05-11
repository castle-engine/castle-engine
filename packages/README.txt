In this directory you can find Lazarus packages that allow you
to easily use Castle Game Engine in Lazarus programs.

Short instructions: just compile and install castle_base and castle_components.
*Do not* install castle_window in Lazarus (only compile it).
castle_components depends on standard Lazarus package LazOpenGLContext.

----------------------------------------
Usage:

  1. Open wanted castle_xxx.lpk file in Lazarus and compile the package.

  2. When you want to use the units included in castle_xxx.lpk
     in your program, just add this castle_xxx package
     as a "Required Package" for your project
     (using "Project" -> "Project Inspector" dialog).

     That's it. Lazarus will automatically take care of such things as
     recompiling the units in the package when needed (and with
     proper command-line options) or adding
     appropriate -Fu command-line options for FPC so that FPC
     "sees" units in the package when compiling your program.

  3. For castle_base and castle_components: you usually want to install
     these packages in Lazarus. This way you get our components
     in the Lazarus palette (tab "Castle"), you can drop them on the form,
     operate with the object inspector etc.

     Installing isn't really stricly required (you could just create
     all components by code, and add package to "Required Package"
     of your project) but usually it's much more comfortable to have them
     installed.

     Note that "castle_components" package depends on standard Lazarus
     package "LazOpenGLContext". So compile LazOpenGLContext first.
     Just open components/opengl/lazopenglcontext.lpk from Lazarus sources,
     and compile + install it under Lazarus.
     You can then try examples/openglcontrol/openglcontrol_demo.lpk,
     also in Lazarus sources, to check that "LazOpenGLContext" works Ok.

     For castle_window: *do not* install this in Lazarus.
     This package uses CastleWindow unit, and merely using this unit
     will cause the program to initialize TGLApplication instance,
     which in turn initiates some talk with WindowManager, GTK toolkit, etc.
     This would conflict with what Lazarus IDE (and LCL) does,
     causing unpredictable results.

     So castle_window package is supposed only to be "used" (by adding
     it to "Required Packages" in project inspector),
     never installed in Lazarus IDE.

Mini-FAQ about these packages:

- Do I need Lazarus to compile Castle Game Engine?

  No. Packages here are provided to easily compile and use the engine
  with Lazarus. But actually units in castle_game_engine don't use
  any Lazarus units (LCL) (besides Lazarus-specific components in
  ../src/components/ subdirectory, corresponding to castle_components package).

  So all you actually need is bare FPC installation.
  Use castle_game_engine/Makefile to compile all units and use compile.sh
  scripts to compile each particular program. If you need more flexibility,
  take a look at castle_game_engine/castle-fpc.cfg, this specifies actual options
  used by castle_game_engine/Makefile and all compile.sh scripts.

- What is the difference between castle_base and castle_components:

  castle_components has classes (components) depending on Lazarus LCL.
  You really cannot use them without Lazarus.

  castle_base doesn't depend on Lazarus LCL. This is the stuff available
  to you also when you don't use Lazarus (or even LCL) at all.

  castle_components requires (that is, automatically installs) also castle_base
  package --- that's OK.

- What is alternative_castle_window_based_on_lcl?

  This is an alternative version of castle_window.lpk package that
  provides CastleWindow unit using Lazarus LCL as a backend.
  It's useful for platforms where we don't have a better CastleWindow backend.

  In practice, use this only for Mac OS X, see

    http://castle-engine.sourceforge.net/macosx_requirements.php

  In principle you can use this for other systems as well (Linux, Windows and such),
  but the standard castle_window.lpk package will usually give you a better
  CastleWindow (that uses native backend, like WinAPI or GTK directly).

------------------------------------------------------------------------------
See ../examples/lazarus/ for demo Lazarus programs using these packages.

------------------------------------------------------------------------------
Development notes:

How to cope with system-specific units and include files inside
a single Lazarus package file?

  (This concerns stuff inside */unix/, */windows/ subdirectories,
  like base/unix|windows, opengl/unix|windows).

  - Add all files, even the system-specific, to the package
    (this way e.g. dependency checking for .compiled files will be Ok).
    This also means that we have to add all paths,
    both for unix/ and windows/, to the package.

  - All include/units files must have unique names.

    I used to have a hack like this: place file unix/xxx.inc
    and windows/xxx.inc, and then using {$I xxx.inc}
    I knew that the correct (for current platform) xxx.inc would be included.
    This is not possible anymore --- both unix/ and windows/ will be
    on the include path for all of the platforms,
    so you cannot depend on include path to select the proper file.

    Not a real problem, I used this hack only in one place actually...

  - For units in platform-specific dirs, it's crucial to set
    AddToUsesPkgSection Value="False". This means that they
    will be compiled only when pulled by dependencies from other units,
    and all will be Ok.

Michalis
