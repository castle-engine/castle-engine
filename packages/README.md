# Lazarus packages to use Castle Game Engine in Lazarus

## Instructions

Register in Lazarus all packages using the button _"Register Lazarus Packages"_ in CGE editor _"Preferences -> FPC and Lazarus"_ (see https://castle-engine.io/install ).

Then, optionally, you can install the `castle_components.lpk` package in Lazarus, to have LCL component `TCastleControl` (see https://castle-engine.io/control_on_form ).

Please use this approach instead of manually compiling/installing .lpk files, and hunting for their dependencies :)

## Packages desciption

### castle_base.lpk

Base engine code and components.

This also includes [Vampyre Imaging Library](https://imaginglib.sourceforge.io/) (you do not have to install it by a separate package).

### castle_window.lpk

`TCastleWindow` class and associated unit.

Should never be installed in Lazarus, as the `CastleWindow` is talking directly to the window system, and would conflict with LCL trying to do the same.

Depends on `castle_base.lpk`.

Add this package as a _requirement_ to Lazarus applications that use `TCastleWindow`.

### castle_components.lpk

Engine code and components using LCL. TODO: a better name of this package would be `castle_lcl.lpk`.

Depends on

- `castle_base.lpk`.

- LCL.

- standard Lazarus package `LazOpenGLContext`.

    Lazarus should automatically know its location, but if necessary you can find it and manually compile in `<lazarus>/components/opengl/lazopenglcontext.lpk`.

Add this package as a _requirement_ to Lazarus applications that use `TCastleControl` (our LCL component).

### castle_editor_components.lpk

Components used by CGE editor. Engine developers (who wish to modify CGE editor forms) should install this in Lazarus IDE.

These components are also used by custom CGE editor builds (with project-specific components, if you use https://castle-engine.io/custom_components ). So "normal" engine users don't need to install this, but they should "register" it with Lazarus, so that you can rebuild Lazarus with custom components.

All the units inside this package are internal for CGE -- normal engine users should not rely on this code at all, it is only internally used by CGE editor.

Note: We avoid adding dependencies on packages elsewhere in CGE directories, like `tools/castle-editor/contrib/mbColorLib/mbcolorliblaz.lpk`. Because this would confuse some people, because seasoned Lazarus users routinely install lpk directly, scanning our "packages/" subdirectory, and not knowing about CGE button "Register Lazarus Packages" (even though it's documented early in manual). So let's keep it simple and store all .lpk here.

Depends on `castle_components.lpk`.

### alternative_castle_window_based_on_lcl.lpk

Alternative version of `castle_window.lpk` package that provides `CastleWindow` unit using Lazarus LCL as a backend. It's useful for platforms where we don't have a better CastleWindow backend.

In practice, it has no use now. (In the past, it was useful for macOS.)

### castle_indy.lpk

There is one, completely optional unit in CGE using Indy: `CastleClientServer`. This package allows to compile applications within it.

Example usage of it is in `examples/network/tcp_connection/`.

Depends on:
- castle_base
- indylaz

TODO: To avoid confusion (as CGE in general doesn't depend on Indy) we should probably move it to separate repo.

## Do I need Lazarus to use Castle Game Engine?

No. Packages here are provided to easily compile and use the engine with Lazarus. But CGE doesn't require LCL, you can develop CGE applications using `TCastleWindow` that doesn't depend on LCL.

So all you actually need is bare FPC installation. Use our editor (https://castle-engine.io/manual_editor.php) or build tool (https://castle-engine.io/build_tool) to compile all examples and tools.

## Development notes: Conventions about these packages

How to cope with system-specific units and include files inside
a single Lazarus package file?

  (This concerns stuff inside */unix/, */windows/ subdirectories,
  like base/unix|windows, opengl/unix|windows).

  - Add all files, .inc and .pas, even the system-specific, to the package
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

  - For units in platform-specific dirs, set "Add to Uses Clause" to false
    checkbox (at the unit configuration inside the package window).
    Otherwise compiling package on other platforms would fail.
