# Pack CGE into a shared library to load and visualize 3D models

Using this code you can compile CGE into a dynamic library
that exposes a simple C API in `castleengine.h` .
It is a very limited API, that allows to load and display a single 3D model
(anything that can be [loaded into TCastleScene](https://castle-engine.io/creating_data_model_formats.php)).
As such, it can be used to develop 3D model viewers from other languages (like C or C++)
using CGE.

See examples in `../../examples/deprecated_library/` that use this library from
Qt application in C++ on desktop, iOS application in Objective-C etc.

## Compiling

Enter this (`src/deprecated_library/`) directory of CGE, and run `./castleengine_compile.sh` shell script.

This should produce `libcastleengine.so` (on Unix) or `castleengine.dll` (on Windows).

_Note for Windows_: `castleengine_compile.sh` requires you to have Cygwin or MinGW installed. It may be easier to call `castleengine_compile_win32.bat` or `castleengine_compile_win64.bat`, these don't require anything special.

_Note_: Make sure that <code>fpc</code> binary is available on the environment variable <code>$PATH</code>. If you don't know how to set the environment variable, search the Internet (e.g. <a href="https://www.computerhope.com/issues/ch000549.htm">these are quick instructions how to do it on various Windows versions</a>).

## Deprecation

This approach to using CGE is deprecated, because

- It is inherently very limited.

    The C API in castleengine.h exposes only a tiny API to load a single 3D model
    and display it. It doesn't allow:
    - using the full TCastleScene API,
    - multiple TCastleScene,
    - adjust TCastleViewport,
    - use all TCastleUserInterface descendants,
    - loading designs from editor,
    - using CGE SoundEngine...

- It didn't gain much usage.

    The only significant application using it is https://www.roomarranger.com/ .

The way forward is to move it to a separate repository, with name that also clearly reflects usage (we don't want to call it "Castle Game Engine library", better name would be "library using CGE to view 3D models" to reflect a more limited use-case).
