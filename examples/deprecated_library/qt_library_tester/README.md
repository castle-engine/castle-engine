# Example how to use Castle Game Engine from a Qt application written in C++

This uses _Castle Game Engine_ packed in a dynamic library, that exposes a simple C API to load and display 3D models.

Note: The library (for now) exposes only a tiny subset of _Castle Game Engine_ possibilities. To write full-featured games using CGE, you should instead use _Object Pascal_ and link with CGE Pascal units, instead of using CGE library. However, the library is a reasonable approach if you simply want to load / display / manipulate a single 3D model.

## How to compile

- First compile _Castle Game Engine_ dynamic library. Enter the `src/deprecated_library/` directory of CGE, and run `./castleengine_compile.sh`. This should produce `libcastleengine.so` (on Unix) or `castleengine.dll` (on Windows).

    _Note for Windows_: `castleengine_compile.sh` requires you to have Cygwin or MinGW installed. It may be easier to call `castleengine_compile_win32.bat` or `castleengine_compile_win64.bat`, these don't require anything special.

    _Note_: Make sure that <code>fpc</code> binary is available on the environment variable <code>$PATH</code>. If you don't know how to set the environment variable, search the Internet (e.g. <a href="https://www.computerhope.com/issues/ch000549.htm">these are quick instructions how to do it on various Windows versions</a>).

- Install <a href="https://www.qt.io/download">Qt Creator</a>.

    _Note for Linux_: It's probably easiest to install Qt and _Qt Creator_ using packages, like `apt install qtcreator qtbase5-dev` (Ubuntu) or `apt install qtcreator qt5-default` (Debian).

- Open the project `qt_library_tester.pro` in _Qt Creator_ and _Build_ it from there.

- Run the example, making sure that the compiled library is visible.

    _Note for Windows_: It's easiest to copy the `castleengine.dll` to the same directory as example `xxx.exe`. Then just execute the exe in any way. Alternatively, put the `dll` file somewhere that is listed on `$PATH`.

    _Note for Linux_: It's easiest to call it from command-line and set `LD_LIBRARY_PATH` earlier. Like this: `export LD_LIBRARY_PATH="$LD_LIBRARY_PATH:/$CASTLE_ENGINE_PATH/src/deprecated_library/" && ./qt_library_tester` .
