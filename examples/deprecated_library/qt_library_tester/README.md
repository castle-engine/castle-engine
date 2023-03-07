# Example how to use Castle Game Engine from a Qt application written in C++

This uses _Castle Game Engine_ packed in a dynamic library, that exposes a simple C API to load and display 3D models.

Note: The library (for now) exposes only a tiny subset of _Castle Game Engine_ possibilities. To write full-featured games using CGE, you should instead use _Object Pascal_ and link with CGE Pascal units, instead of using CGE library. However, the library is a reasonable approach if you simply want to load / display / manipulate a single 3D model.

## How to compile

- First compile _Castle Game Engine_ dynamic library to load and visualize 3D models.

    See `../../../src/deprecated_library/README.md` .

- Install <a href="https://www.qt.io/download">Qt Creator</a>.

    _Note for Linux_: It's probably easiest to install Qt and _Qt Creator_ using packages, like `apt install qtcreator qt6-base-dev ` (Ubuntu). I found it also beneficial to remove Qt5 packages (like `qt5-qmake`), so that you only have Qt6.

- Open the project `qt_library_tester.pro` in _Qt Creator_ and _Build_ it from there.

- Run the example, making sure that the compiled library is visible.

    _Note for Windows_: It's easiest to copy the `castleengine.dll` to the same directory as example `xxx.exe`. Then just execute the exe in any way. Alternatively, put the `dll` file somewhere that is listed on `$PATH`.

    _Note for Linux_: It's easiest to call it from command-line and set `LD_LIBRARY_PATH` earlier. Like this: `export LD_LIBRARY_PATH="$LD_LIBRARY_PATH:/$CASTLE_ENGINE_PATH/src/deprecated_library/" && ./qt_library_tester` .
