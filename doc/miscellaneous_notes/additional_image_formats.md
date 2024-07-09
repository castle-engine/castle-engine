# Additional image formats

## Introduction

In _Castle Game Engine_ we rely on [Vampyre Imaging Library](https://imaginglib.sourceforge.io/) to support most of our image formats.

( There are some exceptions, the most prominent are [KTX](https://castle-engine.io/ktx) and [DDS](https://castle-engine.io/dds) formats that are implemented directly in our engine, to exercise all of their special 3D-related features. And PNG format, that is implemented using _LibPng_ (for maximum read speed), and _Vampyre Imaging Library_ PNG reader is only used as a fallback. In the past, the situation was more complicated, we also used _FpImage_ (but it is slower at reading and FPC-specific) and we had more formats implemented directly in CGE (but we removed them, in favor of delegating more to _Vampyre Imaging Library_). )

However, some image formats supported by _Vampyre Imaging Library_ are *not* supported out-of-the-box in _Castle Game Engine_. This concerns:

- TIFF
- JPEG2000

Reasons:

1. Their implementation in _Vampyre Imaging Library_ is platform-specific, and doesn't support all compilers and platforms we support, at least not easily.

2. Not trivial to use, as you need _LibTiff_ shared library in case of FPC + Linux, macOS, or Delphi + Win64.

For example, the TIFF format relies on a shared library _LibTiff_ for some platform/compiler combinations. It is uncertain can we support it on mobile (Android, iOS). We don't want a situation in which you design your graphics using TIFF on a desktop, and are surprised that on mobile you need to convert everything to PNG.

It is also an extra burden to implement, due to need to deploy _LibTiff_ to all the potential platforms. We have to limit such work, esp. when we can recommend a better replacement -- in case of TIFF, we recommend using PNG.

## Example project for thigns below

Clone https://github.com/castle-engine/castle-image-viewer and switch to branch `extra-image-formats`.

It has all things described below, including:

- modifications in `CastleEngineManifest.xml` to enable TIFF support and package all new libraries and scripts added below,
- `run.sh` to set `LD_LIBRARY_PATH` to find `libtiff.so.5` on Linux,
- Linux libraries in `libraries/x86_64-linux/` .

## Activating extra support

That said, you can compile the engine with `CASTLE_ENABLE_PLATFORM_SPECIFIC_IMAGE_FORMATS` symbol, so add suppport for TIFF. Define `CASTLE_ENABLE_PLATFORM_SPECIFIC_IMAGE_FORMATS` e.g. inside `CastleEngineManifest.xml` (see https://castle-engine.io/project_manifest#_compiler_options_and_paths ).

## Getting LibTiff on Linux

Loading TIFF images on FPC + Linux requires `libtiff.so.5`.

Note: The _LibTiff_ "so" suffix naming (API version) is inconsistent with source code versioning.

- Compiling _LibTiff_ 3.9.6 (latest 3.x) -> results in `libtiff.so.3`.
- Compiling _LibTiff_ 4.6.0 (latest now) -> results in `libtiff.so.6`.
- See https://libtiff.gitlab.io/libtiff/releases/v4.5.0.html : _SONAME version bumped to 6 due to changes in symbol versioning._
- So... 4.4.0 is the latest that makes `libtiff.so.5`.

Follow https://libtiff.gitlab.io/libtiff/ to download the latest source code, right now it is https://download.osgeo.org/libtiff/tiff-4.4.0.zip .

Unpack and compile yourself, it follows the usual procedure for compiling C libraries on Linux. Below is just an example command-line, following personal preference of Michalis to put resulting lib in `~/installed/libtiff`:

```
./configure
make
mkdir -p ~/installed/libtiff
make install DESTDIR=$HOME/installed/libtiff
```

Now you have libraries in `~/installed/libtiff/usr/local/lib` (you could also get them from `libtiff/.libs` directory in source, but using `make install ...` seems cleaner, allows to do any necessary processing). Copy them to your project, and setup `run.sh` to set `LD_LIBRARY_PATH` to this directory. Our CGE build tool and editor will automatically run your project through this script.

Example `run.sh` contents:

```
#!/bin/bash
set -e
# Include additional directory in LD_LIBRARY_PATH, to find libtiff
export LD_LIBRARY_PATH="${LD_LIBRARY_PATH}:libraries/x86_64-linux/"
./castle-image-viewer "$@"
```

Example how to copy `libtiff.*` libraries to your project and test:

```
cd ~/sources/castle-engine/castle-image-viewer
chmod +x run.sh
mkdir -p libraries/x86_64-linux
cp -Rf ~/installed/libtiff/usr/local/lib/* libraries/x86_64-linux/
# make sure it makes sense, should show libtiff.so and valid symlinks
ls -Flah libraries/x86_64-linux/
castle-engine compile
castle-engine run # will run run.sh
```

## Getting LibTiff on Windows

Loading TIFF images on Windows requires `libtiff.dll` in case of compiling with Delphi for Win64.

NOTE: The DLL is not necessary for Delphi + Win32 or FPC + any Windows (32 or 64). These combinations use ready TIFF support compiled-in (the `obj` files in `src/vampyre_imaginglib/src/Extensions/LibTiff/Compiled/`) and it should just work out-of-the-box. You can ignore this section if you don't care about Delphi + Win64.

Following http://www.libtiff.org/build.html :

- Get CMake, from https://cmake.org/download/ . I found it easiest to use Windows installer. Allow it to modify PATH, to have `cmake` on command-line.

- TODO: Finish this.

   And commit resulting DLL to castle-image-viewer in extra-image-formats branch.

   And mention it has DLL in "## Example project for thigns below" section above.

## Packacking LibTiff linbraries and run.sh script

Extend your `CastleEngineManifest.xml` to package extra libraries and scripts:

```xml
<package>
  <include path="libtiff.dll" /> <!-- necessary if you use Delphi + Win64 -->
  <include path="run.sh" />
  <include path="libraries/x86_64-linux/" recursive="True" />
</package>
```