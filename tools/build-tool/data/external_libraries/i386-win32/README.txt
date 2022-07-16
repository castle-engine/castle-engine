Libraries (dll files) required by Castle Game Engine
( https://castle-engine.io/ ) programs on Windows 32-bit (i386).

You need to distribute them together with your Windows 32-bit programs,
alongside the exe file. You can use our build tool (see
https://castle-engine.io/build_tool )
to easily package your programs together with their dependendent libraries.

Note that not all programs require all the libraries.
For example, OpenAL and OggVorbis is only used if you want 3D sound.
FreeType is only used if you want to read font files (like .ttf).
See https://castle-engine.io/apidoc/html/index.html
for details what is required for what purpose.

Sources of these DLLs:
- libpng, zlib: http://gnuwin32.sourceforge.net/ .
  - libpng depends on zlib.
  - freetype depends on zlib.

- OpenAL: as installed by Creative's OpenAL Windows installer,
  from http://www.openal.org/downloads.html

- OggVorbis: http://www.vorbis.com/ (don't remember exactly where,
  but I found some archive with precompiled vorbis libs for Windows
  looking from http://www.vorbis.com/)

- FreeType: https://www.freetype.org/download.html
  which links to https://github.com/ubawurinna/freetype-windows-binaries

  wget 'https://github.com/ubawurinna/freetype-windows-binaries/raw/master/release%20dll/win32/freetype.dll' --output-document=freetype.dll

- Microsoft Redistributables:

  - General info:
    How: Install x86 version, copy dll from c:/windows/syswow64/vcruntime140.dll .
    (it is confusing, yes -- the 32-bit version is placed in syswow64.)

    Licensing:
    https://docs.microsoft.com/en-us/cpp/windows/redistributing-visual-cpp-files?view=msvc-160
    https://visualstudio.microsoft.com/pl/license-terms/mlt552233/
    https://visualstudio.microsoft.com/pl/license-terms/mt171552/

  - vcruntime140.dll (FreeType dependency):
    Visual C++ 2015 Redistributable
    https://www.microsoft.com/en-us/download/details.aspx?id=52685

  - msvcrt120.dll (libvorbis.dll, vorbisfile.dll dependency)
    Visual C++ 2013 Redistributable
    https://support.microsoft.com/en-us/topic/the-latest-supported-visual-c-downloads-2647da03-1eea-4433-9aff-95f26a218cc0
