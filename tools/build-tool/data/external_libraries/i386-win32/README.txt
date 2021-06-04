Libraries (dll files) required by Castle Game Engine
( https://castle-engine.io/ ) programs on Windows 32-bit (i386).

You need to distribute them together with your Windows 32-bit programs,
alongside the exe file. You can use our build tool (see
https://github.com/castle-engine/castle-engine/wiki/Build-Tool )
to easily package your programs together with their dependendent libraries.

Note that not all programs require all the libraries.
For example, OpenAL and OggVorbis is only used if you want 3D sound.
FreeType is only used if you want to read font files (like .ttf).
See https://castle-engine.io/apidoc/html/index.html
for details what is required for what purpose.

Sources of these DLLs:
- libpng, zlib, freetype: http://gnuwin32.sourceforge.net/ .
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
