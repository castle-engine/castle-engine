Libraries (DLL files) for Windows 64-bit, to be distributed
together with Windows 64-bit programs using the Castle Game Engine
( https://castle-engine.io/ ).

You need to distribute these files together with your Windows programs,
alongside the exe file. You can use our build tool (see
https://castle-engine.io/build_tool )
to easily package your programs together with their dependendent libraries.

Origins:
- libpng and zlib: http://www.gtk.org/download/win64.php
  ("Third Party Dependencies", links to
  http://ftp.gnome.org/pub/GNOME/binaries/win64/dependencies/ )

- OpenAL: as installed by Creative's OpenAL Windows installer,
  from http://www.openal.org/creative-installers/

- OggVorbis: http://www.vorbis.com/ libraries were compiled on
  Jan Adamec's computer using VS Express 2013 from sources downloaded
  from http://xiph.org/downloads/ (libogg 1.3.2, libvorbis 1.3.4)

- FreeType: https://www.freetype.org/download.html
  which links to https://github.com/ubawurinna/freetype-windows-binaries

  wget 'https://github.com/ubawurinna/freetype-windows-binaries/raw/master/release%20dll/win64/freetype.dll' --output-document=freetype.dll

- Microsoft Redistributables:

  - General info:
    How: Install x64 version, copy dll from c:/windows/system32/vcruntime140.dll .
    (yes, the 64-bit version is placed in system32.)

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
