Libraries (DLL files) for Windows on ARM64, to be distributed
together with Windows on ARM64 programs using the Castle Game Engine
( https://castle-engine.io/ ).

You need to distribute these files together with your Windows programs,
alongside the exe file. You can use our build tool (see
https://castle-engine.io/build_tool )
to easily package your programs together with their dependendent libraries.

Origins:
- zlib: https://www.zlib.net/ (version 1.3.2) library was compiled on
  Jan Adamec's computer using VS Community 2026 from sources downloaded
  from https://www.zlib.net/

- libpng: https://www.libpng.org/ (version 1.6.56) library was compiled on
  Jan Adamec's computer using VS Community 2026 from sources downloaded
  from https://www.libpng.org/pub/png/libpng.html

- OpenAL: used OpenAL Soft https://openal-soft.org/, library was compiled on
  Jan Adamec's computer using VS Community 2026 from sources downloaded
  from https://github.com/kcat/openal-soft/releases (1.25.1)
  
- OggVorbis: http://www.vorbis.com/ libraries were compiled on
  Jan Adamec's computer using VS Community 2026 from sources downloaded
  from http://xiph.org/downloads/ (libogg 1.3.6, libvorbis 1.3.7)

- FreeType: https://www.freetype.org/download.html
  which links to https://github.com/ubawurinna/freetype-windows-binaries

  wget 'https://github.com/ubawurinna/freetype-windows-binaries/raw/refs/heads/master/release%20dll/arm64/freetype.dll' --output-document=freetype.dll

- Microsoft Redistributables:

  - General info:
    How: Install vc_redist.arm64.exe, or copy the DLLs to app dir. The files
    were taken from "\\Program Files\Microsoft Visual Studio\18\Community\VC\Redist\MSVC\14.50.35710\arm64"

    Licensing:
    https://learn.microsoft.com/en-us/cpp/windows/latest-supported-vc-redist?view=msvc-170
    https://visualstudio.microsoft.com/en/license-terms/vs2026-ga-visualcpp-v14-redist-runtime/

  - msvcp140.dll, msvcp140_1.dll, msvcp140_2.dll, vcruntime140.dll:
    (dependency for all libraries compiled with VS Community 2026)
    Visual C++ v14 Redistributable
    https://aka.ms/vc14/vc_redist.arm64.exe
