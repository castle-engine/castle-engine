Libraries (DLL files) for Windows 64-bit, to be distributed
together with Windows 64-bit programs using the Castle Game Engine
( http://castle-engine.sourceforge.net/ ).

You need to distribute these files together with your Windows programs,
alongside the exe file. You can use our build tool (see
https://github.com/castle-engine/castle-engine/wiki/Build-Tool )
to easily package your programs together with their dependendent libraries.

Origins:
- freetype, libpng and zlib: http://www.gtk.org/download/win64.php
  ("Third Party Dependencies", links to
  http://ftp.gnome.org/pub/GNOME/binaries/win64/dependencies/ )

- OpenAL: as installed by Creative's OpenAL Windows installer,
  from http://www.openal.org/creative-installers/

- OggVorbis: http://www.vorbis.com/ libraries were compiled on
  Jan Adamec's computer using VS Express 2013 from sources downloaded
  from http://xiph.org/downloads/ (libogg 1.3.2, libvorbis 1.3.4)
