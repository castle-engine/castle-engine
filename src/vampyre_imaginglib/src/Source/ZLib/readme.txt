_____________________________________________________________________________

PASZLIB 1.0                                                   May 11th, 1998

Based on the zlib 1.1.2, a general purpose data compression library.

Copyright (C) 1998,1999,2000 by NOMSSI NZALI Jacques H. C. 
[kn&n DES]         See "Legal issues" for conditions of distribution and use.
_____________________________________________________________________________


Introduction
============

The 'zlib' compression library provides in-memory compression and
decompression functions, including integrity checks of the uncompressed
data.  This version of the library supports only one compression method
(deflation) but other algorithms will be added later and will have the same
stream interface.

Compression can be done in a single step if the buffers are large
enough (for example if an input file is mmap'ed), or can be done by
repeated calls of the compression function.  In the latter case, the
application must provide more input and/or consume the output
(providing more output space) before each call.

The default memory requirements for deflate are 256K plus a few kilobytes
for small objects. The default memory requirements for inflate are 32K
plus a few kilobytes for small objects.

Change Log
==========

March 24th 2000 - minizip code by Gilles Vollant ported to Pascal. 
                  z_stream.msg defined as string[255] to avoid problems
                  with Delphi 2+ dynamic string handling.
                  changes to silence Delphi 5 compiler warning. If you
                  have Delphi 5, defines Delphi5 in zconf.inc
                              
May 7th 1999    - Some changes for FPC
                  deflateCopy() has new parameters
                  trees.pas - record constant definition
June 17th 1998  - Applied official 1.1.2 patch. 
	          Memcheck turned off by default.
                  zutil.pas patch for Delphi 1 memory allocation corrected.
                  dzlib.txt file added.
                  compress2() is now exported

June 25th 1998 -  fixed a conversion bug: in inftrees.pas, ZFREE(z, v) was
                  missing in line 574;

File list
=========

Here is a road map to the files in the Paszlib distribution.

readme.txt      Introduction, Documentation
dzlib.txt       Changes to Delphi sources for Paszlib stream classes 

include file

zconf.inc       Configuration declarations.

Pascal source code files:

adler.pas      compute the Adler-32 checksum of a data stream
crc.pas        compute the CRC-32 of a data stream
gzio.pas       IO on .gz files
infblock.pas   interpret and process block types to last block
infcodes.pas   process literals and length/distance pairs
inffast.pas    process literals and length/distance pairs fast
inftrees.pas   generate Huffman trees for efficient decoding
infutil.pas    types and macros common to blocks and codes
strutils.pas   string utilities
trees.pas      output deflated data using Huffman coding
zcompres.pas   compress a memory buffer
zdeflate.pas   compress data using the deflation algorithm
zinflate.pas   zlib interface to inflate modules
zlib.pas       zlib data structures. read the comments there!
zuncompr.pas   decompress a memory buffer
zutil.pas

minizip/ziputils.pas data structure and IO on .zip file 
minizip/unzip.pas  
minizip/zip.pas
      
Test applications

example.pas    usage example of the zlib compression library
minigzip.pas   simulate gzip using the zlib compression library
minizip/miniunz.pas  simulates unzip using the zlib compression library
minizip/minizip.pas  simulates zip using the zlib compression library

Legal issues
============

Copyright (C) 1998,1999,2000 by Jacques Nomssi Nzali

  This software is provided 'as-is', without any express or implied
  warranty.  In no event will the author be held liable for any damages
  arising from the use of this software.

  Permission is granted to anyone to use this software for any purpose,
  including commercial applications, and to alter it and redistribute it
  freely, subject to the following restrictions:

  1. The origin of this software must not be misrepresented; you must not
     claim that you wrote the original software. If you use this software
     in a product, an acknowledgment in the product documentation would be
     appreciated but is not required.
  2. Altered source versions must be plainly marked as such, and must not be
     misrepresented as being the original software.
  3. This notice may not be removed or altered from any source distribution.


Archive Locations:
==================

Check the Paszlib home page with links

      http://www.tu-chemnitz.de/~nomssi/paszlib.html

The data format used by the zlib library is described by RFCs (Request for
Comments) 1950 to 1952 in the files ftp://ds.internic.net/rfc/rfc1950.txt
(zlib format), rfc1951.txt (deflate format) and rfc1952.txt (gzip format).
These documents are also available in other formats from
ftp://ftp.uu.net/graphics/png/documents/zlib/zdoc-index.html.
____________________________________________________________________________
Jacques Nomssi Nzali <mailto:nomssi@physik.tu-chemnitz.de> March 24th, 2000