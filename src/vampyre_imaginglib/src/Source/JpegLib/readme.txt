_____________________________________________________________________________

PASJPEG 1.1                                                   May 29th, 1999

Based on the Independent JPEG Group's JPEG software release 6b

Copyright (C) 1996,1998,1999 by NOMSSI NZALI Jacques H. C.
[kn&n DES]         See "Legal issues" for conditions of distribution and use.
_____________________________________________________________________________


Information in this file
========================

  o Introduction
  o Notes
  o File list
  o Translation
  o Legal issues
  o Archive Locations

Introduction
============

PASJPEG is a port of the sixth public release of the IJG C source (release
6b of 27-Mar-98) [3], that implements JPEG baseline, extended-sequential, and
progressive compression processes to Turbo Pascal 7.0 for DOS (TP). The code
has been tested under Delphi 3.0, it can be ported to other Pascal
environments, since many compilers try to be compatible to TP.

JPEG (pronounced "jay-peg") is a standardized familly of algorithms for
compression of continous tone still images. Most JPEG processes are lossy,
the output image is not exactly identical to the input image. However, on
typical photographic images, very good compression levels can be obtained
with no visible change, and remarkably high compression levels are possible
if you can tolerate a low-quality image [1],[2]. The Independent JPEG Group
(IJG) has created a free, portable C library for JPEG compression and
decompression of JPEG images.

The IJG documentation (system architecture, using the IJG JPEG library,
usage and file list) is a must read. The files DEMO.PAS, TEST.PAS, CJPEG.PAS,
DJPEG.PAS and EXAMPLE.PAS demonstrate the usage of the JPEG decompression
and compression library. The RDJPGCOM application shows how to parse a JFIF
file.

Notes:
======

* Please report any errors/problems you may find in code and in the
  documentation (e.g. this README.TXT file).

* The sample applications (CJPEG, DJPEG) doesn't support all the options
  of the original C code. WRJPGCOM is not ported.

* Environment variable JPEGMEM syntax changed;

* You can modify the jpeg.pas unit from the Delphi 3 distribution to
  use PasJPEG.

Change log
==========

1. bugs fixed: 
   * in procedure read_gif_map(), unit RDCOLMAP.PAS (used by DJPEG sample 
     application). Davie Lee Reed <smatters@iquest.net>
   * -dct int and -dct fast now bytewise equal to the IJG output.
   * -dct float produced large files

2. Support for scripts

3. BASM version of JIDCTINT.PAS for Delphi 2 and 3.

4. images with integral sampling ratios were not decoded correctly.
   Create a jpeg file with cjpeg and the option "-sample 4x1" and try to decode 
   it with any software that uses PasJpeg. Thanks to Jannie Gerber for reporting
   this with a fix: In JDSAMPLE.PAS, procedure int_upsample(), 
         
    for h := pred(h_expand) downto 0 do
    begin
      outptr^ := invalue;
 +=>  inc(outptr);   { this is the culprit that was left out!!! }
      Dec(outcount);
    end;

File list
=========

Here is a road map to the files in the PasJPEG distribution.  The
distribution includes the JPEG library proper, plus two application
programs ("cjpeg" and "djpeg") which use the library to convert JPEG
files to and from some other popular image formats.  A third application
"jpegtran" uses the library to do lossless conversion between different
variants of JPEG.  There is also the stand-alone applications "rdjpgcom".

Documentation(see README for a guide to the documentation files):

readme.txt      Introduction, Documentation

Additional files

demo.pas        Demo program, uses example.pas
example.pas     Sample code for calling JPEG library.
test.pas        Sample application code for demo.pas

Configuration/installation files and programs (see install.doc for more info):

jconfig.inc     Configuration declarations.

*.ijg           script files

Pascal source code files:

jinclude.pas    Central include file used by all IJG .c files to reference
		system include files.
jpeglib.pas     JPEG library's internal data structures, exported data
                and function declarations.
jmorecfg.pas    Additional configuration declarations; need not be changed
                for a standard installation.
jdeferr.pas     defines the error and message text.
jerror.pas      Declares JPEG library's error and trace message codes.
jinclude.pas    the place to specify system depedent input/output code.
jdct.pas        Private declarations for forward & reverse DCT subsystems.

These files contain most of the functions intended to be called directly by
an application program:

jcapimin.pas    Application program interface: core routines for compression.
jcapistd.pas    Application program interface: standard compression.
jdapimin.pas    Application program interface: core routines for decompression.
jdapistd.pas    Application program interface: standard decompression.
jcomapi.pas     Application program interface routines common to compression
                and decompression.
jcparam.pas     Compression parameter setting helper routines.
jctrans.pas     API and library routines for transcoding compression.
jdtrans.pas     API and library routines for transcoding decompression.

Compression side of the library:

jcinit.pas      Initialization: determines which other modules to use.
jcmaster.pas    Master control: setup and inter-pass sequencing logic.
jcmainct.pas    Main buffer controller (preprocessor => JPEG compressor).
jcprepct.pas    Preprocessor buffer controller.
jccoefct.pas    Buffer controller for DCT coefficient buffer.
jccolor.pas     Color space conversion.
jcsample.pas    Downsampling.
jcdctmgr.pas    DCT manager (DCT implementation selection & control).
jfdctint.pas    Forward DCT using slow-but-accurate integer method.
jfdctfst.pas    Forward DCT using faster, less accurate integer method.
jfdctflt.pas    Forward DCT using floating-point arithmetic.
jchuff.pas      Huffman entropy coding for sequential JPEG.
jcphuff.pas     Huffman entropy coding for progressive JPEG.
jcmarker.pas    JPEG marker writing.
jdatadst.pas    Data destination manager for stdio output.

Decompression side of the library:

jdmaster.pas    Master control: determines which other modules to use.
jdinput.pas     Input controller: controls input processing modules.
jdmainct.pas    Main buffer controller (JPEG decompressor => postprocessor).
jdcoefct.pas    Buffer controller for DCT coefficient buffer.
jdpostct.pas    Postprocessor buffer controller.
jdmarker.pas    JPEG marker reading.
jdhuff.pas      Huffman entropy decoding for sequential JPEG.
jdphuff.pas     Huffman entropy decoding for progressive JPEG.
jddctmgr.pas    IDCT manager (IDCT implementation selection & control).
jidctint.pas    Inverse DCT using slow-but-accurate integer method.
jidctasm.pas    BASM specific version of jidctint.pas for 32bit Delphi.
jidctfst.pas    Inverse DCT using faster, less accurate integer method.
jidctflt.pas    Inverse DCT using floating-point arithmetic.
jidctred.pas    Inverse DCTs with reduced-size outputs.
jidct2d.pas     How to for a direct 2D Inverse DCT - not used
jdsample.pas    Upsampling.
jdcolor.pas     Color space conversion.
jdmerge.pas     Merged upsampling/color conversion (faster, lower quality).
jquant1.pas     One-pass color quantization using a fixed-spacing colormap.
jquant2.pas     Two-pass color quantization using a custom-generated colormap.
		Also handles one-pass quantization to an externally given map.
jdatasrc.pas    Data source manager for stdio input.

Support files for both compression and decompression:

jerror.pas      Standard error handling routines (application replaceable).
jmemmgr.pas     System-independent (more or less) memory management code.
jutils.pas      Miscellaneous utility routines.

jmemmgr.pas relies on a system-dependent memory management module.  The
PASJPEG distribution includes the following implementations of the system-
dependent module:

jmemnobs.pas    "No backing store": assumes adequate virtual memory exists.
jmemdos.pas     Custom implementation for MS-DOS (16-bit environment only):
                can use extended and expanded memory as well as temporary
                files.
jmemsys.pas     A skeleton with all the declaration you need to create a
                working system-dependent JPEG memory manager on unusual
                systems.

Exactly one of the system-dependent units should be used in jmemmgr.pas.

jmemdosa.pas    BASM 80x86 assembly code support for jmemdos.pas; used only
                in MS-DOS-specific configurations of the JPEG library.


Applications using the library should use jmorecfg, jerror, jpeglib, and
include jconfig.inc.

CJPEG/DJPEG/JPEGTRAN

Pascal source code files:

cderror.pas     Additional error and trace message codes for cjpeg/djpeg.
                Not used, Those errors have been added to jdeferr.
cjpeg.pas       Main program for cjpeg.
djpeg.pas       Main program for djpeg.
jpegtran.pas    Main program for jpegtran.
cdjpeg.pas      Utility routines used by all three programs.
rdcolmap.pas    Code to read a colormap file for djpeg's "-map" switch.
rdswitch.pas    Code to process some of cjpeg's more complex switches.
                Also used by jpegtran.
transupp.pas    Support code for jpegtran: lossless image manipulations.

fcache.pas
rdswitch.pas    Code to process some of cjpeg's more complex switches.
                Also used by jpegtran.

Image file writer modules for djpeg:

wrbmp.pas       BMP file output.
wrppm.pas	PPM/PGM file output.
wrtarga.pas     Targa file output.

Image file reader modules for cjpeg:

rdbmp.pas       BMP file input.
rdppm.pas       PPM/PGM file input.
rdtarga.pas     Targa file input. - NOT READY YET

This program does not depend on the JPEG library

rdjpgcom.pas	Stand-alone rdjpgcom application.


Translation
===========

TP is unit-centric, exported type definitions and routines are declared
in the "interface" part of the unit, "make" files are not needed.
Macros are not supported, they were either copied as needed or translated
to Pascal routines (procedure). The procedures will be replaced by code in
later releases.
Conditional defines that indicate whether to include various optional
functions are defined in the file JCONFIG.INC. This file is included first
in all source files.

The base type definitions are in the unit JMORECFG.PAS. The error handling
macros have been converted to procedures in JERROR.PAS. The error codes are
in JDEFERR.PAS. jpegint.h and jpeglib.h were merged into one large unit
JPEGLIB.PAS containing type definitions with global scope.

The translation of the header file is the most sophisticated work, a good
understanding of the syntax is required. Once the header files are done,
the translation turns into a lot of editing work. Each C source file was
converted to a unit by editing the syntax (separate variable definition
and usage, define labels, group variable definitions, expanding macros, etc).

The IJG source labels routines GLOBAL, METHODDEF and LOCAL. All globals
routines are in the interface section of the units. The "far" directive is
used for methods (METHODDEF).

Some C  ->  Pascal  examples.

* "{"  -> "begin"    "->"  ->  "^."        " = "  -> " := "  "<<"  -> " shl "
  "}"  -> "end;"     "!="  ->  "<>"        " == " -> " = "   ">>"  -> " shr "
  "/*" -> "{"      routine ->  function    "0x"   -> "$"
  "*/" -> "}"      (void)      procedure   "NULL" -> "NIL"

* structs are records, Unions are variable records, pointers are always far,
  the operators && and || (and/or) have not the same priority in both
  languages, so parenthesis are important. The Pascal "case" doesn't have the
  falltrough option of the C "switch" statement, my work around is to split
  one "switch" statement into many case statements.
* The pointer type in C is not readily interchangeable. It is used to address
  an array (Pascal pointer to an array) or in pointer arithmetic a pointer to
  a single element. I've used the Inc() statement with type casting to
  translate pointer arithmetic most of the time.

  C example:
    typedef JSAMPLE* JSAMPROW;  /* ptr to one image row of pixel samples. */

  Pascal
  type
    JSAMPLE_PTR = ^JSAMPLE;     { ptr to a single pixel sample. }
    jTSample = 0..(MaxInt div SIZEOF(JSAMPLE))-1;
    JSAMPLE_ARRAY = Array[jTSample] of JSAMPLE;  {far}
    JSAMPROW = ^JSAMPLE_ARRAY;  { ptr to one image row of pixel samples. }

  The following code

    JSAMPROW buffer0, buffer1;  /* ptr to a JSAMPLE buffer. */

    ...

    buffer1 = buffer0 + i;

  can be translated to

  var
    buffer0, buffer1 : JSAMPROW;

  ...

    buffer1 := buffer0;
    Inc(JSAMPLE_PTR(buffer1), i);

  or

    buffer1 := JSAMPROW(@ buffer0^[i]);

  Declaring the variables as JSAMPLE_PTR may reduce type casting in some
  places. I use help pointers to handle negative array offsets.

While translating the type of function parameter from C to Pascal, one can
often use "var", "const", or "array of" parameters instead of pointers.

While translating for(;;)-loops with more than one induction variable to
Pascal "for to/downto do"-loops, the extra induction variables have to be
manually updated at the end of the loop and before "continue"-statements.


Legal issues
============

Copyright (C) 1996,1998 by Jacques Nomssi Nzali

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

[1] Thomas G. Lane, JPEG FAQ

      in comp.graphics.misc and related newsgroups

[2] Wallace, Gregory K.: The JPEG Still Picture Compression Standard

      ftp.uu.net, graphics/jpeg/wallace.ps.Z

[3] The Independent JPEG Group C library for JPEG encoding and decoding,
    rev 6b.

      ftp://ftp.uu.net/graphics/jpeg/

      or SimTel in msdos/graphics/

[4] JPEG implementation, written by the PVRG group at Stanford,
      ftp havefun.stanford.edu:/pub/jpeg/JPEGv1.2.tar.Z.

[5] PASJPEG.ZIP at NView ftp site

      ftp://druckfix.physik.tu-chemnitz.de/pub/nv/
      http://www.tu-chemnitz.de/~nomssi/pub/pasjpeg.zip

[6] The PasJPEG home page with links

      http://www.tu-chemnitz.de/~nomssi/pasjpeg.html
_____________________________________________________________________________