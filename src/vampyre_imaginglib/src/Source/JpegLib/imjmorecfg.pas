unit imjmorecfg;

{ This file contains additional configuration options that customize the
  JPEG software for special applications or support machine-dependent
  optimizations.  Most users will not need to touch this file. }

{ Source: jmorecfg.h; Copyright (C) 1991-1996, Thomas G. Lane. }

interface

{$I imjconfig.inc}

type
  int = Integer;
  uInt = Cardinal;
  short = SmallInt;
  ushort = Word;
  long = LongInt;

type
  voidp = pointer;

type
  int_ptr = ^int;
  size_t = int;

{ Define BITS_IN_JSAMPLE as either
    8   for 8-bit sample values (the usual setting)
    12  for 12-bit sample values
  Only 8 and 12 are legal data precisions for lossy JPEG according to the
  JPEG standard, and the IJG code does not support anything else!
  We do not support run-time selection of data precision, sorry. }


{$ifdef BITS_IN_JSAMPLE_IS_8}   { use 8 or 12 }
const
  BITS_IN_JSAMPLE = 8;
{$else}
const
  BITS_IN_JSAMPLE = 12;
{$endif}


{ Maximum number of components (color channels) allowed in JPEG image.
  To meet the letter of the JPEG spec, set this to 255.  However, darn
  few applications need more than 4 channels (maybe 5 for CMYK + alpha
  mask).  We recommend 10 as a reasonable compromise; use 4 if you are
  really short on memory.  (Each allowed component costs a hundred or so
  bytes of storage, whether actually used in an image or not.) }


const
  MAX_COMPONENTS = 10;          { maximum number of image components }


{ Basic data types.
  You may need to change these if you have a machine with unusual data
  type sizes; for example, "char" not 8 bits, "short" not 16 bits,
  or "long" not 32 bits.  We don't care whether "int" is 16 or 32 bits,
  but it had better be at least 16. }


{ Representation of a single sample (pixel element value).
  We frequently allocate large arrays of these, so it's important to keep
  them small.  But if you have memory to burn and access to char or short
  arrays is very slow on your hardware, you might want to change these. }


{$ifdef BITS_IN_JSAMPLE_IS_8}
{ JSAMPLE should be the smallest type that will hold the values 0..255.
  You can use a signed char by having GETJSAMPLE mask it with $FF. }

{ CHAR_IS_UNSIGNED }
type
  JSAMPLE = byte; { Pascal unsigned char }
  GETJSAMPLE = int;

const
  MAXJSAMPLE = 255;
  CENTERJSAMPLE = 128;

{$endif}

{$ifndef BITS_IN_JSAMPLE_IS_8}
{ JSAMPLE should be the smallest type that will hold the values 0..4095.
  On nearly all machines "short" will do nicely. }

type
  JSAMPLE = short;
  GETJSAMPLE = int;

const
  MAXJSAMPLE = 4095;
  CENTERJSAMPLE = 2048;

{$endif} { BITS_IN_JSAMPLE = 12 }


{ Representation of a DCT frequency coefficient.
  This should be a signed value of at least 16 bits; "short" is usually OK.
  Again, we allocate large arrays of these, but you can change to int
  if you have memory to burn and "short" is really slow. }
type
  JCOEF = int;
  JCOEF_PTR = ^JCOEF;


{ Compressed datastreams are represented as arrays of JOCTET.
  These must be EXACTLY 8 bits wide, at least once they are written to
  external storage.  Note that when using the stdio data source/destination
  managers, this is also the data type passed to fread/fwrite. }


type
  JOCTET = Byte;
  jTOctet = 0..(MaxInt div SizeOf(JOCTET))-1;
  JOCTET_FIELD = array[jTOctet] of JOCTET;
  JOCTET_FIELD_PTR = ^JOCTET_FIELD;
  JOCTETPTR = ^JOCTET;

  GETJOCTET = JOCTET; { A work around }


{ These typedefs are used for various table entries and so forth.
  They must be at least as wide as specified; but making them too big
  won't cost a huge amount of memory, so we don't provide special
  extraction code like we did for JSAMPLE.  (In other words, these
  typedefs live at a different point on the speed/space tradeoff curve.) }


{ UINT8 must hold at least the values 0..255. }

type
  UINT8 = Byte;

{ UINT16 must hold at least the values 0..65535. }

  UINT16 = Word;

{ INT16 must hold at least the values -32768..32767. }

  INT16 = SmallInt;

{ INT32 must hold at least signed 32-bit values. }

  INT32 = LongInt;
type
  INT32PTR = ^INT32;

{ Datatype used for image dimensions.  The JPEG standard only supports
  images up to 64K*64K due to 16-bit fields in SOF markers.  Therefore
  "unsigned int" is sufficient on all machines.  However, if you need to
  handle larger images and you don't mind deviating from the spec, you
  can change this datatype. }

type
  JDIMENSION = uInt;

const
  JPEG_MAX_DIMENSION = 65500;  { a tad under 64K to prevent overflows }


{ Ordering of RGB data in scanlines passed to or from the application.
  If your application wants to deal with data in the order B,G,R, just
  change these macros.  You can also deal with formats such as R,G,B,X
  (one extra byte per pixel) by changing RGB_PIXELSIZE.  Note that changing
  the offsets will also change the order in which colormap data is organized.
  RESTRICTIONS:
  1. The sample applications cjpeg,djpeg do NOT support modified RGB formats.
  2. These macros only affect RGB<=>YCbCr color conversion, so they are not
     useful if you are using JPEG color spaces other than YCbCr or grayscale.
  3. The color quantizer modules will not behave desirably if RGB_PIXELSIZE
     is not 3 (they don't understand about dummy color components!).  So you
     can't use color quantization if you change that value. }

{$ifdef RGB_RED_IS_0}
const
  RGB_RED       = 0;    { Offset of Red in an RGB scanline element }
  RGB_GREEN     = 1;    { Offset of Green }
  RGB_BLUE      = 2;    { Offset of Blue }
{$else}
const
  RGB_RED       = 2;    { Offset of Red in an RGB scanline element }
  RGB_GREEN     = 1;    { Offset of Green }
  RGB_BLUE      = 0;    { Offset of Blue }
{$endif}

{$ifdef RGB_PIXELSIZE_IS_3}
const
  RGB_PIXELSIZE = 3;    { JSAMPLEs per RGB scanline element }
{$else}
const
  RGB_PIXELSIZE = ??;   { Nomssi: deliberate syntax error. Set this value }
{$endif}

{ Definitions for speed-related optimizations. }

{ On some machines (notably 68000 series) "int" is 32 bits, but multiplying
  two 16-bit shorts is faster than multiplying two ints.  Define MULTIPLIER
  as short on such a machine.  MULTIPLIER must be at least 16 bits wide. }
type
  MULTIPLIER = int;     { type for fastest integer multiply }


{ FAST_FLOAT should be either float or double, whichever is done faster
  by your compiler.  (Note that this type is only used in the floating point
  DCT routines, so it only matters if you've defined DCT_FLOAT_SUPPORTED.)
  Typically, float is faster in ANSI C compilers, while double is faster in
  pre-ANSI compilers (because they insist on converting to double anyway).
  The code below therefore chooses float if we have ANSI-style prototypes. }

type
  FAST_FLOAT = double; {float}


implementation


end.
