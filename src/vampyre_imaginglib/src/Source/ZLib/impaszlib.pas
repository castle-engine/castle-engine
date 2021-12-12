Unit impaszlib;


{ Original:
   zlib.h -- interface of the 'zlib' general purpose compression library
  version 1.1.0, Feb 24th, 1998

  Copyright (C) 1995-1998 Jean-loup Gailly and Mark Adler

  This software is provided 'as-is', without any express or implied
  warranty.  In no event will the authors be held liable for any damages
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

  Jean-loup Gailly        Mark Adler
  jloup@gzip.org          madler@alumni.caltech.edu


  The data format used by the zlib library is described by RFCs (Request for
  Comments) 1950 to 1952 in the files ftp://ds.internic.net/rfc/rfc1950.txt
  (zlib format), rfc1951.txt (deflate format) and rfc1952.txt (gzip format).


  Pascal tranlastion
  Copyright (C) 1998 by Jacques Nomssi Nzali
  For conditions of distribution and use, see copyright notice in readme.txt
}

interface

{$I imzconf.inc}

uses
  imzutil;

{ zconf.h -- configuration of the zlib compression library }
{ zutil.c -- target dependent utility functions for the compression library }

{ The 'zlib' compression library provides in-memory compression and
  decompression functions, including integrity checks of the uncompressed
  data.  This version of the library supports only one compression method
  (deflation) but other algorithms will be added later and will have the same
  stream interface.

     Compression can be done in a single step if the buffers are large
  enough (for example if an input file is mmap'ed), or can be done by
  repeated calls of the compression function.  In the latter case, the
  application must provide more input and/or consume the output
  (providing more output space) before each call.

     The library also supports reading and writing files in gzip (.gz) format
  with an interface similar to that of stdio.

     The library does not install any signal handler. The decoder checks
  the consistency of the compressed data, so the library should never
  crash even in case of corrupted input. }



{ Compile with -DMAXSEG_64K if the alloc function cannot allocate more
  than 64k bytes at a time (needed on systems with 16-bit int). }

{ Maximum value for memLevel in deflateInit2 }
const
  MAX_MEM_LEVEL = 9;
  DEF_MEM_LEVEL = 8; { if MAX_MEM_LEVEL > 8 }

{ Maximum value for windowBits in deflateInit2 and inflateInit2 }
const
  MAX_WBITS = 15; { 32K LZ77 window }

{ default windowBits for decompression. MAX_WBITS is for compression only }
const
  DEF_WBITS = MAX_WBITS;

{ The memory requirements for deflate are (in bytes):
            1 shl (windowBits+2)   +  1 shl (memLevel+9)
 that is: 128K for windowBits=15  +  128K for memLevel = 8  (default values)
 plus a few kilobytes for small objects. For example, if you want to reduce
 the default memory requirements from 256K to 128K, compile with
     DMAX_WBITS=14 DMAX_MEM_LEVEL=7
 Of course this will generally degrade compression (there's no free lunch).

 The memory requirements for inflate are (in bytes) 1 shl windowBits
 that is, 32K for windowBits=15 (default value) plus a few kilobytes
 for small objects. }


{ Huffman code lookup table entry--this entry is four bytes for machines
  that have 16-bit pointers (e.g. PC's in the small or medium model). }

type
  pInflate_huft = ^inflate_huft;
  inflate_huft = Record
    Exop,             { number of extra bits or operation }
    bits : Byte;      { number of bits in this code or subcode }
    {pad : uInt;}       { pad structure to a power of 2 (4 bytes for }
                      {  16-bit, 8 bytes for 32-bit int's) }
    base : uInt;      { literal, length base, or distance base }
                      { or table offset }
  End;

type
  huft_field = Array[0..(MaxInt div SizeOf(inflate_huft))-1] of inflate_huft;
  huft_ptr = ^huft_field;
type
  ppInflate_huft = ^pInflate_huft;

type
  inflate_codes_mode = ( { waiting for "i:"=input, "o:"=output, "x:"=nothing }
        START,    { x: set up for LEN }
        LEN,      { i: get length/literal/eob next }
        LENEXT,   { i: getting length extra (have base) }
        DIST,     { i: get distance next }
        DISTEXT,  { i: getting distance extra }
        COPY,     { o: copying bytes in window, waiting for space }
        LIT,      { o: got literal, waiting for output space }
        WASH,     { o: got eob, possibly still output waiting }
        ZEND,     { x: got eob and all data flushed }
        BADCODE); { x: got error }

{ inflate codes private state }
type
  pInflate_codes_state = ^inflate_codes_state;
  inflate_codes_state = record

    mode : inflate_codes_mode;        { current inflate_codes mode }

    { mode dependent information }
    len : uInt;
    sub : record                      { submode }
      Case Byte of
      0:(code : record                { if LEN or DIST, where in tree }
          tree : pInflate_huft;       { pointer into tree }
          need : uInt;                { bits needed }
         end);
      1:(lit : uInt);                 { if LIT, literal }
      2:(copy: record                 { if EXT or COPY, where and how much }
           get : uInt;                { bits to get for extra }
           dist : uInt;               { distance back to copy from }
         end);
    end;

    { mode independent information }
    lbits : Byte;                     { ltree bits decoded per branch }
    dbits : Byte;                     { dtree bits decoder per branch }
    ltree : pInflate_huft;            { literal/length/eob tree }
    dtree : pInflate_huft;            { distance tree }
  end;

type
  check_func = function(check : uLong;
                        buf : pBytef;
                        {const buf : array of byte;}
	                len : uInt) : uLong;
type
  inflate_block_mode =
     (ZTYPE,    { get type bits (3, including end bit) }
      LENS,     { get lengths for stored }
      STORED,   { processing stored block }
      TABLE,    { get table lengths }
      BTREE,    { get bit lengths tree for a dynamic block }
      DTREE,    { get length, distance trees for a dynamic block }
      CODES,    { processing fixed or dynamic block }
      DRY,      { output remaining window bytes }
      BLKDONE,  { finished last block, done }
      BLKBAD);  { got a data error--stuck here }

type
  pInflate_blocks_state = ^inflate_blocks_state;

{ inflate blocks semi-private state }
  inflate_blocks_state = record

    mode : inflate_block_mode;     { current inflate_block mode }

    { mode dependent information }
    sub : record                  { submode }
    case Byte of
    0:(left : uInt);              { if STORED, bytes left to copy }
    1:(trees : record             { if DTREE, decoding info for trees }
        table : uInt;               { table lengths (14 bits) }
        index : uInt;               { index into blens (or border) }
        blens : PuIntArray;         { bit lengths of codes }
        bb : uInt;                  { bit length tree depth }
        tb : pInflate_huft;         { bit length decoding tree }
      end);
    2:(decode : record            { if CODES, current state }
        tl : pInflate_huft;
        td : pInflate_huft;         { trees to free }
        codes : pInflate_codes_state;
      end);
    end;
    last : boolean;               { true if this block is the last block }

    { mode independent information }
    bitk : uInt;            { bits in bit buffer }
    bitb : uLong;           { bit buffer }
    hufts : huft_ptr; {pInflate_huft;}  { single malloc for tree space }
    window : pBytef;        { sliding window }
    zend : pBytef;          { one byte after sliding window }
    read : pBytef;          { window read pointer }
    write : pBytef;         { window write pointer }
    checkfn : check_func;   { check function }
    check : uLong;          { check on output }
  end;

type
  inflate_mode = (
      METHOD,   { waiting for method byte }
      FLAG,     { waiting for flag byte }
      DICT4,    { four dictionary check bytes to go }
      DICT3,    { three dictionary check bytes to go }
      DICT2,    { two dictionary check bytes to go }
      DICT1,    { one dictionary check byte to go }
      DICT0,    { waiting for inflateSetDictionary }
      BLOCKS,   { decompressing blocks }
      CHECK4,   { four check bytes to go }
      CHECK3,   { three check bytes to go }
      CHECK2,   { two check bytes to go }
      CHECK1,   { one check byte to go }
      DONE,     { finished check, done }
      BAD);     { got an error--stay here }

{ inflate private state }
type
  pInternal_state = ^internal_state; { or point to a deflate_state record }
  internal_state = record

     mode : inflate_mode;  { current inflate mode }

     { mode dependent information }
     sub : record          { submode }
       case byte of
       0:(method : uInt);  { if FLAGS, method byte }
       1:(check : record   { if CHECK, check values to compare }
           was : uLong;        { computed check value }
           need : uLong;       { stream check value }
          end);
       2:(marker : uInt);  { if BAD, inflateSync's marker bytes count }
     end;

     { mode independent information }
     nowrap : boolean;      { flag for no wrapper }
     wbits : uInt;          { log2(window size)  (8..15, defaults to 15) }
     blocks : pInflate_blocks_state;    { current inflate_blocks state }
   end;

type
  alloc_func = function(opaque : voidpf; items : uInt; size : uInt) : voidpf;
  free_func = procedure(opaque : voidpf; address : voidpf);

type
  z_streamp = ^z_stream;
  z_stream = record
    next_in : pBytef;     { next input byte }
    avail_in : uInt;      { number of bytes available at next_in }
    total_in : uLong;     { total nb of input bytes read so far }

    next_out : pBytef;    { next output byte should be put there }
    avail_out : uInt;     { remaining free space at next_out }
    total_out : uLong;    { total nb of bytes output so far }

    msg : string[255];         { last error message, '' if no error }
    state : pInternal_state; { not visible by applications }

    zalloc : alloc_func;  { used to allocate the internal state }
    zfree : free_func;    { used to free the internal state }
    opaque : voidpf;      { private data object passed to zalloc and zfree }

    data_type : int;      { best guess about the data type: ascii or binary }
    adler : uLong;        { adler32 value of the uncompressed data }
    reserved : uLong;     { reserved for future use }
  end;


{  The application must update next_in and avail_in when avail_in has
   dropped to zero. It must update next_out and avail_out when avail_out
   has dropped to zero. The application must initialize zalloc, zfree and
   opaque before calling the init function. All other fields are set by the
   compression library and must not be updated by the application.

   The opaque value provided by the application will be passed as the first
   parameter for calls of zalloc and zfree. This can be useful for custom
   memory management. The compression library attaches no meaning to the
   opaque value.

   zalloc must return Z_NULL if there is not enough memory for the object.
   On 16-bit systems, the functions zalloc and zfree must be able to allocate
   exactly 65536 bytes, but will not be required to allocate more than this
   if the symbol MAXSEG_64K is defined (see zconf.h). WARNING: On MSDOS,
   pointers returned by zalloc for objects of exactly 65536 bytes *must*
   have their offset normalized to zero. The default allocation function
   provided by this library ensures this (see zutil.c). To reduce memory
   requirements and avoid any allocation of 64K objects, at the expense of
   compression ratio, compile the library with -DMAX_WBITS=14 (see zconf.h).

   The fields total_in and total_out can be used for statistics or
   progress reports. After compression, total_in holds the total size of
   the uncompressed data and may be saved for use in the decompressor
   (particularly if the decompressor wants to decompress everything in
   a single step). }

const  { constants }
   Z_NO_FLUSH      = 0;
   Z_PARTIAL_FLUSH = 1;
   Z_SYNC_FLUSH    = 2;
   Z_FULL_FLUSH    = 3;
   Z_FINISH        = 4;
{ Allowed flush values; see deflate() below for details }

   Z_OK            = 0;
   Z_STREAM_END    = 1;
   Z_NEED_DICT     = 2;
   Z_ERRNO         = (-1);
   Z_STREAM_ERROR  = (-2);
   Z_DATA_ERROR    = (-3);
   Z_MEM_ERROR     = (-4);
   Z_BUF_ERROR     = (-5);
   Z_VERSION_ERROR = (-6);
{ Return codes for the compression/decompression functions. Negative
  values are errors, positive values are used for special but normal events.}

   Z_NO_COMPRESSION         = 0;
   Z_BEST_SPEED             = 1;
   Z_BEST_COMPRESSION       = 9;
   Z_DEFAULT_COMPRESSION    = (-1);
{ compression levels }

   Z_FILTERED            = 1;
   Z_HUFFMAN_ONLY        = 2;
   Z_DEFAULT_STRATEGY    = 0;
{ compression strategy; see deflateInit2() below for details }

   Z_BINARY   = 0;
   Z_ASCII    = 1;
   Z_UNKNOWN  = 2;
{ Possible values of the data_type field }

   Z_DEFLATED   = 8;
{ The deflate compression method (the only one supported in this version) }

   Z_NULL  = NIL;  { for initializing zalloc, zfree, opaque }

  {$IFDEF GZIO}
var
  errno : int;
  {$ENDIF}

        { common constants }


{ The three kinds of block type }
const
  STORED_BLOCK = 0;
  STATIC_TREES = 1;
  DYN_TREES = 2;
{ The minimum and maximum match lengths }
const
  MIN_MATCH = 3;
  MAX_MATCH = 258;

const
  PRESET_DICT = $20; { preset dictionary flag in zlib header }


  {$IFDEF DEBUG}
  procedure Assert(cond : boolean; msg : AnsiString);
  {$ENDIF}

  procedure Trace(x : AnsiString);
  procedure Tracev(x : AnsiString);
  procedure Tracevv(x : AnsiString);
  procedure Tracevvv(x : AnsiString);
  procedure Tracec(c : boolean; x : AnsiString);
  procedure Tracecv(c : boolean; x : AnsiString);

function zlibVersion : AnsiString;
{ The application can compare zlibVersion and ZLIB_VERSION for consistency.
  If the first character differs, the library code actually used is
  not compatible with the zlib.h header file used by the application.
  This check is automatically made by deflateInit and inflateInit. }

function zError(err : int) : AnsiString;
function ZALLOC (var strm : z_stream; items : uInt; size : uInt) : voidpf;
procedure ZFREE (var strm : z_stream; ptr : voidpf);
procedure TRY_FREE (var strm : z_stream; ptr : voidpf);

const
  ZLIB_VERSION : string[10] = '1.1.2';

const
  z_errbase = Z_NEED_DICT;
  z_errmsg : Array[0..9] of string[21] = { indexed by 2-zlib_error }
           ('need dictionary',     { Z_NEED_DICT       2  }
            'stream end',          { Z_STREAM_END      1  }
            '',                    { Z_OK              0  }
            'file error',          { Z_ERRNO         (-1) }
            'stream error',        { Z_STREAM_ERROR  (-2) }
            'data error',          { Z_DATA_ERROR    (-3) }
            'insufficient memory', { Z_MEM_ERROR     (-4) }
            'buffer error',        { Z_BUF_ERROR     (-5) }
            'incompatible version',{ Z_VERSION_ERROR (-6) }
            '');
const
  z_verbose : int = 1;

function deflateInit_(var Stream: z_stream; Level: LongInt; const Version: AnsiString;
  Stream_size: LongInt): LongInt;
function inflateInit_(var Stream: z_stream; const Version: AnsiString;
  Stream_size: Longint): LongInt;
  
{$IFDEF DEBUG}
procedure z_error (m : string);
{$ENDIF}

implementation

uses
  imzdeflate, imzinflate;

function deflateInit_(var Stream: z_stream; Level: LongInt; const Version: AnsiString;
  Stream_size: LongInt): LongInt;
begin
  Result := imzdeflate.deflateInit_(@Stream, Level, Version, Stream_size);
end;

function inflateInit_(var Stream: z_stream; const Version: AnsiString;
  Stream_size: Longint): LongInt;
begin
  Result := imzinflate.inflateInit_(@Stream, Version, Stream_size);
end;

function zError(err : int) : AnsiString;
begin
  zError := z_errmsg[Z_NEED_DICT-err];
end;

function zlibVersion : AnsiString;
begin
  zlibVersion := ZLIB_VERSION;
end;

procedure z_error (m : AnsiString);
begin
  WriteLn(output, m);
  Write('Zlib - Halt...');
  ReadLn;
  Halt(1);
end;

procedure Assert(cond : boolean; msg : AnsiString);
begin
  if not cond then
    z_error(msg);
end;

procedure Trace(x : AnsiString);
begin
  WriteLn(x);
end;

procedure Tracev(x : AnsiString);
begin
 if (z_verbose>0) then
   WriteLn(x);
end;

procedure Tracevv(x : AnsiString);
begin
  if (z_verbose>1) then
    WriteLn(x);
end;

procedure Tracevvv(x : AnsiString);
begin
  if (z_verbose>2) then
    WriteLn(x);
end;

procedure Tracec(c : boolean; x : AnsiString);
begin
  if (z_verbose>0) and (c) then
    WriteLn(x);
end;

procedure Tracecv(c : boolean; x : AnsiString);
begin
  if (z_verbose>1) and c then
    WriteLn(x);
end;

function ZALLOC (var strm : z_stream; items : uInt; size : uInt) : voidpf;
begin
  ZALLOC := strm.zalloc(strm.opaque, items, size);
end;

procedure ZFREE (var strm : z_stream; ptr : voidpf);
begin
  strm.zfree(strm.opaque, ptr);
end;

procedure TRY_FREE (var strm : z_stream; ptr : voidpf);
begin
  {if @strm <> Z_NULL then}
    strm.zfree(strm.opaque, ptr);
end;

end.
