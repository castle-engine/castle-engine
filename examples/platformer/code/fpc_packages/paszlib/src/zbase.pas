unit ZBase;


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

{$I zconf.inc}

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
  than 64k bytes at a time (needed on systems with 16-bit integer). }

{ Maximum value for memLevel in deflateInit2 }
{$ifdef MAXSEG_64K}
  {$IFDEF TP}
  const
    MAX_MEM_LEVEL = 7;
    DEF_MEM_LEVEL = MAX_MEM_LEVEL;  { default memLevel }
  {$ELSE}
  const
    MAX_MEM_LEVEL = 8;
    DEF_MEM_LEVEL = MAX_MEM_LEVEL;  { default memLevel }
  {$ENDIF}
{$else}
const
  MAX_MEM_LEVEL = 9;
  DEF_MEM_LEVEL = 8; { if MAX_MEM_LEVEL > 8 }
{$endif}

{ Maximum value for windowBits in deflateInit2 and inflateInit2 }
const
{$IFDEF TP}
  MAX_WBITS = 14; { 16K LZ77 window }
  maxzbaseint = maxint;
{$ELSE}
  MAX_WBITS = 15; { 32K LZ77 window }
  maxzbaseint = maxlongint;
{$ENDIF}

{ default windowBits for decompression. MAX_WBITS is for compression only }
const
  DEF_WBITS = MAX_WBITS;


type  Pbytearray=^Tbytearray;
      Pwordarray=^Twordarray;
      Pcardinalarray=^Tcardinalarray;

      Tbytearray = array [0..maxzbaseint div sizeof(byte)-1] of byte;
      Twordarray = array [0..maxzbaseint div sizeof(word)-1] of word;
      Tintegerarray = array [0..maxzbaseint div sizeof(integer)-1] of integer;
      Tcardinalarray = array [0..maxzbaseint div sizeof(cardinal)-1] of cardinal;


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
    {pad : cardinal;}       { pad structure to a power of 2 (4 bytes for }
                      {  16-bit, 8 bytes for 32-bit integer's) }
    base : cardinal;      { literal, length base, or distance base }
                      { or table offset }
  End;

type
  huft_field = Array[0..(maxzbaseint div SizeOf(inflate_huft))-1] of inflate_huft;
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
    len : cardinal;
    sub : record                      { submode }
      Case Byte of
      0:(code : record                { if LEN or DIST, where in tree }
          tree : pInflate_huft;       { pointer into tree }
          need : cardinal;                { bits needed }
         end);
      1:(lit : cardinal);                 { if LIT, literal }
      2:(copy: record                 { if EXT or COPY, where and how much }
           get : cardinal;                { bits to get for extra }
           dist : cardinal;               { distance back to copy from }
         end);
    end;

    { mode independent information }
    lbits : Byte;                     { ltree bits decoded per branch }
    dbits : Byte;                     { dtree bits decoder per branch }
    ltree : pInflate_huft;            { literal/length/eob tree }
    dtree : pInflate_huft;            { distance tree }
  end;

type
  check_func = function(check : cardinal;
                        buf : Pbyte;
                        {const buf : array of byte;}
	                len : cardinal) : cardinal;
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
    0:(left : cardinal);              { if STORED, bytes left to copy }
    1:(trees : record             { if DTREE, decoding info for trees }
        table : cardinal;               { table lengths (14 bits) }
        index : cardinal;               { index into blens (or border) }
        blens : Pcardinalarray;         { bit lengths of codes }
        bb : cardinal;                  { bit length tree depth }
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
    bitk : cardinal;            { bits in bit buffer }
    bitb : cardinal;           { bit buffer }
    hufts : huft_ptr; {pInflate_huft;}  { single malloc for tree space }
    window : Pbyte;        { sliding window }
    zend : Pbyte;          { one byte after sliding window }
    read : Pbyte;          { window read pointer }
    write : Pbyte;         { window write pointer }
    checkfn : check_func;   { check function }
    check : cardinal;          { check on output }
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
       0:(method : cardinal);  { if FLAGS, method byte }
       1:(check : record   { if CHECK, check values to compare }
           was : cardinal;        { computed check value }
           need : cardinal;       { stream check value }
          end);
       2:(marker : cardinal);  { if BAD, inflateSync's marker bytes count }
     end;

     { mode independent information }
     nowrap : boolean;      { flag for no wrapper }
     wbits : cardinal;          { log2(window size)  (8..15, defaults to 15) }
     blocks : pInflate_blocks_state;    { current inflate_blocks state }
   end;

type
  z_streamp = ^z_stream;
  z_stream = record
    next_in : Pbyte;     { next input byte }
    avail_in : cardinal;      { number of bytes available at next_in }
    total_in : qword;     { total nb of input bytes read so far }

    next_out : Pbyte;    { next output byte should be put there }
    avail_out : cardinal;     { remaining free space at next_out }
    total_out : qword;    { total nb of bytes output so far }

    msg : string[255];         { last error message, '' if no error }
    state : pInternal_state; { not visible by applications }

    data_type : integer;      { best guess about the data type: ascii or binary }
    adler : cardinal;        { adler32 value of the uncompressed data }
    reserved : cardinal;     { reserved for future use }
  end;


{  The application must update next_in and avail_in when avail_in has
   dropped to zero. It must update next_out and avail_out when avail_out
   has dropped to zero. The application must initialize zalloc, zfree and
   opaque before calling the init function. All other fields are set by the
   compression library and must not be updated by the application.

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

  {$IFDEF GZIO}
var
  errno : integer;
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
{$ifdef MAX_MATCH_IS_258}
  MAX_MATCH = 258;
{$else}
  MAX_MATCH = ??;    { deliberate syntax error }
{$endif}

const
  PRESET_DICT = $20; { preset dictionary flag in zlib header }


  procedure Trace(x : string);
  procedure Tracev(x : string);
  procedure Tracevv(x : string);
  procedure Tracevvv(x : string);
  procedure Tracec(c : boolean; x : string);
  procedure Tracecv(c : boolean; x : string);

function zlibVersion : string;
{ The application can compare zlibVersion and ZLIB_VERSION for consistency.
  If the first character differs, the library code actually used is
  not compatible with the zlib.h header file used by the application.
  This check is automatically made by deflateInit and inflateInit. }

function zError(err : integer) : string;

const
  ZLIB_VERSION : string[10] = '1.1.2';

resourcestring Sneed_dict     = 'need dictionary';
               Sstream_end    = 'stream end';
               Sfile_error    = 'file error';
               Sstream_error  = 'stream error';
               Sdata_error    = 'data error';
               Smem_error     = 'insufficient memory';
               Sbuf_error     = 'buffer error';
               Sversion_error = 'incompatible version';

const
  z_verbose : longint = 1;

{$IFDEF ZLIB_DEBUG}
procedure z_error (m : string);
{$ENDIF}

implementation

function zError(err : integer) : string;

begin
  case err of
    Z_VERSION_ERROR:
      zerror:=Sversion_error;
    Z_BUF_ERROR:
      zerror:=Sbuf_error;
    Z_MEM_ERROR:
      zerror:=Smem_error;
    Z_DATA_ERROR:
      zerror:=Sdata_error;
    Z_STREAM_ERROR:
      zerror:=Sstream_error;
    Z_ERRNO:
      zerror:=Sfile_error;
    Z_OK:
      zerror:='';
    Z_STREAM_END:
      zerror:=Sstream_end;
    Z_NEED_DICT:
      zerror:=Sneed_dict;
    else
      str(err,zerror);
      zerror:='Unknown zlib error '+zerror;
  end;
end;

function zlibVersion : string;
begin
  zlibVersion := ZLIB_VERSION;
end;

procedure z_error (m : string);
begin
  WriteLn(output, m);
  Write('Zlib - Halt...');
  ReadLn;
  Halt(1);
end;

procedure Trace(x : string);
begin
  WriteLn(x);
end;

procedure Tracev(x : string);
begin
 if (z_verbose>0) then
   WriteLn(x);
end;

procedure Tracevv(x : string);
begin
  if (z_verbose>1) then
    WriteLn(x);
end;

procedure Tracevvv(x : string);
begin
  if (z_verbose>2) then
    WriteLn(x);
end;

procedure Tracec(c : boolean; x : string);
begin
  if (z_verbose>0) and (c) then
    WriteLn(x);
end;

procedure Tracecv(c : boolean; x : string);
begin
  if (z_verbose>1) and c then
    WriteLn(x);
end;

end.
