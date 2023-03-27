unit ZDeflate;

{$goto on}

{ Orginal: deflate.h -- internal compression state
           deflate.c -- compress data using the deflation algorithm
  Copyright (C) 1995-1996 Jean-loup Gailly.

  Pascal tranlastion
  Copyright (C) 1998 by Jacques Nomssi Nzali
  For conditions of distribution and use, see copyright notice in readme.txt
}


{  ALGORITHM

       The "deflation" process depends on being able to identify portions
       of the input text which are identical to earlier input (within a
       sliding window trailing behind the input currently being processed).

       The most straightforward technique turns out to be the fastest for
       most input files: try all possible matches and select the longest.
       The key feature of this algorithm is that insertions into the string
       dictionary are very simple and thus fast, and deletions are avoided
       completely. Insertions are performed at each input character, whereas
       string matches are performed only when the previous match ends. So it
       is preferable to spend more time in matches to allow very fast string
       insertions and avoid deletions. The matching algorithm for small
       strings is inspired from that of Rabin & Karp. A brute force approach
       is used to find longer strings when a small match has been found.
       A similar algorithm is used in comic (by Jan-Mark Wams) and freeze
       (by Leonid Broukhis).
          A previous version of this file used a more sophisticated algorithm
       (by Fiala and Greene) which is guaranteed to run in linear amortized
       time, but has a larger average cost, uses more memory and is patented.
       However the F&G algorithm may be faster for some highly redundant
       files if the parameter max_chain_length (described below) is too large.

   ACKNOWLEDGEMENTS

       The idea of lazy evaluation of matches is due to Jan-Mark Wams, and
       I found it in 'freeze' written by Leonid Broukhis.
       Thanks to many people for bug reports and testing.

   REFERENCES

       Deutsch, L.P.,"'Deflate' Compressed Data Format Specification".
       Available in ftp.uu.net:/pub/archiving/zip/doc/deflate-1.1.doc

       A description of the Rabin and Karp algorithm is given in the book
          "Algorithms" by R. Sedgewick, Addison-Wesley, p252.

       Fiala,E.R., and Greene,D.H.
          Data Compression with Finite Windows, Comm.ACM, 32,4 (1989) 490-595}

{ $Id: deflate.c,v 1.14 1996/07/02 12:40:55 me Exp $ }

interface

{$I zconf.inc}

uses
 zbase;


function deflateInit_(strm : z_streamp;
                      level : integer;
                      const version : string;
                      stream_size : integer) : integer;


function deflateInit (var strm : z_stream; level : integer) : integer;

{  Initializes the internal stream state for compression.

     The compression level must be Z_DEFAULT_COMPRESSION, or between 0 and 9:
   1 gives best speed, 9 gives best compression, 0 gives no compression at
   all (the input data is simply copied a block at a time).
   Z_DEFAULT_COMPRESSION requests a default compromise between speed and
   compression (currently equivalent to level 6).

     deflateInit returns Z_OK if success, Z_MEM_ERROR if there was not
   enough memory, Z_STREAM_ERROR if level is not a valid compression level,
   Z_VERSION_ERROR if the zlib library version (zlib_version) is incompatible
   with the version assumed by the caller (ZLIB_VERSION).
   msg is set to null if there is no error message.  deflateInit does not
   perform any compression: this will be done by deflate(). }


{EXPORT}
function deflate (var strm : z_stream; flush : integer) : integer;

{ Performs one or both of the following actions:

  - Compress more input starting at next_in and update next_in and avail_in
    accordingly. If not all input can be processed (because there is not
    enough room in the output buffer), next_in and avail_in are updated and
    processing will resume at this point for the next call of deflate().

  - Provide more output starting at next_out and update next_out and avail_out
    accordingly. This action is forced if the parameter flush is non zero.
    Forcing flush frequently degrades the compression ratio, so this parameter
    should be set only when necessary (in interactive applications).
    Some output may be provided even if flush is not set.

  Before the call of deflate(), the application should ensure that at least
  one of the actions is possible, by providing more input and/or consuming
  more output, and updating avail_in or avail_out accordingly; avail_out
  should never be zero before the call. The application can consume the
  compressed output when it wants, for example when the output buffer is full
  (avail_out == 0), or after each call of deflate(). If deflate returns Z_OK
  and with zero avail_out, it must be called again after making room in the
  output buffer because there might be more output pending.

    If the parameter flush is set to Z_PARTIAL_FLUSH, the current compression
  block is terminated and flushed to the output buffer so that the
  decompressor can get all input data available so far. For method 9, a future
  variant on method 8, the current block will be flushed but not terminated.
  Z_SYNC_FLUSH has the same effect as partial flush except that the compressed
  output is byte aligned (the compressor can clear its internal bit buffer)
  and the current block is always terminated; this can be useful if the
  compressor has to be restarted from scratch after an interruption (in which
  case the internal state of the compressor may be lost).
    If flush is set to Z_FULL_FLUSH, the compression block is terminated, a
  special marker is output and the compression dictionary is discarded; this
  is useful to allow the decompressor to synchronize if one compressed block
  has been damaged (see inflateSync below).  Flushing degrades compression and
  so should be used only when necessary.  Using Z_FULL_FLUSH too often can
  seriously degrade the compression. If deflate returns with avail_out == 0,
  this function must be called again with the same value of the flush
  parameter and more output space (updated avail_out), until the flush is
  complete (deflate returns with non-zero avail_out).

    If the parameter flush is set to Z_FINISH, all pending input is processed,
  all pending output is flushed and deflate returns with Z_STREAM_END if there
  was enough output space; if deflate returns with Z_OK, this function must be
  called again with Z_FINISH and more output space (updated avail_out) but no
  more input data, until it returns with Z_STREAM_END or an error. After
  deflate has returned Z_STREAM_END, the only possible operations on the
  stream are deflateReset or deflateEnd.

    Z_FINISH can be used immediately after deflateInit if all the compression
  is to be done in a single step. In this case, avail_out must be at least
  0.1% larger than avail_in plus 12 bytes.  If deflate does not return
  Z_STREAM_END, then it must be called again as described above.

    deflate() may update data_type if it can make a good guess about
  the input data type (Z_ASCII or Z_BINARY). In doubt, the data is considered
  binary. This field is only for information purposes and does not affect
  the compression algorithm in any manner.

    deflate() returns Z_OK if some progress has been made (more input
  processed or more output produced), Z_STREAM_END if all input has been
  consumed and all output has been produced (only when flush is set to
  Z_FINISH), Z_STREAM_ERROR if the stream state was inconsistent (for example
  if next_in or next_out was NULL), Z_BUF_ERROR if no progress is possible. }


function deflateEnd (var strm : z_stream) : integer;

{     All dynamically allocated data structures for this stream are freed.
   This function discards any unprocessed input and does not flush any
   pending output.

     deflateEnd returns Z_OK if success, Z_STREAM_ERROR if the
   stream state was inconsistent, Z_DATA_ERROR if the stream was freed
   prematurely (some input or output was discarded). In the error case,
   msg may be set but then points to a static string (which must not be
   deallocated). }




                        { Advanced functions }

{ The following functions are needed only in some special applications. }


{EXPORT}
function deflateInit2 (var strm : z_stream;
                       level : integer;
                       method : integer;
                       windowBits : integer;
                       memLevel : integer;
                       strategy : integer) : integer;
function deflateInit2_(var strm : z_stream;
                       level : integer;
                       method : integer;
                       windowBits : integer;
                       memLevel : integer;
                       strategy : integer;
                       const version : string;
                       stream_size : integer) : integer;

{  This is another version of deflateInit with more compression options. The
   fields next_in, and opaque must be initialized before by
   the caller.

     The method parameter is the compression method. It must be Z_DEFLATED in
   this version of the library. (Method 9 will allow a 64K history buffer and
   partial block flushes.)

     The windowBits parameter is the base two logarithm of the window size
   (the size of the history buffer).  It should be in the range 8..15 for this
   version of the library (the value 16 will be allowed for method 9). Larger
   values of this parameter result in better compression at the expense of
   memory usage. The default value is 15 if deflateInit is used instead.

     The memLevel parameter specifies how much memory should be allocated
   for the internal compression state. memLevel=1 uses minimum memory but
   is slow and reduces compression ratio; memLevel=9 uses maximum memory
   for optimal speed. The default value is 8. See zconf.h for total memory
   usage as a function of windowBits and memLevel.

     The strategy parameter is used to tune the compression algorithm. Use the
   value Z_DEFAULT_STRATEGY for normal data, Z_FILTERED for data produced by a
   filter (or predictor), or Z_HUFFMAN_ONLY to force Huffman encoding only (no
   string match).  Filtered data consists mostly of small values with a
   somewhat random distribution. In this case, the compression algorithm is
   tuned to compress them better. The effect of Z_FILTERED is to force more
   Huffman coding and less string matching; it is somewhat intermediate
   between Z_DEFAULT and Z_HUFFMAN_ONLY. The strategy parameter only affects
   the compression ratio but not the correctness of the compressed output even
   if it is not set appropriately.

     If next_in is not null, the library will use this buffer to hold also
   some history information; the buffer must either hold the entire input
   data, or have at least 1<<(windowBits+1) bytes and be writable. If next_in
   is null, the library will allocate its own history buffer (and leave next_in
   null). next_out need not be provided here but must be provided by the
   application for the next call of deflate().

     If the history buffer is provided by the application, next_in must
   must never be changed by the application since the compressor maintains
   information inside this buffer from call to call; the application
   must provide more input only by increasing avail_in. next_in is always
   reset by the library in this case.

      deflateInit2 returns Z_OK if success, Z_MEM_ERROR if there was
   not enough memory, Z_STREAM_ERROR if a parameter is invalid (such as
   an invalid method). msg is set to null if there is no error message.
   deflateInit2 does not perform any compression: this will be done by
   deflate(). }


{EXPORT}
function deflateSetDictionary (var strm : z_stream;
                               dictionary : Pbyte; {const bytes}
			       dictLength : cardinal) : integer;

{    Initializes the compression dictionary (history buffer) from the given
   byte sequence without producing any compressed output. This function must
   be called immediately after deflateInit or deflateInit2, before any call
   of deflate. The compressor and decompressor must use exactly the same
   dictionary (see inflateSetDictionary).
     The dictionary should consist of strings (byte sequences) that are likely
   to be encountered later in the data to be compressed, with the most commonly
   used strings preferably put towards the end of the dictionary. Using a
   dictionary is most useful when the data to be compressed is short and
   can be predicted with good accuracy; the data can then be compressed better
   than with the default empty dictionary. In this version of the library,
   only the last 32K bytes of the dictionary are used.
     Upon return of this function, strm->adler is set to the Adler32 value
   of the dictionary; the decompressor may later use this value to determine
   which dictionary has been used by the compressor. (The Adler32 value
   applies to the whole dictionary even if only a subset of the dictionary is
   actually used by the compressor.)

     deflateSetDictionary returns Z_OK if success, or Z_STREAM_ERROR if a
   parameter is invalid (such as NULL dictionary) or the stream state
   is inconsistent (for example if deflate has already been called for this
   stream). deflateSetDictionary does not perform any compression: this will
   be done by deflate(). }

{EXPORT}
function deflateCopy (dest : z_streamp;
                      source : z_streamp) : integer;

{  Sets the destination stream as a complete copy of the source stream.  If
   the source stream is using an application-supplied history buffer, a new
   buffer is allocated for the destination stream.  The compressed output
   buffer is always application-supplied. It's the responsibility of the
   application to provide the correct values of next_out and avail_out for the
   next call of deflate.

     This function can be useful when several compression strategies will be
   tried, for example when there are several ways of pre-processing the input
   data with a filter. The streams that will be discarded should then be freed
   by calling deflateEnd.  Note that deflateCopy duplicates the internal
   compression state which can be quite large, so this strategy is slow and
   can consume lots of memory.

     deflateCopy returns Z_OK if success, Z_MEM_ERROR if there was not
   enough memory, Z_STREAM_ERROR if the source stream state was inconsistent
   (such as getmem returns nil). msg is left unchanged in both source and
   destination. }

{EXPORT}
function deflateReset (var strm : z_stream) : integer;

{   This function is equivalent to deflateEnd followed by deflateInit,
   but does not free and reallocate all the internal compression state.
   The stream will keep the same compression level and any other attributes
   that may have been set by deflateInit2.

      deflateReset returns Z_OK if success, or Z_STREAM_ERROR if the source
   stream state was inconsistent (such as getmem or state being NIL). }


{EXPORT}
function deflateParams (var strm : z_stream; level : integer; strategy : integer) : integer;

{    Dynamically update the compression level and compression strategy.
   This can be used to switch between compression and straight copy of
   the input data, or to switch to a different kind of input data requiring
   a different strategy. If the compression level is changed, the input
   available so far is compressed with the old level (and may be flushed);
   the new level will take effect only at the next call of deflate().

     Before the call of deflateParams, the stream state must be set as for
   a call of deflate(), since the currently available input may have to
   be compressed and flushed. In particular, strm->avail_out must be non-zero.

     deflateParams returns Z_OK if success, Z_STREAM_ERROR if the source
   stream state was inconsistent or if a parameter was invalid, Z_BUF_ERROR
   if strm->avail_out was zero. }


const
   deflate_copyright : string = ' deflate 1.1.2 Copyright 1995-1998 Jean-loup Gailly ';

{ If you use the zlib library in a product, an acknowledgment is welcome
  in the documentation of your product. If for some reason you cannot
  include such an acknowledgment, I would appreciate that you keep this
  copyright string in the executable of your product. }

implementation

uses
  trees, adler;

{  ===========================================================================
   Function prototypes. }

type
   block_state = (
    need_more,      { block not completed, need more input or more output }
    block_done,     { block flush performed }
    finish_started, { finish started, need only more output at next deflate }
    finish_done);   { finish done, accept no more input or output }

{ Compression function. Returns the block state after the call. }
type
  compress_func = function(var s : deflate_state; flush : integer) : block_state;

{local}
procedure fill_window(var s : deflate_state); forward;
{local}
function deflate_stored(var s : deflate_state; flush : integer) : block_state; far; forward;
{local}
function deflate_fast(var s : deflate_state; flush : integer) : block_state; far; forward;
{local}
function deflate_slow(var s : deflate_state; flush : integer) : block_state; far; forward;
{local}
procedure lm_init(var s : deflate_state); forward;

{local}
procedure putShortMSB(var s : deflate_state; b : cardinal); forward;
{local}
procedure  flush_pending (var strm : z_stream); forward;
{local}
function read_buf(strm : z_streamp;
                  buf : Pbyte;
                  size : cardinal) : cardinal; forward;
{$ifdef ASMV}
procedure match_init; { asm code initialization }
function longest_match(var deflate_state; cur_match : IPos) : cardinal; forward;
{$else}
{local}
function longest_match(var s : deflate_state; cur_match : IPos) : cardinal;
  forward;
{$endif}

{$ifdef ZLIB_DEBUG}
{local}
procedure check_match(var s : deflate_state;
                      start, match : IPos;
                      length : integer); forward;
{$endif}

{  ==========================================================================
  local data }

const
  ZNIL = 0;
{ Tail of hash chains }

const
  TOO_FAR = 4096;
{ Matches of length 3 are discarded if their distance exceeds TOO_FAR }

const
  MIN_LOOKAHEAD = (MAX_MATCH+MIN_MATCH+1);
{ Minimum amount of lookahead, except at the end of the input file.
  See deflate.c for comments about the MIN_MATCH+1. }

{macro MAX_DIST(var s : deflate_state) : cardinal;
begin
  MAX_DIST := (s.w_size - MIN_LOOKAHEAD);
end;
  In order to simplify the code, particularly on 16 bit machines, match
  distances are limited to MAX_DIST instead of WSIZE. }


{ Values for max_lazy_match, good_match and max_chain_length, depending on
  the desired pack level (0..9). The values given below have been tuned to
  exclude worst case performance for pathological files. Better values may be
  found for specific files. }

type
  config = record
   good_length : word; { reduce lazy search above this match length }
   max_lazy : word;    { do not perform lazy search above this match length }
   nice_length : word; { quit search above this match length }
   max_chain : word;
   func : compress_func;
  end;

{local}
const
  configuration_table : array[0..10-1] of config = (
{      good lazy nice chain }
{0} (good_length:0;  max_lazy:0;   nice_length:0;   max_chain:0;    func:@deflate_stored),  { store only }
{1} (good_length:4;  max_lazy:4;   nice_length:8;   max_chain:4;    func:@deflate_fast), { maximum speed, no lazy matches }
{2} (good_length:4;  max_lazy:5;   nice_length:16;  max_chain:8;    func:@deflate_fast),
{3} (good_length:4;  max_lazy:6;   nice_length:32;  max_chain:32;   func:@deflate_fast),

{4} (good_length:4;  max_lazy:4;   nice_length:16;  max_chain:16;   func:@deflate_slow),  { lazy matches }
{5} (good_length:8;  max_lazy:16;  nice_length:32;  max_chain:32;   func:@deflate_slow),
{6} (good_length:8;  max_lazy:16;  nice_length:128; max_chain:128;  func:@deflate_slow),
{7} (good_length:8;  max_lazy:32;  nice_length:128; max_chain:256;  func:@deflate_slow),
{8} (good_length:32; max_lazy:128; nice_length:258; max_chain:1024; func:@deflate_slow),
{9} (good_length:32; max_lazy:258; nice_length:258; max_chain:4096; func:@deflate_slow)); { maximum compression }

{ Note: the deflate() code requires max_lazy >= MIN_MATCH and max_chain >= 4
  For deflate_fast() (levels <= 3) good is ignored and lazy has a different
  meaning. }

const
  EQUAL = 0;
{ result of memcmp for equal strings }

{ ==========================================================================
  Update a hash value with the given input byte
  IN  assertion: all calls to to UPDATE_HASH are made with consecutive
     input characters, so that a running hash key can be computed from the
     previous key instead of complete recalculation each time.

macro UPDATE_HASH(s,h,c)
   h := (( (h) shl s^.hash_shift) xor (c)) and s^.hash_mask;
}

{ ===========================================================================
  Insert string str in the dictionary and set match_head to the previous head
  of the hash chain (the most recent string with same hash key). Return
  the previous length of the hash chain.
  If this file is compiled with -DFASTEST, the compression level is forced
  to 1, and no hash chains are maintained.
  IN  assertion: all calls to to INSERT_STRING are made with consecutive
     input characters and the first MIN_MATCH bytes of str are valid
     (except for the last MIN_MATCH-1 bytes of the input file). }

procedure INSERT_STRING(var s : deflate_state;
                        str : cardinal;
                        var match_head : IPos);
begin
{$ifdef FASTEST}
   {UPDATE_HASH(s, s.ins_h, s.window[(str) + (MIN_MATCH-1)])}
    s.ins_h := ((s.ins_h shl s.hash_shift) xor
                 (s.window^[(str) + (MIN_MATCH-1)])) and s.hash_mask;
    match_head := s.head[s.ins_h]
    s.head[s.ins_h] := Pos(str);
{$else}
   {UPDATE_HASH(s, s.ins_h, s.window[(str) + (MIN_MATCH-1)])}
    s.ins_h := ((s.ins_h shl s.hash_shift) xor
                 (s.window^[(str) + (MIN_MATCH-1)])) and s.hash_mask;

    match_head := s.head^[s.ins_h];
    s.prev^[(str) and s.w_mask] := match_head;
    s.head^[s.ins_h] := Pos(str);
{$endif}
end;

{  =========================================================================
  Initialize the hash table (avoiding 64K overflow for 16 bit systems).
  prev[] will be initialized on the fly.

macro CLEAR_HASH(s)
    s^.head[s^.hash_size-1] := ZNIL;
    zmemzero(Pbyte(s^.head), cardinal(s^.hash_size-1)*sizeof(s^.head^[0]));
}

{  ======================================================================== }

function deflateInit2_(var strm : z_stream;
                       level : integer;
                       method : integer;
                       windowBits : integer;
                       memLevel : integer;
                       strategy : integer;
                       const version : string;
                       stream_size : integer) : integer;
var
  s : deflate_state_ptr;
  noheader : integer;

  overlay : Pwordarray;
  { We overlay pending_buf and d_buf+l_buf. This works since the average
    output size for (length,distance) codes is <= 24 bits. }
begin
  noheader := 0;
  if (version  =  '') or (version[1] <> ZLIB_VERSION[1]) or
     (stream_size <> sizeof(z_stream)) then
  begin
    deflateInit2_ := Z_VERSION_ERROR;
    exit;
  end;
  {
  if strm=nil then
  begin
    deflateInit2_ := Z_STREAM_ERROR;
    exit;
  end;
  }
  { SetLength(strm.msg, 255); }
  strm.msg := '';

  if (level  =  Z_DEFAULT_COMPRESSION) then
    level := 6;
{$ifdef FASTEST}
    level := 1;
{$endif}

  if (windowBits < 0) then { undocumented feature: suppress zlib header }
  begin
    noheader := 1;
    windowBits := -windowBits;
  end;
  if (memLevel < 1) or (memLevel > MAX_MEM_LEVEL) or (method <> Z_DEFLATED)
    or (windowBits < 8) or (windowBits > 15) or (level < 0)
    or (level > 9) or (strategy < 0) or (strategy > Z_HUFFMAN_ONLY) then
  begin
    deflateInit2_ := Z_STREAM_ERROR;
    exit;
  end;

  getmem(s,sizeof(deflate_state));
  if (s = nil) then
  begin
    deflateInit2_ := Z_MEM_ERROR;
    exit;
  end;
  strm.state := pInternal_state(s);
  s^.strm := @strm;

  s^.noheader := noheader;
  s^.w_bits := windowBits;
  s^.w_size := 1 shl s^.w_bits;
  s^.w_mask := s^.w_size - 1;

  s^.hash_bits := memLevel + 7;
  s^.hash_size := 1 shl s^.hash_bits;
  s^.hash_mask := s^.hash_size - 1;
  s^.hash_shift :=  ((s^.hash_bits+MIN_MATCH-1) div MIN_MATCH);

  getmem(s^.window,s^.w_size*2*sizeof(byte));
  getmem(s^.prev,s^.w_size*sizeof(pos));
  getmem(s^.head,s^.hash_size*sizeof(pos));

  s^.lit_bufsize := 1 shl (memLevel + 6); { 16K elements by default }

  getmem(overlay,s^.lit_bufsize*(sizeof(word)+2));
  s^.pending_buf := Pbytearray(overlay);
  s^.pending_buf_size := longint(s^.lit_bufsize) * (sizeof(word)+longint(2));

  if (s^.window=nil) or (s^.prev=nil) or (s^.head=nil) or
     (s^.pending_buf=nil) then
  begin
    {ERR_MSG(Z_MEM_ERROR);}
    strm.msg := zerror(Z_MEM_ERROR);
    deflateEnd (strm);
    deflateInit2_ := Z_MEM_ERROR;
    exit;
  end;
  s^.d_buf := Pwordarray( @overlay^[s^.lit_bufsize div sizeof(word)] );
  s^.l_buf := Pbytearray( @s^.pending_buf^[(1+sizeof(word))*s^.lit_bufsize] );

  s^.level := level;
  s^.strategy := strategy;
  s^.method := Byte(method);

  deflateInit2_ := deflateReset(strm);
end;

{  ========================================================================= }

function deflateInit2(var strm : z_stream;
                      level : integer;
                      method : integer;
                      windowBits : integer;
                      memLevel : integer;
                      strategy : integer) : integer;
{ a macro }
begin
  deflateInit2 := deflateInit2_(strm, level, method, windowBits,
                   memLevel, strategy, ZLIB_VERSION, sizeof(z_stream));
end;

{  ========================================================================= }

function deflateInit_(strm : z_streamp;
                      level : integer;
                      const version : string;
                      stream_size : integer) : integer;
begin
  if strm=nil then
    deflateInit_ := Z_STREAM_ERROR
  else
    deflateInit_ := deflateInit2_(strm^, level, Z_DEFLATED, MAX_WBITS,
                   DEF_MEM_LEVEL, Z_DEFAULT_STRATEGY, version, stream_size);
  { To do: ignore strm^.next_in if we use it as window }
end;

{  ========================================================================= }

function deflateInit(var strm : z_stream; level : integer) : integer;
{ deflateInit is a macro to allow checking the zlib version
  and the compiler's view of z_stream: }
begin
  deflateInit := deflateInit2_(strm, level, Z_DEFLATED, MAX_WBITS,
         DEF_MEM_LEVEL, Z_DEFAULT_STRATEGY, ZLIB_VERSION, sizeof(z_stream));
end;

{  ======================================================================== }
function deflateSetDictionary (var strm : z_stream;
                               dictionary : Pbyte;
                               dictLength : cardinal) : integer;
var
  s : deflate_state_ptr;
  length : cardinal;
  n : cardinal;
  hash_head : IPos;
var
  MAX_DIST : cardinal;  {macro}
begin
  length := dictLength;
  hash_head := 0;

  if {(@strm=nil) or}
     (strm.state=nil) or (dictionary=nil)
    or (deflate_state_ptr(strm.state)^.status<>INIT_STATE) then
  begin
    deflateSetDictionary := Z_STREAM_ERROR;
    exit;
  end;

  s := deflate_state_ptr(strm.state);
  strm.adler := adler32(strm.adler, dictionary, dictLength);

  if (length < MIN_MATCH) then
  begin
    deflateSetDictionary := Z_OK;
    exit;
  end;
  MAX_DIST := (s^.w_size - MIN_LOOKAHEAD);
  if (length > MAX_DIST) then
  begin
    length := MAX_DIST;
{$ifndef USE_DICT_HEAD}
    inc(dictionary, dictLength - length);  { use the tail of the dictionary }
{$endif}
  end;

  move(dictionary^,Pbyte(s^.window)^,length);
  s^.strstart := length;
  s^.block_start := longint(length);

  { Insert all strings in the hash table (except for the last two bytes).
    s^.lookahead stays null, so s^.ins_h will be recomputed at the next
    call of fill_window. }

  s^.ins_h := s^.window^[0];
  {UPDATE_HASH(s, s^.ins_h, s^.window[1]);}
  s^.ins_h := ((s^.ins_h shl s^.hash_shift) xor (s^.window^[1]))
              and s^.hash_mask;

  for n := 0 to length - MIN_MATCH do
    INSERT_STRING(s^, n, hash_head);
  {if (hash_head <> 0) then
    hash_head := 0;  - to make compiler happy }
  deflateSetDictionary := Z_OK;
end;

{  ======================================================================== }
function deflateReset (var strm : z_stream) : integer;
var
  s : deflate_state_ptr;
begin
  if {(@strm=nil) or}
   (strm.state=nil) then
  begin
    deflateReset := Z_STREAM_ERROR;
    exit;
  end;

  strm.total_out := 0;
  strm.total_in := 0;
  strm.msg := '';      { use freemem if we ever allocate msg dynamically }
  strm.data_type := Z_UNKNOWN;

  s := deflate_state_ptr(strm.state);
  s^.pending := 0;
  s^.pending_out := Pbyte(s^.pending_buf);

  if (s^.noheader < 0) then
  begin
    s^.noheader := 0; { was set to -1 by deflate(..., Z_FINISH); }
  end;
  if s^.noheader <> 0 then
    s^.status := BUSY_STATE
  else
    s^.status := INIT_STATE;
  strm.adler := 1;
  s^.last_flush := Z_NO_FLUSH;

  _tr_init(s^);
  lm_init(s^);

  deflateReset := Z_OK;
end;

{  ======================================================================== }
function deflateParams(var strm : z_stream;
                       level : integer;
                       strategy : integer) : integer;
var
  s : deflate_state_ptr;
  func : compress_func;
  err : integer;
begin
  err := Z_OK;
  if {(@strm=nil) or} (strm.state=nil) then
  begin
    deflateParams := Z_STREAM_ERROR;
    exit;
  end;

  s := deflate_state_ptr(strm.state);

  if (level = Z_DEFAULT_COMPRESSION) then
  begin
    level := 6;
  end;
  if (level < 0) or (level > 9) or (strategy < 0)
  or (strategy > Z_HUFFMAN_ONLY) then
  begin
    deflateParams := Z_STREAM_ERROR;
    exit;
  end;
  func := configuration_table[s^.level].func;

  if (@func <> @configuration_table[level].func)
    and (strm.total_in <> 0) then
  begin
      { Flush the last buffer: }
      err := deflate(strm, Z_PARTIAL_FLUSH);
  end;
  if (s^.level <> level) then
  begin
    s^.level := level;
    s^.max_lazy_match   := configuration_table[level].max_lazy;
    s^.good_match       := configuration_table[level].good_length;
    s^.nice_match       := configuration_table[level].nice_length;
    s^.max_chain_length := configuration_table[level].max_chain;
  end;
  s^.strategy := strategy;
  deflateParams := err;
end;

{ =========================================================================
  Put a short in the pending buffer. The 16-bit value is put in MSB order.
  IN assertion: the stream state is correct and there is enough room in
  pending_buf. }

{local}
procedure putShortMSB (var s : deflate_state; b : cardinal);
begin
  s.pending_buf^[s.pending] := Byte(b shr 8);
  inc(s.pending);
  s.pending_buf^[s.pending] := Byte(b and $ff);
  inc(s.pending);
end;

{ =========================================================================
  Flush as much pending output as possible. All deflate() output goes
  through this function so some applications may wish to modify it
  to avoid allocating a large strm^.next_out buffer and copying into it.
  (See also read_buf()). }

{local}
procedure flush_pending(var strm : z_stream);
var
  len : cardinal;
  s : deflate_state_ptr;
begin
  s := deflate_state_ptr(strm.state);
  len := s^.pending;

  if (len > strm.avail_out) then
    len := strm.avail_out;
  if (len = 0) then
    exit;

  move(s^.pending_out^,strm.next_out^,len);
  inc(strm.next_out, len);
  inc(s^.pending_out, len);
  inc(strm.total_out, len);
  dec(strm.avail_out, len);
  dec(s^.pending, len);
  if (s^.pending = 0) then
  begin
    s^.pending_out := Pbyte(s^.pending_buf);
  end;
end;

{ ========================================================================= }
function deflate (var strm : z_stream; flush : integer) : integer;
var
  old_flush : integer; { value of flush param for previous deflate call }
  s : deflate_state_ptr;
var
  header : cardinal;
  level_flags : cardinal;
var
  bstate : block_state;
begin
  if {(@strm=nil) or} (strm.state=nil)
    or (flush > Z_FINISH) or (flush < 0) then
  begin
    deflate := Z_STREAM_ERROR;
    exit;
  end;
  s := deflate_state_ptr(strm.state);

  if (strm.next_out=nil) or
     ((strm.next_in=nil) and (strm.avail_in<>0)) or
     ((s^.status=FINISH_STATE) and (flush<>Z_FINISH)) then
  begin
    {ERR_RETURN(strm^, Z_STREAM_ERROR);}
    strm.msg := zerror(Z_STREAM_ERROR);
    deflate := Z_STREAM_ERROR;
    exit;
  end;
  if (strm.avail_out = 0) then
  begin
    {ERR_RETURN(strm^, Z_BUF_ERROR);}
    strm.msg := zerror(Z_BUF_ERROR);
    deflate := Z_BUF_ERROR;
    exit;
  end;

  s^.strm := @strm; { just in case }
  old_flush := s^.last_flush;
  s^.last_flush := flush;

  { Write the zlib header }
  if (s^.status = INIT_STATE) then
  begin

    header := (Z_DEFLATED + ((s^.w_bits-8) shl 4)) shl 8;
    level_flags := (s^.level-1) shr 1;

    if (level_flags > 3) then
      level_flags := 3;
    header := header or (level_flags shl 6);
    if (s^.strstart <> 0) then
      header := header or PRESET_DICT;
    inc(header, 31 - (header mod 31));

    s^.status := BUSY_STATE;
    putShortMSB(s^, header);

    { Save the adler32 of the preset dictionary: }
    if (s^.strstart <> 0) then
    begin
      putShortMSB(s^, cardinal(strm.adler shr 16));
      putShortMSB(s^, cardinal(strm.adler and $ffff));
    end;
    strm.adler := longint(1);
  end;

  { Flush as much pending output as possible }
  if (s^.pending <> 0) then
  begin
    flush_pending(strm);
    if (strm.avail_out = 0) then
    begin
      { Since avail_out is 0, deflate will be called again with
	more output space, but possibly with both pending and
	avail_in equal to zero. There won't be anything to do,
	but this is not an error situation so make sure we
	return OK instead of BUF_ERROR at next call of deflate: }

      s^.last_flush := -1;
      deflate := Z_OK;
      exit;
    end;

  { Make sure there is something to do and avoid duplicate consecutive
    flushes. For repeated and useless calls with Z_FINISH, we keep
    returning Z_STREAM_END instead of Z_BUFF_ERROR. }

  end
  else
    if (strm.avail_in = 0) and (flush <= old_flush)
      and (flush <> Z_FINISH) then
    begin
      {ERR_RETURN(strm^, Z_BUF_ERROR);}
      strm.msg := zerror(Z_BUF_ERROR);
      deflate := Z_BUF_ERROR;
      exit;
    end;

  { User must not provide more input after the first FINISH: }
  if (s^.status = FINISH_STATE) and (strm.avail_in <> 0) then
  begin
    {ERR_RETURN(strm^, Z_BUF_ERROR);}
    strm.msg := zerror(Z_BUF_ERROR);
    deflate := Z_BUF_ERROR;
    exit;
  end;

  { Start a new block or continue the current one. }
  if (strm.avail_in <> 0) or (s^.lookahead <> 0)
    or ((flush <> Z_NO_FLUSH) and (s^.status <> FINISH_STATE)) then
  begin
    bstate := configuration_table[s^.level].func(s^, flush);

    if (bstate = finish_started) or (bstate = finish_done) then
      s^.status := FINISH_STATE;

    if (bstate = need_more) or (bstate = finish_started) then
    begin
      if (strm.avail_out = 0) then
        s^.last_flush := -1; { avoid BUF_ERROR next call, see above }

      deflate := Z_OK;
      exit;
      { If flush != Z_NO_FLUSH && avail_out == 0, the next call
	of deflate should use the same flush parameter to make sure
	that the flush is complete. So we don't have to output an
	empty block here, this will be done at next call. This also
	ensures that for a very small output buffer, we emit at most
	 one empty block. }
    end;
    if (bstate = block_done) then
    begin
      if (flush = Z_PARTIAL_FLUSH) then
        _tr_align(s^)
      else
      begin  { FULL_FLUSH or SYNC_FLUSH }
        _tr_stored_block(s^, nil, 0, FALSE);
        { For a full flush, this empty block will be recognized
          as a special marker by inflate_sync(). }

        if (flush = Z_FULL_FLUSH) then
        begin
          {macro CLEAR_HASH(s);}             { forget history }
          s^.head^[s^.hash_size-1] := ZNIL;
          fillchar(Pbyte(s^.head)^,cardinal(s^.hash_size-1)*sizeof(s^.head^[0]),0);
        end;
      end;

      flush_pending(strm);
      if (strm.avail_out = 0) then
      begin
        s^.last_flush := -1; { avoid BUF_ERROR at next call, see above }
	deflate := Z_OK;
        exit;
      end;

    end;
  end;
  {$IFDEF ZLIB_DEBUG}
  Assert(strm.avail_out > 0, 'bug2');
  {$ENDIF}
  if (flush <> Z_FINISH) then
  begin
    deflate := Z_OK;
    exit;
  end;

  if (s^.noheader <> 0) then
  begin
    deflate := Z_STREAM_END;
    exit;
  end;

  { Write the zlib trailer (adler32) }
  putShortMSB(s^, cardinal(strm.adler shr 16));
  putShortMSB(s^, cardinal(strm.adler and $ffff));
  flush_pending(strm);
  { If avail_out is zero, the application will call deflate again
    to flush the rest. }

  s^.noheader := -1; { write the trailer only once! }
  if s^.pending <> 0 then
    deflate := Z_OK
  else
    deflate := Z_STREAM_END;
end;

{ ========================================================================= }
function deflateEnd (var strm : z_stream) : integer;
var
  status : integer;
  s : deflate_state_ptr;
begin
  if {(@strm=nil) or} (strm.state=nil) then
  begin
    deflateEnd := Z_STREAM_ERROR;
    exit;
  end;

  s := deflate_state_ptr(strm.state);
  status := s^.status;
  if (status <> INIT_STATE) and (status <> BUSY_STATE) and
     (status <> FINISH_STATE) then
  begin
    deflateEnd := Z_STREAM_ERROR;
    exit;
  end;

  { Deallocate in reverse order of allocations: }
  freemem(s^.pending_buf);
  freemem(s^.head);
  freemem(s^.prev);
  freemem(s^.window);

  freemem(s);
  strm.state := nil;

  if status = BUSY_STATE then
    deflateEnd := Z_DATA_ERROR
  else
    deflateEnd := Z_OK;
end;

{ =========================================================================
  Copy the source state to the destination state.
  To simplify the source, this is not supported for 16-bit MSDOS (which
  doesn't have enough memory anyway to duplicate compression states). }


{ ========================================================================= }
function deflateCopy (dest, source : z_streamp) : integer;
{$ifndef MAXSEG_64K}
var
  ds : deflate_state_ptr;
  ss : deflate_state_ptr;
  overlay : Pwordarray;
{$endif}
begin
{$ifdef MAXSEG_64K}
  deflateCopy := Z_STREAM_ERROR;
  exit;
{$else}

  if (source=nil) or (dest=nil) or (source^.state=nil) then
  begin
    deflateCopy := Z_STREAM_ERROR;
    exit;
  end;
  ss := deflate_state_ptr(source^.state);
  dest^ := source^;

  getmem(ds,sizeof(deflate_state));
  if ds=nil then
  begin
    deflateCopy := Z_MEM_ERROR;
    exit;
  end;
  dest^.state := pInternal_state(ds);
  ds^ := ss^;
  ds^.strm := dest;

  getmem(ds^.window,ds^.w_size*2*sizeof(byte));
  getmem(ds^.prev,ds^.w_size*sizeof(pos));
  getmem(ds^.head,ds^.hash_size*sizeof(pos));
  getmem(overlay,ds^.lit_bufsize*(sizeof(word)+2));
  ds^.pending_buf := Pbytearray ( overlay );

  if (ds^.window=nil) or (ds^.prev=nil) or (ds^.head=nil)
     or (ds^.pending_buf=nil) then
  begin
    deflateEnd (dest^);
    deflateCopy := Z_MEM_ERROR;
    exit;
  end;

  move(Pbyte(ss^.window)^,Pbyte(ds^.window)^,ds^.w_size * 2 * sizeof(byte));
  move(Pbyte(ss^.prev)^,Pbyte(ds^.prev)^,ds^.w_size * sizeof(pos));
  move(Pbyte(ss^.head)^,Pbyte(ds^.head)^,ds^.hash_size * sizeof(pos));
  move(Pbyte(ss^.pending_buf)^,Pbyte(ds^.pending_buf)^,cardinal(ds^.pending_buf_size));

  ds^.pending_out := @ds^.pending_buf^[ptruint(ss^.pending_out) - ptruint(ss^.pending_buf)];
  ds^.d_buf := Pwordarray(@overlay^[ds^.lit_bufsize div sizeof(word)] );
  ds^.l_buf := Pbytearray(@ds^.pending_buf^[(1+sizeof(word))*ds^.lit_bufsize]);

  ds^.l_desc.dyn_tree := tree_ptr(@ds^.dyn_ltree);
  ds^.d_desc.dyn_tree := tree_ptr(@ds^.dyn_dtree);
  ds^.bl_desc.dyn_tree := tree_ptr(@ds^.bl_tree);

  deflateCopy := Z_OK;
{$endif}
end;


{ ===========================================================================
  Read a new buffer from the current input stream, update the adler32
  and total number of bytes read.  All deflate() input goes through
  this function so some applications may wish to modify it to avoid
  allocating a large strm^.next_in buffer and copying from it.
  (See also flush_pending()). }

{local}
function read_buf(strm:z_streamp;buf:Pbyte;size:cardinal):cardinal;

var len:cardinal;

begin
  len:=strm^.avail_in;
  if len>size then
    len:=size;
  dec(strm^.avail_in, len);

  if len<>0 then
    begin
      if deflate_state_ptr(strm^.state)^.noheader=0 then
        strm^.adler:=adler32(strm^.adler,strm^.next_in,len);
      move(strm^.next_in^,buf^,len);
      inc(strm^.next_in,len);
      inc(strm^.total_in,len);
    end;
  read_buf:=len;
end;

{ ===========================================================================
  Initialize the "longest match" routines for a new zlib stream }

{local}
procedure lm_init (var s : deflate_state);
begin
  s.window_size := longint( 2*s.w_size);

  {macro CLEAR_HASH(s);}
  s.head^[s.hash_size-1] := ZNIL;
  fillchar(Pbyte(s.head)^, cardinal(s.hash_size-1)*sizeof(s.head^[0]),0);

  { Set the default configuration parameters: }

  s.max_lazy_match   := configuration_table[s.level].max_lazy;
  s.good_match       := configuration_table[s.level].good_length;
  s.nice_match       := configuration_table[s.level].nice_length;
  s.max_chain_length := configuration_table[s.level].max_chain;

  s.strstart := 0;
  s.block_start := longint(0);
  s.lookahead := 0;
  s.prev_length := MIN_MATCH-1;
  s.match_length := MIN_MATCH-1;
  s.match_available := FALSE;
  s.ins_h := 0;
{$ifdef ASMV}
  match_init; { initialize the asm code }
{$endif}
end;

{ ===========================================================================
  Set match_start to the longest match starting at the given string and
  return its length. Matches shorter or equal to prev_length are discarded,
  in which case the result is equal to prev_length and match_start is
  garbage.
  IN assertions: cur_match is the head of the hash chain for the current
    string (strstart) and its distance is <= MAX_DIST, and prev_length >= 1
  OUT assertion: the match length is not greater than s^.lookahead. }


{$ifndef ASMV}
{ For 80x86 and 680x0, an optimized version will be provided in match.asm or
  match.S. The code will be functionally equivalent. }

{$ifndef FASTEST}

{local}
function longest_match(var s : deflate_state;
                       cur_match : IPos  { current match }
                       ) : cardinal;
label
  nextstep;
var
  chain_length : cardinal;    { max hash chain length }
  {register} scan : Pbyte;   { current string }
  {register} match : Pbyte;  { matched string }
  {register} len : integer;       { length of current match }
  best_len : integer;             { best match length so far }
  nice_match : integer;           { stop if match longint enough }
  limit : IPos;

  prev : pzPosfArray;
  wmask : cardinal;
{$ifdef UNALIGNED_OK}
  {register} strend : Pbyte;
  {register} scan_start : word;
  {register} scan_end : word;
{$else}
  {register} strend : Pbyte;
  {register} scan_end1 : Byte;
  {register} scan_end : Byte;
{$endif}
var
  MAX_DIST : cardinal;
begin
  chain_length := s.max_chain_length; { max hash chain length }
  scan := @(s.window^[s.strstart]);
  best_len := s.prev_length;              { best match length so far }
  nice_match := s.nice_match;             { stop if match longint enough }


  MAX_DIST := s.w_size - MIN_LOOKAHEAD;
{In order to simplify the code, particularly on 16 bit machines, match
distances are limited to MAX_DIST instead of WSIZE. }

  if s.strstart > IPos(MAX_DIST) then
    limit := s.strstart - IPos(MAX_DIST)
  else
    limit := ZNIL;
  { Stop when cur_match becomes <= limit. To simplify the code,
    we prevent matches with the string of window index 0. }

  prev := s.prev;
  wmask := s.w_mask;

{$ifdef UNALIGNED_OK}
  { Compare two bytes at a time. Note: this is not always beneficial.
    Try with and without -DUNALIGNED_OK to check. }

  strend := Pbyte(@(s.window^[s.strstart + MAX_MATCH - 1]));
  scan_start := pushf(scan)^;
  scan_end   := Pwordarray(scan)^[best_len-1];   { fix }
{$else}
  strend := Pbyte(@(s.window^[s.strstart + MAX_MATCH]));
  {$push} {$R-}
  scan_end1  := Pbytearray(scan)^[best_len-1];
  {$pop}
  scan_end   := Pbytearray(scan)^[best_len];
{$endif}

    { The code is optimized for HASH_BITS >= 8 and MAX_MATCH-2 multiple of 16.
      It is easy to get rid of this optimization if necessary. }
    {$IFDEF ZLIB_DEBUG}
    Assert((s.hash_bits >= 8) and (MAX_MATCH = 258), 'Code too clever');
    {$ENDIF}
    { Do not waste too much time if we already have a good match: }
    if (s.prev_length >= s.good_match) then
    begin
      chain_length := chain_length shr 2;
    end;

    { Do not look for matches beyond the end of the input. This is necessary
      to make deflate deterministic. }

    if (cardinal(nice_match) > s.lookahead) then
      nice_match := s.lookahead;
    {$IFDEF ZLIB_DEBUG}
    Assert(longint(s.strstart) <= s.window_size-MIN_LOOKAHEAD, 'need lookahead');
    {$ENDIF}
    repeat
        {$IFDEF ZLIB_DEBUG}
        Assert(cur_match < s.strstart, 'no future');
        {$ENDIF}
        match := @(s.window^[cur_match]);

        { Skip to next match if the match length cannot increase
          or if the match length is less than 2: }

{$undef DO_UNALIGNED_OK}
{$ifdef UNALIGNED_OK}
  {$ifdef MAX_MATCH_IS_258}
    {$define DO_UNALIGNED_OK}
  {$endif}
{$endif}

{$ifdef DO_UNALIGNED_OK}
        { This code assumes sizeof(cardinal short) = 2. Do not use
          UNALIGNED_OK if your compiler uses a different size. }
  {$PUSH} {$R-}
        if (match[best_len-1]<>scan_end) or
           (match^ <> scan_start) then
          goto nextstep; {continue;}
  {$POP}

        { It is not necessary to compare scan[2] and match[2] since they are
          always equal when the other bytes match, given that the hash keys
          are equal and that HASH_BITS >= 8. Compare 2 bytes at a time at
          strstart+3, +5, ... up to strstart+257. We check for insufficient
          lookahead only every 4th comparison; the 128th check will be made
          at strstart+257. If MAX_MATCH-2 is not a multiple of 8, it is
          necessary to put more guard bytes at the end of the window, or
          to check more often for insufficient lookahead. }
        {$IFDEF ZLIB_DEBUG}
        Assert(pzByteArray(scan)^[2] = pzByteArray(match)^[2], 'scan[2]?');
        {$ENDIF}
        inc(scan);
        inc(match);

        repeat
          inc(scan,2); inc(match,2); if scan^<>match^ then break;
          inc(scan,2); inc(match,2); if scan^<>match^ then break;
          inc(scan,2); inc(match,2); if scan^<>match^ then break;
          inc(scan,2); inc(match,2); if scan^<>match^ then break;
        until ptruint(scan)>=ptruint(strend);
        { The funny "do while" generates better code on most compilers }

        { Here, scan <= window+strstart+257 }
        {$IFDEF ZLIB_DEBUG}
        {$PUSH} {$R-}
        Assert(ptruint(scan) <=
               ptruint(@(s.window^[cardinal(s.window_size-1)])),
               'wild scan');
        {$POP}
        {$ENDIF}
        if scan^=match^ then
          inc(scan);

        len := (MAX_MATCH - 1) - integer(ptruint(strend)) + integer(ptruint(scan));
        scan := strend;
        dec(scan, (MAX_MATCH-1));

{$else} { UNALIGNED_OK }

  {$PUSH} {$R-}
        if (Pbytearray(match)^[best_len]   <> scan_end) or
           (Pbytearray(match)^[best_len-1] <> scan_end1) or
           (match^ <> scan^) then
        /// goto nextstep; {continue;}
  {$POP}
        inc(match);
        if (match^ <> Pbytearray(scan)^[1]) then
          /// goto nextstep; {continue;}

        { The check at best_len-1 can be removed because it will be made
          again later. (This heuristic is not always a win.)
          It is not necessary to compare scan[2] and match[2] since they
          are always equal when the other bytes match, given that
          the hash keys are equal and that HASH_BITS >= 8. }

        inc(scan, 2);
        inc(match);
        {$IFDEF ZLIB_DEBUG}
        Assert( scan^ = match^, 'match[2]?');
        {$ENDIF}
        { We check for insufficient lookahead only every 8th comparison;
          the 256th check will be made at strstart+258. }

        repeat
          inc(scan); inc(match); if scan^ <> match^ then break;
          inc(scan); inc(match); if scan^ <> match^ then break;
          inc(scan); inc(match); if scan^ <> match^ then break;
          inc(scan); inc(match); if scan^ <> match^ then break;
          inc(scan); inc(match); if scan^ <> match^ then break;
          inc(scan); inc(match); if scan^ <> match^ then break;
          inc(scan); inc(match); if scan^ <> match^ then break;
          inc(scan); inc(match); if scan^ <> match^ then break;
        until ptruint(scan)>=ptruint(strend);

        {$IFDEF ZLIB_DEBUG}
        Assert(ptruint(scan) <=
               ptruint(@(s.window^[cardinal(s.window_size-1)])),
               'wild scan');
        {$ENDIF}

        len := MAX_MATCH - (ptruint(strend) - ptruint(scan));
        scan := strend;
        dec(scan, MAX_MATCH);

{$endif} { UNALIGNED_OK }

        if (len > best_len) then
        begin
            s.match_start := cur_match;
            best_len := len;
            if (len >= nice_match) then
              break;
{$push} {$R-}
{$ifdef UNALIGNED_OK}
            scan_end   := Pbytearray(scan)^[best_len-1];
{$else}
            scan_end1  := Pbytearray(scan)^[best_len-1];
            scan_end   := Pbytearray(scan)^[best_len];
{$endif}
{$pop}
        end;
    nextstep:
      cur_match := prev^[cur_match and wmask];
      dec(chain_length);
    until (cur_match <= limit) or (chain_length = 0);

    if (cardinal(best_len) <= s.lookahead) then
      longest_match := cardinal(best_len)
    else
      longest_match := s.lookahead;
end;
{$endif} { ASMV }

{$else} { FASTEST }
{ ---------------------------------------------------------------------------
  Optimized version for level = 1 only }

{local}
function longest_match(var s : deflate_state;
                       cur_match : IPos  { current match }
                       ) : cardinal;
var
  {register} scan : Pbyte;   { current string }
  {register} match : Pbyte;  { matched string }
  {register} len : integer;       { length of current match }
  {register} strend : Pbyte;
begin
  scan := @s.window^[s.strstart];
  strend := @s.window^[s.strstart + MAX_MATCH];


    { The code is optimized for HASH_BITS >= 8 and MAX_MATCH-2 multiple of 16.
      It is easy to get rid of this optimization if necessary. }
    {$IFDEF ZLIB_DEBUG}
    Assert((s.hash_bits >= 8) and (MAX_MATCH = 258), 'Code too clever');

    Assert(longint(s.strstart) <= s.window_size-MIN_LOOKAHEAD, 'need lookahead');

    Assert(cur_match < s.strstart, 'no future');
    {$ENDIF}
    match := s.window + cur_match;

    { Return failure if the match length is less than 2: }

    if (match[0] <> scan[0]) or (match[1] <> scan[1]) then
    begin
      longest_match := MIN_MATCH-1;
      exit;
    end;

    { The check at best_len-1 can be removed because it will be made
      again later. (This heuristic is not always a win.)
      It is not necessary to compare scan[2] and match[2] since they
      are always equal when the other bytes match, given that
      the hash keys are equal and that HASH_BITS >= 8. }

    scan += 2, match += 2;
    Assert(scan^ = match^, 'match[2]?');

    { We check for insufficient lookahead only every 8th comparison;
      the 256th check will be made at strstart+258. }

    repeat
      inc(scan); inc(match); if scan^<>match^ then break;
      inc(scan); inc(match); if scan^<>match^ then break;
      inc(scan); inc(match); if scan^<>match^ then break;
      inc(scan); inc(match); if scan^<>match^ then break;
      inc(scan); inc(match); if scan^<>match^ then break;
      inc(scan); inc(match); if scan^<>match^ then break;
      inc(scan); inc(match); if scan^<>match^ then break;
      inc(scan); inc(match); if scan^<>match^ then break;
    until (ptruint(scan) >= ptruint(strend));

    Assert(scan <= s.window+cardinal(s.window_size-1), 'wild scan');

    len := MAX_MATCH - integer(strend - scan);

    if (len < MIN_MATCH) then
    begin
      return := MIN_MATCH - 1;
      exit;
    end;

    s.match_start := cur_match;
    if len <= s.lookahead then
      longest_match := len
    else
      longest_match := s.lookahead;
end;
{$endif} { FASTEST }

{$ifdef ZLIB_DEBUG}
function zmemcmp(s1p, s2p : PByte; len : Cardinal) : Integer;
var
  j : Cardinal;
  source,
  dest : PByte;
begin
  source := s1p;
  dest := s2p;
  for j := 0 to pred(len) do
  begin
    if (source^ <> dest^) then
    begin
      zmemcmp := 2*Ord(source^ > dest^)-1;
      exit;
    end;
    Inc(source);
    Inc(dest);
  end;
  zmemcmp := 0;
end;

{ ===========================================================================
  Check that the match at match_start is indeed a match. }

{local}
procedure check_match(var s : deflate_state;
                      start, match : IPos;
                      length : integer);
begin
  exit;
  { check that the match is indeed a match }
  if (zmemcmp(Pbyte(@s.window^[match]),
              Pbyte(@s.window^[start]), length) <> EQUAL) then
  begin
    WriteLn(' start ',start,', match ',match ,' length ', length);
    repeat
      Write(char(s.window^[match]), char(s.window^[start]));
      inc(match);
      inc(start);
      dec(length);
    Until (length = 0);
    z_error('invalid match');
  end;
  if (z_verbose > 1) then
  begin
    Write('\\[',start-match,',',length,']');
    repeat
       Write(char(s.window^[start]));
       inc(start);
       dec(length);
    Until (length = 0);
  end;
end;
{$endif}

{ ===========================================================================
  Fill the window when the lookahead becomes insufficient.
  Updates strstart and lookahead.

  IN assertion: lookahead < MIN_LOOKAHEAD
  OUT assertions: strstart <= window_size-MIN_LOOKAHEAD
     At least one byte has been read, or avail_in = 0; reads are
     performed for at least two bytes (required for the zip translate_eol
     option -- not supported here). }

{local}
procedure fill_window(var s : deflate_state);
var
  {register} n, m : cardinal;
  {register} p : pPosf;
  more : cardinal;    { Amount of free space at the end of the window. }
  wsize : cardinal;
begin
   wsize := s.w_size;
   repeat
     more := cardinal(s.window_size -longint(s.lookahead) -longint(s.strstart));

     { Deal with !@#$% 64K limit: }
     if (more = 0) and (s.strstart = 0) and (s.lookahead = 0) then
       more := wsize
     else
     if (more = cardinal(-1)) then
     begin
       { Very unlikely, but possible on 16 bit machine if strstart = 0
         and lookahead = 1 (input done one byte at time) }
       dec(more);

       { If the window is almost full and there is insufficient lookahead,
         move the upper half to the lower one to make room in the upper half.}
     end
     else
       if (s.strstart >= wsize+ {MAX_DIST}(wsize-MIN_LOOKAHEAD)) then
       begin
         move(s.window^[wsize],Pbyte(s.window)^,wsize);
         dec(s.match_start, wsize);
         dec(s.strstart, wsize); { we now have strstart >= MAX_DIST }
         dec(s.block_start, longint(wsize));

         { Slide the hash table (could be avoided with 32 bit values
           at the expense of memory usage). We slide even when level = 0
           to keep the hash table consistent if we switch back to level > 0
           later. (Using level 0 permanently is not an optimal usage of
           zlib, so we don't care about this pathological case.) }

         n := s.hash_size;
         p := @s.head^[n];
         repeat
           dec(p);
           m := p^;
           if (m >= wsize) then
             p^ := Pos(m-wsize)
           else
             p^ := Pos(ZNIL);
           dec(n);
         Until (n=0);

         n := wsize;
{$ifndef FASTEST}
         p := @s.prev^[n];
         repeat
           dec(p);
           m := p^;
           if (m >= wsize) then
             p^ := Pos(m-wsize)
           else
             p^:= Pos(ZNIL);
             { If n is not on any hash chain, prev^[n] is garbage but
               its value will never be used. }
           dec(n);
         Until (n=0);
{$endif}
         inc(more, wsize);
     end;
     if (s.strm^.avail_in = 0) then
       exit;

     {* If there was no sliding:
      *    strstart <= WSIZE+MAX_DIST-1 && lookahead <= MIN_LOOKAHEAD - 1 &&
      *    more == window_size - lookahead - strstart
      * => more >= window_size - (MIN_LOOKAHEAD-1 + WSIZE + MAX_DIST-1)
      * => more >= window_size - 2*WSIZE + 2
      * In the BIG_MEM or MMAP case (not yet supported),
      *   window_size == input_size + MIN_LOOKAHEAD  &&
      *   strstart + s->lookahead <= input_size => more >= MIN_LOOKAHEAD.
      * Otherwise, window_size == 2*WSIZE so more >= 2.
      * If there was sliding, more >= WSIZE. So in all cases, more >= 2. }

     {$IFDEF ZLIB_DEBUG}
     Assert(more >= 2, 'more < 2');
     {$ENDIF}

     n := read_buf(s.strm, Pbyte(@(s.window^[s.strstart + s.lookahead])),
                  more);
     inc(s.lookahead, n);

     { Initialize the hash value now that we have some input: }
     if (s.lookahead >= MIN_MATCH) then
     begin
       s.ins_h := s.window^[s.strstart];
       {UPDATE_HASH(s, s.ins_h, s.window[s.strstart+1]);}
       s.ins_h := ((s.ins_h shl s.hash_shift) xor s.window^[s.strstart+1])
                     and s.hash_mask;
{$ifdef MIN_MATCH <> 3}
       Call UPDATE_HASH() MIN_MATCH-3 more times
{$endif}
     end;
     { If the whole input has less than MIN_MATCH bytes, ins_h is garbage,
       but this is not important since only literal bytes will be emitted. }

   until (s.lookahead >= MIN_LOOKAHEAD) or (s.strm^.avail_in = 0);
end;

{ ===========================================================================
  Flush the current block, with given end-of-file flag.
  IN assertion: strstart is set to the end of the current match. }

procedure FLUSH_BLOCK_ONLY(var s : deflate_state; eof : boolean); {macro}
begin
  if (s.block_start >= 0) then
    _tr_flush_block(s, Pbyte(@s.window^[s.block_start]),
                    longint(longint(s.strstart) - s.block_start), eof)
  else
    _tr_flush_block(s, nil,
                    longint(longint(s.strstart) - s.block_start), eof);

  s.block_start := s.strstart;
  flush_pending(s.strm^);
  {$IFDEF ZLIB_DEBUG}
  Tracev('[FLUSH]');
  {$ENDIF}
end;

{ Same but force premature exit if necessary.
macro FLUSH_BLOCK(var s : deflate_state; eof : boolean) : boolean;
var
  result : block_state;
begin
 FLUSH_BLOCK_ONLY(s, eof);
 if (s.strm^.avail_out = 0) then
 begin
   if eof then
     result := finish_started
   else
     result := need_more;
   exit;
 end;
end;
}

{ ===========================================================================
  Copy without compression as much as possible from the input stream, return
  the current block state.
  This function does not insert new strings in the dictionary since
  uncompressible data is probably not useful. This function is used
  only for the level=0 compression option.
  NOTE: this function should be optimized to avoid extra copying from
  window to pending_buf. }


{local}
function deflate_stored(var s : deflate_state; flush : integer) : block_state;
{ Stored blocks are limited to 0xffff bytes, pending_buf is limited
  to pending_buf_size, and each stored block has a 5 byte header: }
var
  max_block_size : longint;
  max_start : longint;
begin
  max_block_size := $ffff;
  if (max_block_size > s.pending_buf_size - 5) then
    max_block_size := s.pending_buf_size - 5;

  { Copy as much as possible from input to output: }
  while TRUE do
  begin
    { Fill the window as much as possible: }
    if (s.lookahead <= 1) then
    begin
      {$IFDEF ZLIB_DEBUG}
      Assert( (s.strstart < s.w_size + {MAX_DIST}s.w_size-MIN_LOOKAHEAD) or
              (s.block_start >= longint(s.w_size)), 'slide too late');
      {$ENDIF}
      fill_window(s);
      if (s.lookahead = 0) and (flush = Z_NO_FLUSH) then
      begin
        deflate_stored := need_more;
        exit;
      end;

      if (s.lookahead = 0) then
        break; { flush the current block }
    end;
    {$IFDEF ZLIB_DEBUG}
    Assert(s.block_start >= 0, 'block gone');
    {$ENDIF}
    inc(s.strstart, s.lookahead);
    s.lookahead := 0;

    { Emit a stored block if pending_buf will be full: }
    max_start := s.block_start + max_block_size;
    if (s.strstart = 0) or (longint(s.strstart) >= max_start) then
    begin
      { strstart = 0 is possible when wraparound on 16-bit machine }
      s.lookahead := cardinal(s.strstart) - cardinal(max_start);
      s.strstart := cardinal(max_start);
      {FLUSH_BLOCK(s, FALSE);}
      FLUSH_BLOCK_ONLY(s, FALSE);
      if (s.strm^.avail_out = 0) then
      begin
        deflate_stored := need_more;
        exit;
      end;
    end;

    { Flush if we may have to slide, otherwise block_start may become
      negative and the data will be gone: }

    if (s.strstart - cardinal(s.block_start) >= {MAX_DIST}
        s.w_size-MIN_LOOKAHEAD) then
    begin
      {FLUSH_BLOCK(s, FALSE);}
      FLUSH_BLOCK_ONLY(s, FALSE);
      if (s.strm^.avail_out = 0) then
      begin
        deflate_stored := need_more;
        exit;
      end;
    end;
  end;

  {FLUSH_BLOCK(s, flush = Z_FINISH);}
  FLUSH_BLOCK_ONLY(s, flush = Z_FINISH);
  if (s.strm^.avail_out = 0) then
  begin
    if flush = Z_FINISH then
      deflate_stored := finish_started
    else
      deflate_stored := need_more;
    exit;
  end;

  if flush = Z_FINISH then
    deflate_stored := finish_done
  else
    deflate_stored := block_done;
end;

{ ===========================================================================
  Compress as much as possible from the input stream, return the current
  block state.
  This function does not perform lazy evaluation of matches and inserts
  new strings in the dictionary only for unmatched strings or for short
  matches. It is used only for the fast compression options. }

{local}
function deflate_fast(var s : deflate_state; flush : integer) : block_state;
var
  hash_head : IPos;     { head of the hash chain }
  bflush : boolean;     { set if current block must be flushed }
begin
  hash_head := ZNIL;
  while TRUE do
  begin
  { Make sure that we always have enough lookahead, except
    at the end of the input file. We need MAX_MATCH bytes
    for the next match, plus MIN_MATCH bytes to insert the
    string following the next match. }

    if (s.lookahead < MIN_LOOKAHEAD) then
    begin
      fill_window(s);
      if (s.lookahead < MIN_LOOKAHEAD) and (flush = Z_NO_FLUSH) then
      begin
        deflate_fast := need_more;
        exit;
      end;

      if (s.lookahead = 0) then
        break; { flush the current block }
    end;


    { Insert the string window[strstart .. strstart+2] in the
      dictionary, and set hash_head to the head of the hash chain: }

    if (s.lookahead >= MIN_MATCH) then
      INSERT_STRING(s, s.strstart, hash_head);

    { Find the longest match, discarding those <= prev_length.
      At this point we have always match_length < MIN_MATCH }
    if (hash_head <> ZNIL) and
       (s.strstart - hash_head <= (s.w_size-MIN_LOOKAHEAD){MAX_DIST}) then
    begin
      { To simplify the code, we prevent matches with the string
        of window index 0 (in particular we have to avoid a match
        of the string with itself at the start of the input file). }
      if (s.strategy <> Z_HUFFMAN_ONLY) then
      begin
        s.match_length := longest_match (s, hash_head);
      end;
      { longest_match() sets match_start }
    end;
    if (s.match_length >= MIN_MATCH) then
    begin
      {$IFDEF ZLIB_DEBUG}
      check_match(s, s.strstart, s.match_start, s.match_length);
      {$ENDIF}

      {_tr_tally_dist(s, s.strstart - s.match_start,
                        s.match_length - MIN_MATCH, bflush);}
      bflush := _tr_tally(s, s.strstart - s.match_start,
                        s.match_length - MIN_MATCH);

      dec(s.lookahead, s.match_length);

      { Insert new strings in the hash table only if the match length
        is not too large. This saves time but degrades compression. }

{$ifndef FASTEST}
      if (s.match_length <= s.max_insert_length)
       and (s.lookahead >= MIN_MATCH) then
      begin
        dec(s.match_length); { string at strstart already in hash table }
        repeat
          inc(s.strstart);
          INSERT_STRING(s, s.strstart, hash_head);
          { strstart never exceeds WSIZE-MAX_MATCH, so there are
            always MIN_MATCH bytes ahead. }
          dec(s.match_length);
        until (s.match_length = 0);
        inc(s.strstart);
      end
      else
{$endif}

      begin
        inc(s.strstart, s.match_length);
        s.match_length := 0;
        s.ins_h := s.window^[s.strstart];
        {UPDATE_HASH(s, s.ins_h, s.window[s.strstart+1]);}
        s.ins_h := (( s.ins_h shl s.hash_shift) xor
                     s.window^[s.strstart+1]) and s.hash_mask;
if MIN_MATCH <> 3 then   { the linker removes this }
begin
          {Call UPDATE_HASH() MIN_MATCH-3 more times}
end;

        { If lookahead < MIN_MATCH, ins_h is garbage, but it does not
          matter since it will be recomputed at next deflate call. }

      end;
    end
    else
    begin
      { No match, output a literal byte }
      {$IFDEF ZLIB_DEBUG}
      Tracevv(char(s.window^[s.strstart]));
      {$ENDIF}
      {_tr_tally_lit (s, 0, s.window^[s.strstart], bflush);}
      bflush := _tr_tally (s, 0, s.window^[s.strstart]);

      dec(s.lookahead);
      inc(s.strstart);
    end;
    if bflush then
    begin  {FLUSH_BLOCK(s, FALSE);}
      FLUSH_BLOCK_ONLY(s, FALSE);
      if (s.strm^.avail_out = 0) then
      begin
        deflate_fast := need_more;
        exit;
      end;
    end;
  end;
  {FLUSH_BLOCK(s, flush = Z_FINISH);}
  FLUSH_BLOCK_ONLY(s, flush = Z_FINISH);
  if (s.strm^.avail_out = 0) then
  begin
    if flush = Z_FINISH then
      deflate_fast := finish_started
    else
      deflate_fast := need_more;
    exit;
  end;

  if flush = Z_FINISH then
    deflate_fast := finish_done
  else
    deflate_fast := block_done;
end;

{ ===========================================================================
  Same as above, but achieves better compression. We use a lazy
  evaluation for matches: a match is finally adopted only if there is
  no better match at the next window position. }

{local}
function deflate_slow(var s : deflate_state; flush : integer) : block_state;
var
  hash_head : IPos;       { head of hash chain }
  bflush : boolean;       { set if current block must be flushed }
var
  max_insert : cardinal;
begin
  hash_head := ZNIL;

  { Process the input block. }
  repeat
    { Make sure that we always have enough lookahead, except
      at the end of the input file. We need MAX_MATCH bytes
      for the next match, plus MIN_MATCH bytes to insert the
      string following the next match. }

    if (s.lookahead < MIN_LOOKAHEAD) then
      begin
        fill_window(s);
        if (s.lookahead < MIN_LOOKAHEAD) and (flush = Z_NO_FLUSH) then
          begin
            deflate_slow := need_more;
            exit;
          end;

        if s.lookahead=0 then
          break; { flush the current block }
      end;

    { Insert the string window[strstart .. strstart+2] in the
      dictionary, and set hash_head to the head of the hash chain: }

    if (s.lookahead >= MIN_MATCH) then
      INSERT_STRING(s, s.strstart, hash_head);

    { Find the longest match, discarding those <= prev_length. }

    s.prev_length := s.match_length;
    s.prev_match := s.match_start;
    s.match_length := MIN_MATCH-1;

    if (hash_head <> ZNIL) and (s.prev_length < s.max_lazy_match) and
       (s.strstart - hash_head <= {MAX_DIST}(s.w_size-MIN_LOOKAHEAD)) then
      begin
        { To simplify the code, we prevent matches with the string
          of window index 0 (in particular we have to avoid a match
          of the string with itself at the start of the input file). }

        if (s.strategy <> Z_HUFFMAN_ONLY) then
          s.match_length := longest_match (s, hash_head);
        { longest_match() sets match_start }

        if (s.match_length <= 5) and ((s.strategy = Z_FILTERED) or
             ((s.match_length = MIN_MATCH) and
              (s.strstart - s.match_start > TOO_FAR))) then
          begin
            { If prev_match is also MIN_MATCH, match_start is garbage
              but we will ignore the current match anyway. }

            s.match_length := MIN_MATCH-1;
          end;
      end;
    { If there was a match at the previous step and the current
      match is not better, output the previous match: }

    if (s.prev_length>=MIN_MATCH) and (s.match_length<=s.prev_length) then
      begin
        max_insert := s.strstart + s.lookahead - MIN_MATCH;
        { Do not insert strings in hash table beyond this. }
        {$ifdef ZLIB_DEBUG}
        check_match(s, s.strstart-1, s.prev_match, s.prev_length);
        {$endif}

        {_tr_tally_dist(s, s->strstart -1 - s->prev_match,
  	                  s->prev_length - MIN_MATCH, bflush);}
        bflush := _tr_tally(s, s.strstart -1 - s.prev_match,
                             s.prev_length - MIN_MATCH);

      { Insert in hash table all strings up to the end of the match.
        strstart-1 and strstart are already inserted. If there is not
        enough lookahead, the last two strings are not inserted in
        the hash table. }

{$ifdef ZLIB_DEBUG}
        if s.lookahead<s.prev_length-1 then
           runerror(255);
{$endif}
        dec(s.lookahead, s.prev_length-1);
        dec(s.prev_length, 2);
        repeat
          inc(s.strstart);
          if s.strstart<=max_insert then
            INSERT_STRING(s, s.strstart, hash_head);
          dec(s.prev_length);
        until s.prev_length = 0;
        s.match_available := false;
        s.match_length := MIN_MATCH-1;
        inc(s.strstart);

        if bflush then  {FLUSH_BLOCK(s, FALSE);}
          begin
            FLUSH_BLOCK_ONLY(s,false);
            if s.strm^.avail_out=0 then
              begin
                deflate_slow := need_more;
                exit;
              end;
          end;
      end
    else
      if s.match_available then
        begin
          { If there was no match at the previous position, output a
            single literal. If there was a match but the current match
            is longer, truncate the previous match to a single literal. }
          {$IFDEF ZLIB_DEBUG}
          Tracevv(char(s.window^[s.strstart-1]));
          {$ENDIF}
          bflush := _tr_tally (s, 0, s.window^[s.strstart-1]);

          if bflush then
            FLUSH_BLOCK_ONLY(s, FALSE);
          inc(s.strstart);
{$ifdef ZLIB_DEBUG}
          if s.lookahead=0 then
             runerror(255);
{$endif}
          dec(s.lookahead);
          if (s.strm^.avail_out = 0) then
            begin
              deflate_slow := need_more;
              exit;
            end;
        end
      else
        begin
        { There is no previous match to compare with, wait for
          the next step to decide. }

          s.match_available := TRUE;
          inc(s.strstart);
{$ifdef ZLIB_DEBUG}
          if s.lookahead=0 then
             runerror(255);
{$endif}
          dec(s.lookahead);
        end;
  until false;

  {$IFDEF ZLIB_DEBUG}
  Assert (flush <> Z_NO_FLUSH, 'no flush?');
  {$ENDIF}
  if (s.match_available) then
  begin
    {$IFDEF ZLIB_DEBUG}
    Tracevv(char(s.window^[s.strstart-1]));
    bflush :=
    {$ENDIF}
      _tr_tally (s, 0, s.window^[s.strstart-1]);
    s.match_available := FALSE;
  end;
  {FLUSH_BLOCK(s, flush = Z_FINISH);}
  FLUSH_BLOCK_ONLY(s, flush = Z_FINISH);
  if (s.strm^.avail_out = 0) then
  begin
    if flush = Z_FINISH then
      deflate_slow := finish_started
    else
      deflate_slow := need_more;
    exit;
  end;
  if flush = Z_FINISH then
    deflate_slow := finish_done
  else
    deflate_slow := block_done;
end;

end.
