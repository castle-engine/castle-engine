Unit  imzinflate;

{  inflate.c -- zlib interface to inflate modules
   Copyright (C) 1995-1998 Mark Adler

  Pascal tranlastion
  Copyright (C) 1998 by Jacques Nomssi Nzali
  For conditions of distribution and use, see copyright notice in readme.txt
}

interface

{$I imzconf.inc}

uses
  imzutil, impaszlib, iminfblock, iminfutil;

function inflateInit(var z : z_stream) : int;

{    Initializes the internal stream state for decompression. The fields
   zalloc, zfree and opaque must be initialized before by the caller.  If
   zalloc and zfree are set to Z_NULL, inflateInit updates them to use default
   allocation functions.

     inflateInit returns Z_OK if success, Z_MEM_ERROR if there was not
   enough memory, Z_VERSION_ERROR if the zlib library version is incompatible
   with the version assumed by the caller.  msg is set to null if there is no
   error message. inflateInit does not perform any decompression: this will be
   done by inflate(). }



function inflateInit_(z : z_streamp;
                      const version : AnsiString;
                      stream_size : int) : int; 


function inflateInit2_(var z: z_stream;
                       w : int;
                       const version : AnsiString;
                       stream_size : int) : int;

function inflateInit2(var z: z_stream;
                       windowBits : int) : int;

{
     This is another version of inflateInit with an extra parameter. The
   fields next_in, avail_in, zalloc, zfree and opaque must be initialized
   before by the caller.

     The windowBits parameter is the base two logarithm of the maximum window
   size (the size of the history buffer).  It should be in the range 8..15 for
   this version of the library. The default value is 15 if inflateInit is used
   instead. If a compressed stream with a larger window size is given as
   input, inflate() will return with the error code Z_DATA_ERROR instead of
   trying to allocate a larger window.

      inflateInit2 returns Z_OK if success, Z_MEM_ERROR if there was not enough
   memory, Z_STREAM_ERROR if a parameter is invalid (such as a negative
   memLevel). msg is set to null if there is no error message.  inflateInit2
   does not perform any decompression apart from reading the zlib header if
   present: this will be done by inflate(). (So next_in and avail_in may be
   modified, but next_out and avail_out are unchanged.)
}



function inflateEnd(var z : z_stream) : int;

{
   All dynamically allocated data structures for this stream are freed.
   This function discards any unprocessed input and does not flush any
   pending output.

     inflateEnd returns Z_OK if success, Z_STREAM_ERROR if the stream state
   was inconsistent. In the error case, msg may be set but then points to a
   static string (which must not be deallocated).
}

function inflateReset(var z : z_stream) : int;

{
   This function is equivalent to inflateEnd followed by inflateInit,
   but does not free and reallocate all the internal decompression state.
   The stream will keep attributes that may have been set by inflateInit2.

      inflateReset returns Z_OK if success, or Z_STREAM_ERROR if the source
   stream state was inconsistent (such as zalloc or state being NULL).
}


function inflate(var z : z_stream;
                 f : int) : int;
{
  inflate decompresses as much data as possible, and stops when the input
  buffer becomes empty or the output buffer becomes full. It may introduce
  some output latency (reading input without producing any output)
  except when forced to flush.

  The detailed semantics are as follows. inflate performs one or both of the
  following actions:

  - Decompress more input starting at next_in and update next_in and avail_in
    accordingly. If not all input can be processed (because there is not
    enough room in the output buffer), next_in is updated and processing
    will resume at this point for the next call of inflate().

  - Provide more output starting at next_out and update next_out and avail_out
    accordingly.  inflate() provides as much output as possible, until there
    is no more input data or no more space in the output buffer (see below
    about the flush parameter).

  Before the call of inflate(), the application should ensure that at least
  one of the actions is possible, by providing more input and/or consuming
  more output, and updating the next_* and avail_* values accordingly.
  The application can consume the uncompressed output when it wants, for
  example when the output buffer is full (avail_out == 0), or after each
  call of inflate(). If inflate returns Z_OK and with zero avail_out, it
  must be called again after making room in the output buffer because there
  might be more output pending.

    If the parameter flush is set to Z_SYNC_FLUSH, inflate flushes as much
  output as possible to the output buffer. The flushing behavior of inflate is
  not specified for values of the flush parameter other than Z_SYNC_FLUSH
  and Z_FINISH, but the current implementation actually flushes as much output
  as possible anyway.

    inflate() should normally be called until it returns Z_STREAM_END or an
  error. However if all decompression is to be performed in a single step
  (a single call of inflate), the parameter flush should be set to
  Z_FINISH. In this case all pending input is processed and all pending
  output is flushed; avail_out must be large enough to hold all the
  uncompressed data. (The size of the uncompressed data may have been saved
  by the compressor for this purpose.) The next operation on this stream must
  be inflateEnd to deallocate the decompression state. The use of Z_FINISH
  is never required, but can be used to inform inflate that a faster routine
  may be used for the single inflate() call.

     If a preset dictionary is needed at this point (see inflateSetDictionary
  below), inflate sets strm-adler to the adler32 checksum of the
  dictionary chosen by the compressor and returns Z_NEED_DICT; otherwise 
  it sets strm->adler to the adler32 checksum of all output produced
  so far (that is, total_out bytes) and returns Z_OK, Z_STREAM_END or
  an error code as described below. At the end of the stream, inflate()
  checks that its computed adler32 checksum is equal to that saved by the
  compressor and returns Z_STREAM_END only if the checksum is correct.

    inflate() returns Z_OK if some progress has been made (more input processed
  or more output produced), Z_STREAM_END if the end of the compressed data has
  been reached and all uncompressed output has been produced, Z_NEED_DICT if a
  preset dictionary is needed at this point, Z_DATA_ERROR if the input data was
  corrupted (input stream not conforming to the zlib format or incorrect
  adler32 checksum), Z_STREAM_ERROR if the stream structure was inconsistent
  (for example if next_in or next_out was NULL), Z_MEM_ERROR if there was not
  enough memory, Z_BUF_ERROR if no progress is possible or if there was not
  enough room in the output buffer when Z_FINISH is used. In the Z_DATA_ERROR
  case, the application may then call inflateSync to look for a good
  compression block.
}


function inflateSetDictionary(var z : z_stream;
                              dictionary : pBytef; {const array of byte}
                              dictLength : uInt) : int;

{
     Initializes the decompression dictionary from the given uncompressed byte
   sequence. This function must be called immediately after a call of inflate
   if this call returned Z_NEED_DICT. The dictionary chosen by the compressor
   can be determined from the Adler32 value returned by this call of
   inflate. The compressor and decompressor must use exactly the same
   dictionary (see deflateSetDictionary).

     inflateSetDictionary returns Z_OK if success, Z_STREAM_ERROR if a
   parameter is invalid (such as NULL dictionary) or the stream state is
   inconsistent, Z_DATA_ERROR if the given dictionary doesn't match the
   expected one (incorrect Adler32 value). inflateSetDictionary does not
   perform any decompression: this will be done by subsequent calls of
   inflate().
}

function inflateSync(var z : z_stream) : int;

{
  Skips invalid compressed data until a full flush point (see above the
  description of deflate with Z_FULL_FLUSH) can be found, or until all
  available input is skipped. No output is provided.

    inflateSync returns Z_OK if a full flush point has been found, Z_BUF_ERROR
  if no more input was provided, Z_DATA_ERROR if no flush point has been found,
  or Z_STREAM_ERROR if the stream structure was inconsistent. In the success
  case, the application may save the current current value of total_in which
  indicates where valid compressed data was found. In the error case, the
  application may repeatedly call inflateSync, providing more input each time,
  until success or end of the input data.
}


function inflateSyncPoint(var z : z_stream) : int;


implementation

uses
  imadler;

function inflateReset(var z : z_stream) : int;
begin
  if (z.state = Z_NULL) then
  begin
    inflateReset :=  Z_STREAM_ERROR;
    exit;
  end;
  z.total_out := 0;
  z.total_in := 0;
  z.msg := '';
  if z.state^.nowrap then
    z.state^.mode := BLOCKS
  else
    z.state^.mode := METHOD;
  inflate_blocks_reset(z.state^.blocks^, z, Z_NULL);
  {$IFDEF DEBUG}
  Tracev('inflate: reset');
  {$ENDIF}
  inflateReset :=  Z_OK;
end;


function inflateEnd(var z : z_stream) : int;
begin
  if (z.state = Z_NULL) or not Assigned(z.zfree) then
  begin
    inflateEnd :=  Z_STREAM_ERROR;
    exit;
  end;
  if (z.state^.blocks <> Z_NULL) then
    inflate_blocks_free(z.state^.blocks, z);
  ZFREE(z, z.state);
  z.state := Z_NULL;
  {$IFDEF DEBUG}
  Tracev('inflate: end');
  {$ENDIF}
  inflateEnd :=  Z_OK;
end;


function inflateInit2_(var z: z_stream;
                       w : int;
                       const version : AnsiString;
                       stream_size : int) : int;
begin
  if (version = '') or (version[1] <> ZLIB_VERSION[1]) or
      (stream_size <> sizeof(z_stream)) then
  begin
    inflateInit2_ := Z_VERSION_ERROR;
    exit;
  end;
  { initialize state }
  { SetLength(strm.msg, 255); }
  z.msg := '';
  if not Assigned(z.zalloc) then
  begin
    {$IFDEF FPC}  z.zalloc := @zcalloc;  {$ELSE}
    z.zalloc := zcalloc;
    {$endif}
    z.opaque := voidpf(0);
  end;
  if not Assigned(z.zfree) then
    {$IFDEF FPC}  z.zfree := @zcfree;  {$ELSE}
    z.zfree := zcfree;
    {$ENDIF}

  z.state := pInternal_state( ZALLOC(z,1,sizeof(internal_state)) );
  if (z.state = Z_NULL) then
  begin
    inflateInit2_ := Z_MEM_ERROR;
    exit;
  end;

  z.state^.blocks := Z_NULL;

  { handle undocumented nowrap option (no zlib header or check) }
  z.state^.nowrap := FALSE;
  if (w < 0) then
  begin
    w := - w;
    z.state^.nowrap := TRUE;
  end;

  { set window size }
  if (w < 8) or (w > 15) then
  begin
    inflateEnd(z);
    inflateInit2_ := Z_STREAM_ERROR;
    exit;
  end;
  z.state^.wbits := uInt(w);

  { create inflate_blocks state }
  if z.state^.nowrap then
    z.state^.blocks := inflate_blocks_new(z, NIL, uInt(1) shl w)
  else
  {$IFDEF FPC}
    z.state^.blocks := inflate_blocks_new(z, @adler32, uInt(1) shl w);
  {$ELSE}
    z.state^.blocks := inflate_blocks_new(z, adler32, uInt(1) shl w);
  {$ENDIF}
  if (z.state^.blocks = Z_NULL) then
  begin
    inflateEnd(z);
    inflateInit2_ := Z_MEM_ERROR;
    exit;
  end;
  {$IFDEF DEBUG}
  Tracev('inflate: allocated');
  {$ENDIF}
  { reset state }
  inflateReset(z);
  inflateInit2_ :=  Z_OK;
end;

function inflateInit2(var z: z_stream; windowBits : int) : int;
begin
  inflateInit2 := inflateInit2_(z, windowBits, ZLIB_VERSION, sizeof(z_stream));
end;


function inflateInit(var z : z_stream) : int;
{ inflateInit is a macro to allow checking the zlib version
  and the compiler's view of z_stream:  }
begin
  inflateInit := inflateInit2_(z, DEF_WBITS, ZLIB_VERSION, sizeof(z_stream));
end;

function inflateInit_(z : z_streamp;
                      const version : AnsiString;
                      stream_size : int) : int;
begin
  { initialize state }
  if (z = Z_NULL) then
    inflateInit_ := Z_STREAM_ERROR
  else
    inflateInit_ := inflateInit2_(z^, DEF_WBITS, version, stream_size);
end;

function inflate(var z : z_stream;
                 f : int) : int;
var
  r : int;
  b : uInt;
begin
  if (z.state = Z_NULL) or (z.next_in = Z_NULL) then
  begin
    inflate := Z_STREAM_ERROR;
    exit;
  end;
  if f = Z_FINISH then
    f := Z_BUF_ERROR
  else
    f := Z_OK;
  r := Z_BUF_ERROR;
  while True do
  case (z.state^.mode) of
    BLOCKS:
      begin
        r := inflate_blocks(z.state^.blocks^, z, r);
        if (r = Z_DATA_ERROR) then
        begin
          z.state^.mode := BAD;
          z.state^.sub.marker := 0;       { can try inflateSync }
          continue;            { break C-switch }
        end;
        if (r = Z_OK) then
          r := f;
        if (r <> Z_STREAM_END) then
        begin
          inflate := r;
          exit;
        end;
        r := f;
        inflate_blocks_reset(z.state^.blocks^, z, @z.state^.sub.check.was);
        if (z.state^.nowrap) then
        begin
          z.state^.mode := DONE;
          continue;            { break C-switch }
        end;
        z.state^.mode := CHECK4;  { falltrough }
      end;
    CHECK4:
      begin
        {NEEDBYTE}
        if (z.avail_in = 0) then
        begin
          inflate := r;
          exit;
        end;
        r := f;

        {z.state^.sub.check.need := uLong(NEXTBYTE(z)) shl 24;}
        Dec(z.avail_in);
        Inc(z.total_in);
        z.state^.sub.check.need := uLong(z.next_in^) shl 24;
        Inc(z.next_in);

        z.state^.mode := CHECK3;   { falltrough }
      end;
    CHECK3:
      begin
        {NEEDBYTE}
        if (z.avail_in = 0) then
        begin
          inflate := r;
          exit;
        end;
        r := f;
        {Inc( z.state^.sub.check.need, uLong(NEXTBYTE(z)) shl 16);}
        Dec(z.avail_in);
        Inc(z.total_in);
        Inc(z.state^.sub.check.need, uLong(z.next_in^) shl 16);
        Inc(z.next_in);

        z.state^.mode := CHECK2;   { falltrough }
      end;
    CHECK2:
      begin
        {NEEDBYTE}
        if (z.avail_in = 0) then
        begin
          inflate := r;
          exit;
        end;
        r := f;

        {Inc( z.state^.sub.check.need, uLong(NEXTBYTE(z)) shl 8);}
        Dec(z.avail_in);
        Inc(z.total_in);
        Inc(z.state^.sub.check.need, uLong(z.next_in^) shl 8);
        Inc(z.next_in);

        z.state^.mode := CHECK1;   { falltrough }
      end;
    CHECK1:
      begin
        {NEEDBYTE}
        if (z.avail_in = 0) then
        begin
          inflate := r;
          exit;
        end;
        r := f;
        {Inc( z.state^.sub.check.need, uLong(NEXTBYTE(z)) );}
        Dec(z.avail_in);
        Inc(z.total_in);
        Inc(z.state^.sub.check.need, uLong(z.next_in^) );
        Inc(z.next_in);


        if (z.state^.sub.check.was <> z.state^.sub.check.need) then
        begin
          z.state^.mode := BAD;
          z.msg := 'incorrect data check';
          z.state^.sub.marker := 5;       { can't try inflateSync }
          continue;           { break C-switch }
        end;
        {$IFDEF DEBUG}
        Tracev('inflate: zlib check ok');
        {$ENDIF}
        z.state^.mode := DONE; { falltrough }
      end;
    DONE:
      begin
        inflate := Z_STREAM_END;
        exit;
      end;
    METHOD:
      begin
        {NEEDBYTE}
        if (z.avail_in = 0) then
        begin
          inflate := r;
          exit;
        end;
        r := f; {}

        {z.state^.sub.method := NEXTBYTE(z);}
        Dec(z.avail_in);
        Inc(z.total_in);
        z.state^.sub.method := z.next_in^;
        Inc(z.next_in);

        if ((z.state^.sub.method and $0f) <> Z_DEFLATED) then
        begin
          z.state^.mode := BAD;
          z.msg := 'unknown compression method';
          z.state^.sub.marker := 5;       { can't try inflateSync }
          continue;  { break C-switch }
        end;
        if ((z.state^.sub.method shr 4) + 8 > z.state^.wbits) then
        begin
          z.state^.mode := BAD;
          z.msg := 'invalid window size';
          z.state^.sub.marker := 5;       { can't try inflateSync }
          continue; { break C-switch }
        end;
        z.state^.mode := FLAG;
        { fall trough }
      end;
    FLAG:
      begin
        {NEEDBYTE}
        if (z.avail_in = 0) then
        begin
          inflate := r;
          exit;
        end;
        r := f; {}
        {b := NEXTBYTE(z);}
        Dec(z.avail_in);
        Inc(z.total_in);
        b := z.next_in^;
        Inc(z.next_in);

        if (((z.state^.sub.method shl 8) + b) mod 31) <> 0 then {% mod ?}
        begin
          z.state^.mode := BAD;
          z.msg := 'incorrect header check';
          z.state^.sub.marker := 5;       { can't try inflateSync }
          continue;      { break C-switch }
        end;
        {$IFDEF DEBUG}
        Tracev('inflate: zlib header ok');
        {$ENDIF}
        if ((b and PRESET_DICT) = 0) then
        begin
          z.state^.mode := BLOCKS;
	  continue;      { break C-switch }
        end;
        z.state^.mode := DICT4;
        { falltrough }
      end;
    DICT4:
      begin
        if (z.avail_in = 0) then
        begin
          inflate := r;
          exit;
        end;
        r := f;

        {z.state^.sub.check.need := uLong(NEXTBYTE(z)) shl 24;}
        Dec(z.avail_in);
        Inc(z.total_in);
        z.state^.sub.check.need :=  uLong(z.next_in^) shl 24;
        Inc(z.next_in);

        z.state^.mode := DICT3;        { falltrough }
      end;
    DICT3:
      begin
        if (z.avail_in = 0) then
        begin
          inflate := r;
          exit;
        end;
        r := f;
        {Inc(z.state^.sub.check.need, uLong(NEXTBYTE(z)) shl 16);}
        Dec(z.avail_in);
        Inc(z.total_in);
        Inc(z.state^.sub.check.need, uLong(z.next_in^) shl 16);
        Inc(z.next_in);

        z.state^.mode := DICT2;        { falltrough }
      end;
    DICT2:
      begin
        if (z.avail_in = 0) then
        begin
          inflate := r;
          exit;
        end;
        r := f;

        {Inc(z.state^.sub.check.need, uLong(NEXTBYTE(z)) shl 8);}
        Dec(z.avail_in);
        Inc(z.total_in);
        Inc(z.state^.sub.check.need, uLong(z.next_in^) shl 8);
        Inc(z.next_in);

        z.state^.mode := DICT1;        { falltrough }
      end;
    DICT1:
      begin
        if (z.avail_in = 0) then
        begin
          inflate := r;
          exit;
        end;
        { r := f;    ---  wird niemals benutzt }
        {Inc(z.state^.sub.check.need, uLong(NEXTBYTE(z)) );}
        Dec(z.avail_in);
        Inc(z.total_in);
        Inc(z.state^.sub.check.need, uLong(z.next_in^) );
        Inc(z.next_in);

        z.adler := z.state^.sub.check.need;
        z.state^.mode := DICT0;
        inflate := Z_NEED_DICT;
        exit;
      end;
    DICT0:
      begin
        z.state^.mode := BAD;
        z.msg := 'need dictionary';
        z.state^.sub.marker := 0;         { can try inflateSync }
        inflate := Z_STREAM_ERROR;
        exit;
      end;
    BAD:
      begin
        inflate := Z_DATA_ERROR;
        exit;
      end;
    else
      begin
        inflate := Z_STREAM_ERROR;
        exit;
      end;
  end;
{$ifdef NEED_DUMMY_result}
  result := Z_STREAM_ERROR;  { Some dumb compilers complain without this }
{$endif}
end;

function inflateSetDictionary(var z : z_stream;
                              dictionary : pBytef; {const array of byte}
                              dictLength : uInt) : int;
var
  length : uInt;
begin
  length := dictLength;

  if (z.state = Z_NULL) or (z.state^.mode <> DICT0) then
  begin
    inflateSetDictionary := Z_STREAM_ERROR;
    exit;
  end;
  if (adler32(Long(1), dictionary, dictLength) <> z.adler) then
  begin
    inflateSetDictionary := Z_DATA_ERROR;
    exit;
  end;
  z.adler := Long(1);

  if (length >= (uInt(1) shl z.state^.wbits)) then
  begin
    length := (1 shl z.state^.wbits)-1;
    Inc( dictionary, dictLength - length);
  end;
  inflate_set_dictionary(z.state^.blocks^, dictionary^, length);
  z.state^.mode := BLOCKS;
  inflateSetDictionary := Z_OK;
end;


function inflateSync(var z : z_stream) : int;
const
  mark : packed array[0..3] of byte = (0, 0, $ff, $ff);
var
  n : uInt;       { number of bytes to look at }
  p : pBytef;     { pointer to bytes }
  m : uInt;       { number of marker bytes found in a row }
  r, w : uLong;   { temporaries to save total_in and total_out }
begin
  { set up }
  if (z.state = Z_NULL) then
  begin
    inflateSync := Z_STREAM_ERROR;
    exit;
  end;
  if (z.state^.mode <> BAD) then
  begin
    z.state^.mode := BAD;
    z.state^.sub.marker := 0;
  end;
  n := z.avail_in;
  if (n = 0) then
  begin
    inflateSync := Z_BUF_ERROR;
    exit;
  end;
  p := z.next_in;
  m := z.state^.sub.marker;

  { search }
  while (n <> 0) and (m < 4) do
  begin
    if (p^ = mark[m]) then
      Inc(m)
    else
      if (p^ <> 0) then
        m := 0
      else
        m := 4 - m;
    Inc(p);
    Dec(n);
  end;

  { restore }
  Inc(z.total_in, ptr2int(p) - ptr2int(z.next_in));
  z.next_in := p;
  z.avail_in := n;
  z.state^.sub.marker := m;


  { return no joy or set up to restart on a new block }
  if (m <> 4) then
  begin
    inflateSync := Z_DATA_ERROR;
    exit;
  end;
  r := z.total_in;
  w := z.total_out;
  inflateReset(z);
  z.total_in := r;
  z.total_out := w;
  z.state^.mode := BLOCKS;
  inflateSync := Z_OK;
end;


{
  returns true if inflate is currently at the end of a block generated
  by Z_SYNC_FLUSH or Z_FULL_FLUSH. This function is used by one PPP
  implementation to provide an additional safety check. PPP uses Z_SYNC_FLUSH
  but removes the length bytes of the resulting empty stored block. When
  decompressing, PPP checks that at the end of input packet, inflate is
  waiting for these length bytes.
}

function inflateSyncPoint(var z : z_stream) : int;
begin
  if (z.state = Z_NULL) or (z.state^.blocks = Z_NULL) then
  begin
    inflateSyncPoint := Z_STREAM_ERROR;
    exit;
  end;
  inflateSyncPoint := inflate_blocks_sync_point(z.state^.blocks^);
end;

end.
