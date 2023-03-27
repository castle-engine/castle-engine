program example;

{ example.c -- usage example of the zlib compression library
  Copyright (C) 1995-1998 Jean-loup Gailly.

  Pascal tranlastion
  Copyright (C) 1998 by Jacques Nomssi Nzali
  For conditions of distribution and use, see copyright notice in readme.txt
}
{-$define MemCheck}
{$DEFINE TEST_COMPRESS}
{$DEFINE TEST_GZIO}
{$DEFINE TEST_INFLATE}
{$DEFINE TEST_DEFLATE}
{$DEFINE TEST_SYNC}
{$DEFINE TEST_DICT}
{$DEFINE TEST_FLUSH}

uses
  strings,
  zbase,
  gzio,
  zinflate,
  zdeflate,
  zcompres,
  zuncompr
{$ifdef memcheck}
  , memcheck in '..\..\monotekt\pas\memcheck\memcheck.pas'
{$endif}
;

procedure Stop;
begin
  Write('Program halted...');
  ReadLn;
  Halt(1);
end;

procedure CHECK_ERR(err : integer; msg : string);
begin
  if (err <> Z_OK) then
  begin
    Write(msg, ' error: ', err);
    Stop;
  end;
end;

const
  hello : PChar = 'hello, hello!';
{ "hello world" would be more standard, but the repeated "hello"
  stresses the compression code better, sorry... }

{$IFDEF TEST_DICT}
const
  dictionary : PChar = 'hello';
var
  dictId : cardinal; { Adler32 value of the dictionary }
{$ENDIF}

{ ===========================================================================
  Test compress() and uncompress() }

{$IFDEF TEST_COMPRESS}
procedure test_compress(compr : Pbyte; var comprLen : cardinal;
                        uncompr : Pbyte; uncomprLen : cardinal);
var
  err : integer;
  len : cardinal;
begin
  len := strlen(hello)+1;
  err := compress(compr, comprLen, Pbyte(hello)^, len);
  CHECK_ERR(err, 'compress');

  strcopy(PChar(uncompr), 'garbage');

  err := uncompress(uncompr, uncomprLen, compr^, comprLen);
  CHECK_ERR(err, 'uncompress');

  if (strcomp(PChar(uncompr), hello)) <> 0 then
  begin
    WriteLn('bad uncompress');
    Stop;
  end
  else
    WriteLn('uncompress(): ', StrPas(PChar(uncompr)));
end;
{$ENDIF}

{ ===========================================================================
  Test read/write of .gz files }

{$IFDEF TEST_GZIO}
procedure test_gzio(const outf : string; { output file }
                    const inf : string;  { input file }
                    uncompr : Pbyte;
                    uncomprLen : integer);
var
  err : integer;
  len : integer;
var
  zfile : gzFile;
  pos : z_off_t;
begin
  len := strlen(hello)+1;

  zfile := gzopen(outf, 'w');
  if (zfile = NIL) then
  begin
    WriteLn('_gzopen error');
    Stop;
  end;
  gzputc(zfile, 'h');
  if (gzputs(zfile, 'ello') <> 4) then
  begin
    WriteLn('gzputs err: ', gzerror(zfile, err));
    Stop;
  end;
  {$ifdef GZ_FORMAT_STRING}
  if (gzprintf(zfile, ', %s!', 'hello') <> 8) then
  begin
    WriteLn('gzprintf err: ', gzerror(zfile, err));
    Stop;
  end;
  {$else}
  if (gzputs(zfile, ', hello!') <> 8) then
  begin
    WriteLn('gzputs err: ', gzerror(zfile, err));
    Stop;
  end;
  {$ENDIF}
  gzseek(zfile, longint(1), SEEK_CUR); { add one zero byte }
  gzclose(zfile);

  zfile := gzopen(inf, 'r');
  if (zfile = NIL) then
    WriteLn('gzopen error');

  strcopy(pchar(uncompr), 'garbage');

  uncomprLen := gzread(zfile, uncompr, cardinal(uncomprLen));
  if (uncomprLen <> len) then
  begin
    WriteLn('gzread err: ', gzerror(zfile, err));
    Stop;
  end;
  if (strcomp(pchar(uncompr), hello)) <> 0 then
  begin
    WriteLn('bad gzread: ', pchar(uncompr));
    Stop;
  end
  else
    WriteLn('gzread(): ', pchar(uncompr));

  pos := gzseek(zfile, longint(-8), SEEK_CUR);
  if (pos <> 6) or (gztell(zfile) <> pos) then
  begin
    WriteLn('gzseek error, pos=',pos,', gztell=',gztell(zfile));
    Stop;
  end;

  if (char(gzgetc(zfile)) <> ' ') then
  begin
    WriteLn('gzgetc error');
    Stop;
  end;

  gzgets(zfile, pchar(uncompr), uncomprLen);
  uncomprLen := strlen(pchar(uncompr));
  if (uncomprLen <> 6) then
  begin { "hello!" }
    WriteLn('gzgets err after gzseek: ', gzerror(zfile, err));
    Stop;
  end;
  if (strcomp(pchar(uncompr), hello+7)) <> 0 then
  begin
    WriteLn('bad gzgets after gzseek');
    Stop;
  end
  else
    WriteLn('gzgets() after gzseek: ', PChar(uncompr));

  gzclose(zfile);
end;
{$ENDIF}

{ ===========================================================================
  Test deflate() with small buffers }

{$IFDEF TEST_DEFLATE}
procedure test_deflate(compr : Pbyte; comprLen : cardinal);
var
  c_stream : z_stream; { compression stream }
  err : integer;
  len : integer;
begin
  len := strlen(hello)+1;

  err := deflateInit(c_stream, Z_DEFAULT_COMPRESSION);
  CHECK_ERR(err, 'deflateInit');

  c_stream.next_in  := Pbyte(hello);
  c_stream.next_out := compr;

  while (c_stream.total_in <> cardinal(len)) and (c_stream.total_out < comprLen) do
  begin
    c_stream.avail_out := 1; { force small buffers }
    c_stream.avail_in := 1;
    err := deflate(c_stream, Z_NO_FLUSH);
    CHECK_ERR(err, 'deflate');
  end;

  { Finish the stream, still forcing small buffers: }
  while TRUE do
  begin
    c_stream.avail_out := 1;
    err := deflate(c_stream, Z_FINISH);
    if (err = Z_STREAM_END) then
      break;
    CHECK_ERR(err, 'deflate');
  end;

  err := deflateEnd(c_stream);
  CHECK_ERR(err, 'deflateEnd');
end;
{$ENDIF}

{ ===========================================================================
  Test inflate() with small buffers
}

{$IFDEF TEST_INFLATE}
procedure test_inflate(compr : Pbyte; comprLen : cardinal;
                       uncompr : Pbyte;  uncomprLen : cardinal);
var
  err : integer;
  d_stream : z_stream; { decompression stream }
begin
  strcopy(PChar(uncompr), 'garbage');

  d_stream.next_in  := compr;
  d_stream.avail_in := 0;
  d_stream.next_out := uncompr;

  err := inflateInit(d_stream);
  CHECK_ERR(err, 'inflateInit');

  while (d_stream.total_out < uncomprLen) and
        (d_stream.total_in < comprLen) do
  begin
    d_stream.avail_out := 1; { force small buffers }
    d_stream.avail_in := 1;
    err := inflate(d_stream, Z_NO_FLUSH);
    if (err = Z_STREAM_END) then
      break;
    CHECK_ERR(err, 'inflate');
  end;

  err := inflateEnd(d_stream);
  CHECK_ERR(err, 'inflateEnd');

  if (strcomp(PChar(uncompr), hello) <> 0) then
  begin
    WriteLn('bad inflate');
    exit;
  end
  else
  begin
    WriteLn('inflate(): ', StrPas(PChar(uncompr)));
  end;
end;
{$ENDIF}

{ ===========================================================================
  Test deflate() with large buffers and dynamic change of compression level
 }

{$IFDEF TEST_DEFLATE}
procedure test_large_deflate(compr : Pbyte; comprLen : cardinal;
                             uncompr : Pbyte;  uncomprLen : cardinal);
var
  c_stream : z_stream; { compression stream }
  err : integer;
begin
  err := deflateInit(c_stream, Z_BEST_SPEED);
  CHECK_ERR(err, 'deflateInit');

  c_stream.next_out := compr;
  c_stream.avail_out := cardinal(comprLen);

  { At this point, uncompr is still mostly zeroes, so it should compress
    very well: }

  c_stream.next_in := uncompr;
  c_stream.avail_in := cardinal(uncomprLen);
  err := deflate(c_stream, Z_NO_FLUSH);
  CHECK_ERR(err, 'deflate');
  if (c_stream.avail_in <> 0) then
  begin
    WriteLn('deflate not greedy');
    exit;
  end;

  { Feed in already compressed data and switch to no compression: }
  deflateParams(c_stream, Z_NO_COMPRESSION, Z_DEFAULT_STRATEGY);
  c_stream.next_in := compr;
  c_stream.avail_in := cardinal(comprLen div 2);
  err := deflate(c_stream, Z_NO_FLUSH);
  CHECK_ERR(err, 'deflate');

  { Switch back to compressing mode: }
  deflateParams(c_stream, Z_BEST_COMPRESSION, Z_FILTERED);
  c_stream.next_in := uncompr;
  c_stream.avail_in := cardinal(uncomprLen);
  err := deflate(c_stream, Z_NO_FLUSH);
  CHECK_ERR(err, 'deflate');

  err := deflate(c_stream, Z_FINISH);
  if (err <> Z_STREAM_END) then
  begin
    WriteLn('deflate should report Z_STREAM_END');
    exit;
  end;
  err := deflateEnd(c_stream);
  CHECK_ERR(err, 'deflateEnd');
end;
{$ENDIF}

{ ===========================================================================
  Test inflate() with large buffers }

{$IFDEF TEST_INFLATE}
procedure test_large_inflate(compr : Pbyte; comprLen : cardinal;
                             uncompr : Pbyte;  uncomprLen : cardinal);
var
  err : integer;
  d_stream : z_stream; { decompression stream }
begin
  strcopy(PChar(uncompr), 'garbage');

  d_stream.next_in  := compr;
  d_stream.avail_in := cardinal(comprLen);

  err := inflateInit(d_stream);
  CHECK_ERR(err, 'inflateInit');

  while TRUE do
  begin
    d_stream.next_out := uncompr;            { discard the output }
    d_stream.avail_out := cardinal(uncomprLen);
    err := inflate(d_stream, Z_NO_FLUSH);
    if (err = Z_STREAM_END) then
      break;
    CHECK_ERR(err, 'large inflate');
  end;

  err := inflateEnd(d_stream);
  CHECK_ERR(err, 'inflateEnd');

  if (d_stream.total_out <> 2*uncomprLen + comprLen div 2) then
  begin
    WriteLn('bad large inflate: ', d_stream.total_out);
    Stop;
  end
  else
    WriteLn('large_inflate(): OK');
end;
{$ENDIF}

{ ===========================================================================
  Test deflate() with full flush
 }
{$IFDEF TEST_FLUSH}
procedure test_flush(compr : Pbyte; var comprLen : cardinal);
var
  c_stream : z_stream; { compression stream }
  err : integer;
  len : integer;

begin
  len := strlen(hello)+1;
  err := deflateInit(c_stream, Z_DEFAULT_COMPRESSION);
  CHECK_ERR(err, 'deflateInit');

  c_stream.next_in := Pbyte(hello);
  c_stream.next_out := compr;
  c_stream.avail_in := 3;
  c_stream.avail_out := cardinal(comprLen);

  err := deflate(c_stream, Z_FULL_FLUSH);
  CHECK_ERR(err, 'deflate');

  Inc(pchar(compr)[3]); { force an error in first compressed block }
  c_stream.avail_in := len - 3;

  err := deflate(c_stream, Z_FINISH);
  if (err <> Z_STREAM_END) then
    CHECK_ERR(err, 'deflate');

  err := deflateEnd(c_stream);
    CHECK_ERR(err, 'deflateEnd');

  comprLen := c_stream.total_out;
end;
{$ENDIF}

{ ===========================================================================
  Test inflateSync()
 }
{$IFDEF TEST_SYNC}
procedure test_sync(compr : Pbyte; comprLen : cardinal;
                    uncompr : Pbyte; uncomprLen : cardinal);
var
  err : integer;
  d_stream : z_stream; { decompression stream }
begin
  strcopy(PChar(uncompr), 'garbage');

  d_stream.next_in  := compr;
  d_stream.avail_in := 2; { just read the zlib header }

  err := inflateInit(d_stream);
  CHECK_ERR(err, 'inflateInit');

  d_stream.next_out := uncompr;
  d_stream.avail_out := cardinal(uncomprLen);

  inflate(d_stream, Z_NO_FLUSH);
  CHECK_ERR(err, 'inflate');

  d_stream.avail_in := cardinal(comprLen-2);   { read all compressed data }
  err := inflateSync(d_stream);           { but skip the damaged part }
  CHECK_ERR(err, 'inflateSync');

  err := inflate(d_stream, Z_FINISH);
  if (err <> Z_DATA_ERROR) then
  begin
    WriteLn('inflate should report DATA_ERROR');
      { Because of incorrect adler32 }
    Stop;
  end;
  err := inflateEnd(d_stream);
  CHECK_ERR(err, 'inflateEnd');

  WriteLn('after inflateSync(): hel', StrPas(PChar(uncompr)));
end;
{$ENDIF}

{ ===========================================================================
  Test deflate() with preset dictionary
 }
{$IFDEF TEST_DICT}
procedure test_dict_deflate(compr : Pbyte; comprLen : cardinal);
var
  c_stream : z_stream; { compression stream }
  err : integer;
begin
  err := deflateInit(c_stream, Z_BEST_COMPRESSION);
  CHECK_ERR(err, 'deflateInit');

  err := deflateSetDictionary(c_stream,
                              Pbyte(dictionary), StrLen(dictionary));
  CHECK_ERR(err, 'deflateSetDictionary');

  dictId := c_stream.adler;
  c_stream.next_out := compr;
  c_stream.avail_out := cardinal(comprLen);

  c_stream.next_in := Pbyte(hello);
  c_stream.avail_in := cardinal(strlen(hello)+1);

  err := deflate(c_stream, Z_FINISH);
  if (err <> Z_STREAM_END) then
  begin
    WriteLn('deflate should report Z_STREAM_END');
    exit;
  end;
  err := deflateEnd(c_stream);
  CHECK_ERR(err, 'deflateEnd');
end;

{ ===========================================================================
  Test inflate() with a preset dictionary }

procedure test_dict_inflate(compr : Pbyte; comprLen : cardinal;
                            uncompr : Pbyte; uncomprLen : cardinal);
var
  err : integer;
  d_stream : z_stream; { decompression stream }
begin
  strcopy(PChar(uncompr), 'garbage');

  d_stream.next_in  := compr;
  d_stream.avail_in := cardinal(comprLen);

  err := inflateInit(d_stream);
  CHECK_ERR(err, 'inflateInit');

  d_stream.next_out := uncompr;
  d_stream.avail_out := cardinal(uncomprLen);

  while TRUE do
  begin
    err := inflate(d_stream, Z_NO_FLUSH);
    if (err = Z_STREAM_END) then
      break;
    if (err = Z_NEED_DICT) then
    begin
      if (d_stream.adler <> dictId) then
      begin
        WriteLn('unexpected dictionary');
	Stop;
      end;
      err := inflateSetDictionary(d_stream, Pbyte(dictionary),
				     StrLen(dictionary));
    end;
    CHECK_ERR(err, 'inflate with dict');
  end;

  err := inflateEnd(d_stream);
  CHECK_ERR(err, 'inflateEnd');

  if (strcomp(PChar(uncompr), hello)) <> 0 then
  begin
    WriteLn('bad inflate with dict');
    Stop;
  end
  else
  begin
    WriteLn('inflate with dictionary: ', StrPas(PChar(uncompr)));
  end;
end;
{$ENDIF}

function GetFromFile(buf : Pbyte; FName : string;
                     var MaxLen : cardinal) : boolean;
const
  zOfs = 0;
var
  f : file;
  Len : cardinal;
begin
  assign(f, FName);
  GetFromFile := false;
  {$I-}
  filemode := 0; { read only }
  reset(f, 1);
  if IOresult = 0 then
  begin
    Len := FileSize(f)-zOfs;
    Seek(f, zOfs);
    if Len < MaxLen then
      MaxLen := Len;
    BlockRead(f, buf^, MaxLen);
    close(f);
    WriteLn(FName);
    GetFromFile := (IOresult = 0) and (MaxLen > 0);
  end
  else
    WriteLn('Could not open ', FName);
end;

{ ===========================================================================
  Usage:  example [output.gz  [input.gz]]
}

var
  compr, uncompr : Pbyte;
const
  msdoslen = 25000;
  comprLenL : cardinal = msdoslen div sizeof(cardinal); { don't overflow on MSDOS }
  uncomprLenL : cardinal = msdoslen div sizeof(cardinal);
var
  zVersion,
  myVersion : string;
var
  comprLen : cardinal;
  uncomprLen : cardinal;
begin
  {$ifdef MemCheck}
  MemChk;
  {$endif}
  comprLen := comprLenL;
  uncomprLen := uncomprLenL;

  myVersion := ZLIB_VERSION;
  zVersion := zlibVersion;
  if (zVersion[1] <> myVersion[1]) then
  begin
    WriteLn('incompatible zlib version');
    Stop;
  end
  else
    if (zVersion <> ZLIB_VERSION) then
    begin
      WriteLn('warning: different zlib version');
    end;

  GetMem(compr, comprLen*sizeof(cardinal));
  GetMem(uncompr, uncomprLen*sizeof(cardinal));
  { compr and uncompr are cleared to avoid reading uninitialized
    data and to ensure that uncompr compresses well. }

  if (compr = nil) or (uncompr = nil) then
  begin
    WriteLn('out of memory');
    Stop;
  end;
  FillChar(compr^, comprLen*sizeof(cardinal), 0);
  FillChar(uncompr^, uncomprLen*sizeof(cardinal), 0);

  if (compr = nil) or (uncompr = nil) then
  begin
    WriteLn('out of memory');
    Stop;
  end;
  {$IFDEF TEST_COMPRESS}
  test_compress(compr, comprLenL, uncompr, uncomprLen);
  {$ENDIF}

  {$IFDEF TEST_GZIO}
  Case ParamCount of
    0:  test_gzio('foo.gz', 'foo.gz', uncompr, integer(uncomprLen));
    1:  test_gzio(ParamStr(1), 'foo.gz', uncompr, integer(uncomprLen));
  else
    test_gzio(ParamStr(1), ParamStr(2), uncompr, integer(uncomprLen));
  end;
  {$ENDIF}

  {$IFDEF TEST_DEFLATE}
  WriteLn('small buffer Deflate');
  test_deflate(compr, comprLen);
  {$ENDIF}
  {$IFDEF TEST_INFLATE}
  {$IFNDEF TEST_DEFLATE}
  WriteLn('small buffer Inflate');
  if GetFromFile(compr, 'u:\nomssi\paszlib\new\test0.z', comprLen) then
  {$ENDIF}
    test_inflate(compr, comprLen, uncompr, uncomprLen);
  {$ENDIF}
  readln;
  {$IFDEF TEST_DEFLATE}
  WriteLn('large buffer Deflate');
  test_large_deflate(compr, comprLen, uncompr, uncomprLen);
  {$ENDIF}
  {$IFDEF TEST_INFLATE}
  WriteLn('large buffer Inflate');
  test_large_inflate(compr, comprLen, uncompr, uncomprLen);
  {$ENDIF}
  {$IFDEF TEST_FLUSH}
  test_flush(compr, comprLenL);
  {$ENDIF}
  {$IFDEF TEST_SYNC}
  test_sync(compr, comprLen, uncompr, uncomprLen);
  {$ENDIF}
  comprLen := uncomprLen;

  {$IFDEF TEST_DICT}
  test_dict_deflate(compr, comprLen);
  test_dict_inflate(compr, comprLen, uncompr, uncomprLen);
  {$ENDIF}
  readln;
  FreeMem(compr, comprLen*sizeof(cardinal));
  FreeMem(uncompr, uncomprLen*sizeof(cardinal));
end.
