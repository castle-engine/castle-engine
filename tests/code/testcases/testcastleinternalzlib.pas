{
  Copyright 2026 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.md,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Test CastleInternalZLib unit: the zlib C library through our bindings. }
unit TestCastleInternalZLib;

{$I ../../../src/common_includes/castleconf.inc}

interface

uses Classes, SysUtils,
  CastleTester;

type
  TTestCastleInternalZLib = class(TCastleTestCase)
  published
    { TZStream must have the size and layout of C z_stream,
      zlib checks the size given to deflateInit_ / inflateInit_. }
    procedure TestZStreamSize;
    { Compress and decompress with deflate / inflate (raw deflate,
      without zlib header, like ZIP and gzip use). }
    procedure TestDeflateInflate;
    { Compress and decompress with compress / uncompress. }
    procedure TestCompressUncompress;
  end;

implementation

{$ifndef CASTLE_ZLIB_USING_PASZLIB}
uses CastleInternalZLib;
{$endif}

{$ifndef CASTLE_ZLIB_USING_PASZLIB}

{ Some data that compresses: text with repetitions. }
function SampleData: TBytes;
var
  S: AnsiString;
  I: Integer;
begin
  S := '';
  for I := 1 to 200 do
    S := S + 'Castle Game Engine zlib test line ' + AnsiString(IntToStr(I mod 17)) + #10;
  SetLength(Result, Length(S));
  Move(S[1], Result[0], Length(S));
end;

{$endif}

procedure TTestCastleInternalZLib.TestZStreamSize;
begin
  {$ifndef CASTLE_ZLIB_USING_PASZLIB}
  { C z_stream: 7 pointers, 2 uInt, 4 uLong, 1 int, with natural alignment.
    uLong is "unsigned long": 32-bit on Windows (also 64-bit),
    64-bit on 64-bit Unix. }
  {$if defined(CPU64) and defined(MSWINDOWS)}
  AssertEquals(88, SizeOf(TZStream));
  {$elseif defined(CPU64)}
  AssertEquals(112, SizeOf(TZStream));
  {$else}
  AssertEquals(56, SizeOf(TZStream));
  {$endif}
  {$endif}
end;

procedure TTestCastleInternalZLib.TestDeflateInflate;
{$ifndef CASTLE_ZLIB_USING_PASZLIB}
var
  Data, Compressed, Unpacked: TBytes;
  Z: TZStream;
  PackedSize: Integer;
begin
  if not CastleZLibInitialized then
  begin
    WriteLn('zlib library not available, skipping TestDeflateInflate');
    Exit;
  end;
  Data := SampleData;

  { compress (raw deflate: negative window bits) }
  SetLength(Compressed, Length(Data) + 1024);
  FillChar(Z, SizeOf(Z), 0);
  Z.next_in := @Data[0];
  Z.avail_in := Length(Data);
  Z.next_out := @Compressed[0];
  Z.avail_out := Length(Compressed);
  { Before fixing TZStream layout, this failed on 64-bit with Z_VERSION_ERROR. }
  AssertEquals(Z_OK, deflateInit2(Z, Z_BEST_COMPRESSION, Z_DEFLATED, -15, 8, Z_DEFAULT_STRATEGY));
  try
    AssertEquals(Z_STREAM_END, deflate(Z, Z_FINISH));
    PackedSize := Length(Compressed) - Integer(Z.avail_out);
    AssertEquals(Int64(PackedSize), Int64(Z.total_out));
  finally
    AssertEquals(Z_OK, deflateEnd(Z));
  end;
  AssertTrue(PackedSize < Length(Data) div 4);

  { decompress }
  SetLength(Unpacked, Length(Data));
  FillChar(Z, SizeOf(Z), 0);
  Z.next_in := @Compressed[0];
  Z.avail_in := PackedSize;
  Z.next_out := @Unpacked[0];
  Z.avail_out := Length(Unpacked);
  AssertEquals(Z_OK, inflateInit2(Z, -15));
  try
    AssertEquals(Z_STREAM_END, inflate(Z, Z_FINISH));
    AssertEquals(0, Integer(Z.avail_out));
    AssertEquals(Int64(Length(Data)), Int64(Z.total_out));
  finally
    AssertEquals(Z_OK, inflateEnd(Z));
  end;
  AssertTrue(CompareMem(@Data[0], @Unpacked[0], Length(Data)));
end;
{$else}
begin
end;
{$endif}

procedure TTestCastleInternalZLib.TestCompressUncompress;
{$ifndef CASTLE_ZLIB_USING_PASZLIB}
var
  Data, Compressed, Unpacked: TBytes;
  PackedSize, UnpackedSize: Ulongf;
begin
  if not CastleZLibInitialized then
  begin
    WriteLn('zlib library not available, skipping TestCompressUncompress');
    Exit;
  end;
  Data := SampleData;
  SetLength(Compressed, Length(Data) + 1024);
  PackedSize := Length(Compressed);
  AssertEquals(Z_OK, compress(@Compressed[0], @PackedSize, @Data[0], Length(Data)));
  AssertTrue(PackedSize < Ulongf(Length(Data) div 4));

  SetLength(Unpacked, Length(Data));
  UnpackedSize := Length(Unpacked);
  AssertEquals(Z_OK, uncompress(@Unpacked[0], @UnpackedSize, @Compressed[0], PackedSize));
  AssertEquals(Int64(Length(Data)), Int64(UnpackedSize));
  AssertTrue(CompareMem(@Data[0], @Unpacked[0], Length(Data)));
end;
{$else}
begin
end;
{$endif}

initialization
  RegisterTest(TTestCastleInternalZLib);
end.
