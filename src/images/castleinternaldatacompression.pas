{
  Copyright 2023-2024 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Internal compression algorithms.

  For now only used to compress embedded images in EXE,
  to make units like src/fonts/castletexturefont_defaultui.pas
  smaller (faster to compile, and not taking huge space in EXE). }
unit CastleInternalDataCompression;

{$I castleconf.inc}

interface

uses SysUtils, Classes, Generics.Collections;

type
  TChannelsSplit = class
    Data: array of Pointer;
    { Every Data[I] has this size. }
    DataSize: Cardinal;

    { Allocate all Data[I] arrays using GetMem.

      Note that this is not the only way to initialize Data[I] arrays,
      one can also set Data[I] to pointers to some hardcoded arrays.
      Though this use-case is not actually used now in the end. }
    procedure DataAllocate;

    { Free all Data[I] arrays using FreeMem. }
    procedure DataFree;
  end;

{ Split data into per-channel arrays.
  This is good to apply RLE compression to each image channel separately,
  this makes compression most efficient, e.g. RGB image will compress great
  when it contains the single color.

  The TChannelsSplit has allocated data with DataAllocate,
  so remember to call DataFree when you're done with it.

  While this routine technically doesn't assume anything about "what is the data
  content", but in practice it is useful only with TCastleImage.RawPixels data.
  It splits the data following the TCastleImage.RawPixels layout,
  i.e. assuming that data is a sequence of pixels, each pixel having PixelSize
  bytes. }
function DataChannelsSplit(const RawPixels: Pointer; const RawPixelsSize: Cardinal;
  const PixelSize: Integer): TChannelsSplit;

{ Fill the RawPixels memory by data from ChannelData.
  Reverses the operation of DataChannelsSplit. }
procedure DataChannelsCombine(const RawPixels: Pointer; const RawPixelsSize: Cardinal;
  const PixelSize: Integer; const ChannelData: TChannelsSplit);

{ RLE compression / decompression.

  Design goals:

  - Simple to implement, so we can implement it in Pascal in CGE,
    without depending on any external libs or any FPC/Delphi-specific units.
    So can be used with all platforms, compilers, easily.

  - Fast to decompress.
    Compression itself is also fast, but isn't a priority.
    In our main use-case, compression occurs at pre-processing stage
    (texture-to-pascal, font-to-pascal) while decompression occurs at runtime
    (almost every CGE application will need to decompress to use FallbackFont).

  - Efficient compression of typical image data,
    where we have often long sequences of the same byte.
    We are not trying to get the best compression ratio,
    we are useful only with specialized data that is really easy to compress.
    See https://en.wikipedia.org/wiki/Run-length_encoding .

  - See implementation comments for the exact algorithm.
    This algorithm should be considered an implementation detail only!

  Compatibility:

  This is not a stable algorithm for now, do not count on it being backward
  compatible between CGE releases, do not compress your files with it!
  It is used internally in CGE only, to temporarily compress some data.
  When we use it, we always depend that compression + decompression
  was done by the same CGE version.

  Stream usage:

  Since the primary use-case is to compress / decompress data already in memory,
  we assume that whole input is already in memory with known size.
  So we don't try to read source from a general TStream (with unknown size),
  or even TMemoryStream. The Source is just a pointer with content size.

  SourceSize is limited to 4 GB (max Cardinal) to allow fast iteration
  using 32-bit Integers.

  @groupBegin }
procedure RleCompress(
  const Source: Pointer; const SourceSize: Cardinal; const Dest: TStream);
procedure RleDecompress(
  const Source: Pointer; const SourceSize: Cardinal; const Dest: TStream); overload;
procedure RleDecompress(
  const Source: Pointer; const SourceSize: Cardinal;
  const Dest: Pointer; const DestSize: Cardinal); overload;
{ @groupEnd }

implementation

uses CastleUtils;

{ TChannelsSplit ------------------------------------------------------------- }

procedure TChannelsSplit.DataAllocate;
var
  I: Integer;
begin
  for I := 0 to High(Data) do
    Data[I] := GetMem(DataSize);
end;

procedure TChannelsSplit.DataFree;
var
  I: Integer;
begin
  for I := 0 to High(Data) do
    FreeMemNiling(Data[I]);
end;

{ DataChannelsSplit / DataChannelsCombine ------------------------------------ }

function DataChannelsSplit(const RawPixels: Pointer; const RawPixelsSize: Cardinal;
  const PixelSize: Integer): TChannelsSplit;
var
  Channel: Integer;
  SourcePtr, DestPtr: PByte;
  I: Integer;
begin
  Result := TChannelsSplit.Create;
  try
    { prepare TChannelsSplit memory }
    SetLength(Result.Data, PixelSize);
    Result.DataSize := RawPixelsSize div PixelSize;
    Assert(RawPixelsSize mod PixelSize = 0);
    Result.DataAllocate;

    for Channel := 0 to PixelSize - 1 do
    begin
      // fill Result.Data[Channel] memory contents
      SourcePtr := RawPixels;
      Inc(SourcePtr, Channel);
      DestPtr := Result.Data[Channel];
      for I := 0 to Result.DataSize - 1 do
      begin
        DestPtr^ := SourcePtr^;
        Inc(SourcePtr, PixelSize);
        Inc(DestPtr);
      end;
    end;
  except FreeAndNil(Result); raise end;
end;

procedure DataChannelsCombine(const RawPixels: Pointer; const RawPixelsSize: Cardinal;
  const PixelSize: Integer; const ChannelData: TChannelsSplit);
var
  Channel, I: Integer;
  SourcePtr, DestPtr: PByte;
begin
  if ChannelData.DataSize <> RawPixelsSize div PixelSize then
    raise EInternalError.Create('Invalid input size to DataChannelsCombine');

  for Channel := 0 to PixelSize - 1 do
  begin
    SourcePtr := ChannelData.Data[Channel];
    DestPtr := RawPixels;
    Inc(DestPtr, Channel);

    for I := 0 to ChannelData.DataSize - 1 do
    begin
      DestPtr^ := SourcePtr^;
      Inc(SourcePtr);
      Inc(DestPtr, PixelSize);
    end;
  end;
end;

{ ------------------------------------------------------------------------
  Castle Game Engine RLE compression data format:

  We have 2 types of chunks:

  - 1st byte has most significant bit not set, IOW it is < 128.

    Let's call Length = 1st byte.

    Decompression should read next byte, and repeat it Length times.
    This is the core of the RLE compression.
    So, at best, 127 bytes can be compressed into 2 bytes
    (length = 127, and byte to be repeated 127 times).

  - 1st byte has most significant bit set, IOW it is >= 128.

    Let's call Length = 1st byte - 128 = 1st byte with most significant bit zeroed.

    Decompression should read next Length bytes, and copy them to output, once.

    This is done to lessed the impact of the least favorable data for
    RLE compression, when all bytes are different.
    So we don't need to encode each unique byte as "1 a", "1 b" etc.
    (effectively doubling the size of unique bytes)
    Instead, we can encode a sequence of unique bytes.

    On tests (TTestCastleClassUtils.TestRleCompression),
    this makes compression better, from 17% to 12%.

  -------------------------------------------------------------------------
}

const
  { max value in least significant 7 digits }
  MaxChunkLength = $7F;
  MaxLeftoverUniqueBytes = $7F;

procedure RleCompress(
  const Source: Pointer; const SourceSize: Cardinal; const Dest: TStream);
var
  LeftoverUniqueBytes: array [1..MaxLeftoverUniqueBytes] of Byte;
  LeftoverUniqueBytesCount: Byte;

  { Put LeftoverUniqueBytes* into Dest, if LeftoverUniqueBytesCount is zero.
    Set LeftoverUniqueBytesCount to zero. }
  procedure FlushLeftoverUniqueBytes;
  var
    LeftoverUniqueBytesCountEncoded: Byte;
  begin
    if LeftoverUniqueBytesCount <> 0 then
    begin
      {$ifdef CASTLE_DEBUG_RLE} WritelnLog(Format('Compress leftover unique bytes count %d', [LeftoverUniqueBytesCount])); {$endif}
      LeftoverUniqueBytesCountEncoded := LeftoverUniqueBytesCount or (1 shl 7);
      Dest.WriteBuffer(LeftoverUniqueBytesCountEncoded, SizeOf(LeftoverUniqueBytesCountEncoded));
      Dest.WriteBuffer(LeftoverUniqueBytes, LeftoverUniqueBytesCount);
      LeftoverUniqueBytesCount := 0;
    end;
  end;

var
  I: Cardinal;
  ChunkByte, ChunkSize: Byte;
  Buffer: PByte;
begin
  LeftoverUniqueBytesCount := 0;

  { Compress contents of Buffer^ into Dest stream. }
  Buffer := PByte(Source);

  { We look at character with index I (1-based).
    We move Buffer pointer, such that Buffer^ always points to ith byte. }
  I := 1;
  while I <= SourceSize do
  begin
    ChunkSize := 1;
    ChunkByte := Buffer^;
    Inc(I);
    Inc(Buffer);
    while
      (I <= SourceSize) and
      (Buffer^ = ChunkByte) and
      (ChunkSize < MaxChunkLength) do
    begin
      Inc(I);
      Inc(Buffer);
      Inc(ChunkSize);
    end;

    { We could ignore now the optimization to use LeftoverUniqueBytes,
      and just do this:
        Dest.WriteBuffer(ChunkSize, SizeOf(ChunkSize));
        Dest.WriteBuffer(ChunkByte, SizeOf(ChunkByte));
      This would result in correct, albeit unoptimal, compression.
    }

    if ChunkSize = 1 then
    begin
      if LeftoverUniqueBytesCount = MaxLeftoverUniqueBytes then
        FlushLeftoverUniqueBytes;
      Assert(LeftoverUniqueBytesCount < MaxLeftoverUniqueBytes);
      Inc(LeftoverUniqueBytesCount);
      LeftoverUniqueBytes[LeftoverUniqueBytesCount] := ChunkByte;
    end else
    begin
      FlushLeftoverUniqueBytes;
      {$ifdef CASTLE_DEBUG_RLE} WritelnLog(Format('Compress chunk size %d byte %d', [ChunkSize, ChunkByte])); {$endif}
      Dest.WriteBuffer(ChunkSize, SizeOf(ChunkSize));
      Dest.WriteBuffer(ChunkByte, SizeOf(ChunkByte));
    end;
  end;

  FlushLeftoverUniqueBytes;
end;

procedure RleDecompress(
  const Source: Pointer; const SourceSize: Cardinal;
  const Dest: Pointer; const DestSize: Cardinal);
var
  DestStream: TMemoryStream;
begin
  DestStream := TMemoryStream.Create;
  try
    DestStream.Size := DestSize; // preallocate necessary memory
    RleDecompress(Source, SourceSize, DestStream);
    if DestStream.Size <> DestSize then
      raise Exception.CreateFmt('Decompressed stream size %d is different than expected %d', [
        DestStream.Size,
        DestSize
      ]);
    Move(DestStream.Memory^, Dest^, DestSize);
  finally FreeAndNil(DestStream) end;
end;

procedure RleDecompress(
  const Source: Pointer; const SourceSize: Cardinal; const Dest: TStream);
var
  Buffer: PByte;
  I, J: Cardinal;
  ChunkSize, ChunkByte: Byte;
begin
  { Decompress contents of Buffer^ into Dest stream. }
  Buffer := PByte(Source);

  { We look at character with index I (1-based).
    We move Buffer pointer, such that Buffer^ always points to ith byte. }
  I := 1;
  while I <= SourceSize do
  begin
    ChunkSize := Buffer^;
    Inc(I);
    Inc(Buffer);

    if ChunkSize and (1 shl 7) <> 0 then
    begin
      ChunkSize := ChunkSize and not (1 shl 7);

      { We have ChunkSize of unique bytes to copy from Buffer^ to Dest. }
      {$ifdef CASTLE_DEBUG_RLE} WritelnLog(Format('Decompress leftover unique bytes count %d', [ChunkSize])); {$endif}

      if ChunkSize > MaxLeftoverUniqueBytes then
        raise Exception.CreateFmt('Too large leftover chunk size %d, should be <= %d', [
          ChunkSize,
          MaxLeftoverUniqueBytes
        ]);
      if I + ChunkSize - 1 > SourceSize then
        raise Exception.CreateFmt('Compressed stream ended in the middle of leftover chunk (has size %d, should have size at least %d)', [
          SourceSize,
          I + ChunkSize - 1
        ]);

      Dest.WriteBuffer(Buffer^, ChunkSize);
      Inc(I, ChunkSize);
      Inc(Buffer, ChunkSize);
    end else
    begin
      ChunkByte := Buffer^;
      Inc(I);
      Inc(Buffer);

      {$ifdef CASTLE_DEBUG_RLE} WritelnLog(Format('Decompress chunk size %d byte %d', [ChunkSize, ChunkByte])); {$endif}
      for J := 1 to ChunkSize do
        Dest.WriteBuffer(ChunkByte, SizeOf(ChunkByte));
    end;
  end;
end;

end.