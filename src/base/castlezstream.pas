{
    $Id: zstream.pp,v 1.6 2005/02/14 17:13:15 peter Exp $
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by the Free Pascal development team

    Implementation of compression streams.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{ Streams using zlib compression.

  This is adjusted from FPC sources unit fcl/inc/zstream.pp,
  to use my CastleZLib unit. }
unit CastleZStream;

{ We also have here some work with TDecompressionStream.WindowBits property
  started, but it apparently does not work yet. Some tests need to be made. }

interface

uses
  Sysutils, Classes, CastleZLib;

{$H+}

type
  // Error reporting.
  EZlibError = class(EStreamError);
  ECompressionError = class(EZlibError);
  EDecompressionError = class(EZlibError);

  TCustomZlibStream = class(TStream)
  private
    FStrm: TStream;
    FStrmPos: Integer;
    FOnProgress: TNotifyEvent;
    FZRec: TZStream;
    FBuffer: array [Word] of Byte;
  protected
    procedure Progress(Sender: TObject); dynamic;
    property OnProgress: TNotifyEvent read FOnProgress write FOnProgress;
  public
    constructor Create(Strm: TStream);
  end;

  TCompressionLevel = (clNone, clFastest, clDefault, clMax);

  TCompressionStream = class(TCustomZlibStream)
  private
    function GetCompressionRate: extended;
    function CompressionCheck(code: Integer): Integer;
    procedure CompressBuf(const InBuf: Pointer; InBytes: Integer;
                          var OutBuf: Pointer; var OutBytes: Integer);
  public
    constructor Create(CompressionLevel: TCompressionLevel; Dest: TStream);
    destructor Destroy; override;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;
    property CompressionRate: extended read GetCompressionRate;
    property OnProgress;
  end;

  TDecompressionStream = class(TCustomZlibStream)
  private
    FWindowBits: LongInt;
    function DecompressionCheck(code: Integer): Integer;
    procedure DecompressBuf(const InBuf: Pointer; InBytes: Integer;
    OutEstimate: Integer; var OutBuf: Pointer; var OutBytes: Integer);
  public
    constructor Create(Source: TStream; AWindowBits: LongInt = 15);

    destructor Destroy; override;

    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;
    property OnProgress;

    { WindowBits meaning is the same as for zlib inflateInit2 function. }
    property WindowBits: LongInt read FWindowBits;
  end;

  TGZOpenMode = (gzOpenRead,gzOpenWrite);

  TGZFileStream = Class(TStream)
    Private
    FOpenMode : TGZOpenmode;
    FFIle : gzfile;
    Public
    Constructor Create(FileName: String;FileMode: TGZOpenMode);
    Destructor Destroy;override;
    Function Read(Var Buffer; Count : longint): longint;override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;
    end;


implementation

Const
  ErrorStrings : array [0..6] of string =
    ('Unknown error %d','Z_ERRNO','Z_STREAM_ERROR',
     'Z_DATA_ERROR','Z_MEM_ERROR','Z_BUF_ERROR','Z_VERSION_ERROR');
  SCouldntOpenFile = 'Couldn''t open file : %s';
  SReadOnlyStream = 'Decompression streams are read-only';
  SWriteOnlyStream = 'Compression streams are write-only';
  SSeekError = 'Compression stream seek error';
  SInvalidSeek = 'Invalid Compression seek operation';

function zlibAllocMem(opaque:pointer; items:uInt; size:uInt):pointer;
{$ifndef usepaszlib} {$ifdef ZLIB_STDCALL} stdcall {$else} cdecl {$endif}; {$endif}
begin
  Result:=GetMem(Items*Size);
end;

procedure zlibFreeMem(opaque:pointer; address:pointer);
{$ifndef usepaszlib} {$ifdef ZLIB_STDCALL} stdcall {$else} cdecl {$endif}; {$endif}
begin
  FreeMem(address);
end;


procedure TCompressionStream.CompressBuf(const InBuf: Pointer; InBytes: Integer;
                      var OutBuf: Pointer; var OutBytes: Integer);
var
  strm: TZStream;
  P: Pointer;
begin
  FillChar(strm, sizeof(strm), 0);
  strm.zalloc := @zlibAllocMem;
  strm.zfree := @zlibFreeMem;
  OutBytes := ((InBytes + (InBytes div 10) + 12) + 255) and not 255;
  OutBuf:=GetMem(OutBytes);
  try
    strm.next_in := InBuf;
    strm.avail_in := InBytes;
    strm.next_out := OutBuf;
    strm.avail_out := OutBytes;
    CompressionCheck(deflateInit_(strm, Z_BEST_COMPRESSION, ZLIB_VERSION, sizeof(strm)));
    try
      while CompressionCheck(deflate(strm, Z_FINISH)) <> Z_STREAM_END do
      begin
        P := OutBuf;
        Inc(OutBytes, 256);
        ReallocMem(OutBuf,OutBytes);
        { Kambi: this crazy typecasting to PtrUInt (both partial and final
          results) apparently needed to get rid of warnings about convertions. }
        strm.next_out := PByte(PtrUInt(
          PtrUInt(OutBuf) + (PtrUInt(strm.next_out) - PtrUInt(P))));
        strm.avail_out := 256;
      end;
    finally
      CompressionCheck(deflateEnd(strm));
    end;
    ReallocMem(OutBuf,strm.total_out);
    OutBytes := strm.total_out;
  except
    FreeMem(OutBuf);
    raise;
  end;
end;


procedure TDecompressionStream.DecompressBuf(const InBuf: Pointer; InBytes: Integer;
       OutEstimate: Integer; var OutBuf: Pointer; var OutBytes: Integer);
var
  strm: TZStream;
  P: Pointer;
  BufInc: Integer;
Type
  PByte = ^Byte;
begin
  FillChar(strm, sizeof(strm), 0);
  strm.zalloc := @zlibAllocMem;
  strm.zfree := @zlibFreeMem;
  BufInc := (InBytes + 255) and not 255;
  if OutEstimate = 0 then
    OutBytes := BufInc
  else
    OutBytes := OutEstimate;
  OutBuf:=GetMem(OutBytes);
  try
    strm.next_in := InBuf;
    strm.avail_in := InBytes;
    strm.next_out := OutBuf;
    strm.avail_out := OutBytes;
    DecompressionCheck(inflateInit2(strm, WindowBits));
    try
      while DecompressionCheck(inflate(strm, Z_FINISH)) <> Z_STREAM_END do
      begin
        P := OutBuf;
        Inc(OutBytes, BufInc);
        ReallocMem(OutBuf, OutBytes);
        { Kambi: this crazy typecasting to PtrUInt (both partial and final
          results) apparently needed to get rid of warnings about convertions. }
        strm.next_out := PByte(PtrUInt(
          PtrUInt(OutBuf) + (PtrUInt(strm.next_out) - PtrUInt(P))));
        strm.avail_out := BufInc;
      end;
    finally
      DecompressionCheck(inflateEnd(strm));
    end;
    ReallocMem(OutBuf, strm.total_out);
    OutBytes := strm.total_out;
  except
    FreeMem(OutBuf);
    raise;
  end;
end;


// TCustomZlibStream

constructor TCustomZLibStream.Create(Strm: TStream);
begin
  inherited Create;
  FStrm := Strm;
  FStrmPos := Strm.Position;
  FZRec.zalloc := @zlibAllocMem;
  FZRec.zfree := @zlibFreeMem;
end;

procedure TCustomZLibStream.Progress(Sender: TObject);
begin
  if Assigned(FOnProgress) then FOnProgress(Sender);
end;


// TCompressionStream

constructor TCompressionStream.Create(CompressionLevel: TCompressionLevel;
  Dest: TStream);
const
  Levels: array [TCompressionLevel] of ShortInt =
    (Z_NO_COMPRESSION, Z_BEST_SPEED, Z_DEFAULT_COMPRESSION, Z_BEST_COMPRESSION);
begin
  inherited Create(Dest);
  FZRec.next_out := @FBuffer;
  FZRec.avail_out := sizeof(FBuffer);
  CompressionCheck(deflateInit_(FZRec, Levels[CompressionLevel], ZLIB_VERSION, sizeof(FZRec)));
end;

destructor TCompressionStream.Destroy;
begin
  FZRec.next_in := nil;
  FZRec.avail_in := 0;
  try
    if FStrm.Position <> FStrmPos then FStrm.Position := FStrmPos;
    while (CompressionCheck(deflate(FZRec, Z_FINISH)) <> Z_STREAM_END)
      and (FZRec.avail_out = 0) do
    begin
      FStrm.WriteBuffer(FBuffer, sizeof(FBuffer));
      FZRec.next_out := @FBuffer;
      FZRec.avail_out := sizeof(FBuffer);
    end;
    if FZRec.avail_out < sizeof(FBuffer) then
      FStrm.WriteBuffer(FBuffer, sizeof(FBuffer) - FZRec.avail_out);
  finally
    deflateEnd(FZRec);
  end;
  inherited Destroy;
end;

function TCompressionStream.CompressionCheck(code: Integer): Integer;
begin
  Result := code;
  if (code < 0) then
    if code < -6 then
      raise ECompressionError.CreateFmt(Errorstrings[0],[Code])
    else
      raise ECompressionError.Create(ErrorStrings[Abs(Code)]);
end;


function TCompressionStream.Read(var Buffer; Count: Longint): Longint;
begin
  raise ECompressionError.Create('Invalid stream operation');
  result:=0;
end;

function TCompressionStream.Write(const Buffer; Count: Longint): Longint;
begin
  FZRec.next_in := @Buffer;
  FZRec.avail_in := Count;
  if FStrm.Position <> FStrmPos then FStrm.Position := FStrmPos;
  while (FZRec.avail_in > 0) do
  begin
    CompressionCheck(deflate(FZRec, 0));
    if FZRec.avail_out = 0 then
    begin
      FStrm.WriteBuffer(FBuffer, sizeof(FBuffer));
      FZRec.next_out := @FBuffer;
      FZRec.avail_out := sizeof(FBuffer);
      FStrmPos := FStrm.Position;
      Progress(Self);
    end;
  end;
  Result := Count;
end;

function TCompressionStream.Seek(Offset: Longint; Origin: Word): Longint;
begin
  if (Offset = 0) and (Origin = soFromCurrent) then
    Result := FZRec.total_in
  else
    raise ECompressionError.Create(SInvalidSeek);
end;

function TCompressionStream.GetCompressionRate: extended;
begin
  Result:=0.0;
{  With FZrec do
    if total_in = 0 then
      GetCompressionRate:=0.0
    else
      GetCompressionRate:=1.0E2*(1.0E0-(total_out/total_in));
}
end;


// TDecompressionStream

constructor TDecompressionStream.Create(Source: TStream; AWindowBits: LongInt);
begin
  inherited Create(Source);
  FZRec.next_in := @FBuffer;
  FWindowBits := AWindowBits;
  DecompressionCheck(inflateInit2(FZRec, WindowBits));
end;

destructor TDecompressionStream.Destroy;
begin
  inflateEnd(FZRec);
  inherited Destroy;
end;

function TDecompressionStream.DecompressionCheck(code: Integer): Integer;
begin
  Result := code;
  If Code<0 then
    if code < -6 then
      raise EDecompressionError.CreateFmt(Errorstrings[0],[Code])
    else
      raise EDecompressionError.Create(ErrorStrings[Abs(Code)]);
end;

function TDecompressionStream.Read(var Buffer; Count: Longint): Longint;
begin
  FZRec.next_out := @Buffer;
  FZRec.avail_out := Count;
  if FStrm.Position <> FStrmPos then FStrm.Position := FStrmPos;
  while (FZRec.avail_out > 0) do
  begin
    if FZRec.avail_in = 0 then
    begin
      FZRec.avail_in := FStrm.Read(FBuffer, sizeof(FBuffer));
      if FZRec.avail_in = 0 then
        begin
          Result := Count - FZRec.avail_out;
          Exit;
        end;
      FZRec.next_in := @FBuffer;
      FStrmPos := FStrm.Position;
      Progress(Self);
    end;
    DeCompressionCheck(inflate(FZRec, 0));
  end;
  Result := Count;
end;

function TDecompressionStream.Write(const Buffer; Count: Longint): Longint;
begin
  raise EDecompressionError.Create('Invalid stream operation');
  result:=0;
end;

function TDecompressionStream.Seek(Offset: Longint; Origin: Word): Longint;
var
  I: Integer;
  Buf: array [0..4095] of Char;
begin
  if (Offset = 0) and (Origin = soFromBeginning) then
  begin
    DecompressionCheck(inflateReset(FZRec));
    FZRec.next_in := @FBuffer;
    FZRec.avail_in := 0;
    FStrm.Position := 0;
    FStrmPos := 0;
  end
  else if ( (Offset >= 0) and (Origin = soFromCurrent)) or
          ( ((Offset - FZRec.total_out) > 0) and (Origin = soFromBeginning)) then
  begin
    if Origin = soFromBeginning then Dec(Offset, FZRec.total_out);
    if Offset > 0 then
    begin
      for I := 1 to Offset div sizeof(Buf) do
        ReadBuffer(Buf, sizeof(Buf));
      ReadBuffer(Buf, Offset mod sizeof(Buf));
    end;
  end
  else
    raise EDecompressionError.Create(SInvalidSeek);
  Result := FZRec.total_out;
end;

// TGZFileStream

Constructor TGZFileStream.Create(FileName: String;FileMode: TGZOpenMode);

Const OpenStrings : array[TGZOpenMode] of pchar = ('rb','wb');

begin
   FOpenMode:=FileMode;
   FFile:=gzopen (PChar(FileName),Openstrings[FileMode]);
   If FFile=Nil then
     Raise ezlibError.CreateFmt (SCouldntOpenFIle,[FileName]);
end;

Destructor TGZFileStream.Destroy;
begin
  gzclose(FFile);
  Inherited Destroy;
end;

Function TGZFileStream.Read(Var Buffer; Count : longint): longint;
begin
  If FOpenMode=gzOpenWrite then
    Raise ezliberror.create(SWriteOnlyStream);
  Result:=gzRead(FFile,@Buffer,Count);
end;

function TGZFileStream.Write(const Buffer; Count: Longint): Longint;
begin
  If FOpenMode=gzOpenRead then
    Raise EzlibError.Create(SReadonlyStream);
  Result:=gzWrite(FFile,@Buffer,Count);
end;

function TGZFileStream.Seek(Offset: Longint; Origin: Word): Longint;
begin
  Result:=gzseek(FFile,Offset,Origin);
  If Result=-1 then
    Raise eZlibError.Create(SSeekError);
end;

end.
{
  $Log: zstream.pp,v $
  Revision 1.6  2005/02/14 17:13:15  peter
    * truncate log

}
