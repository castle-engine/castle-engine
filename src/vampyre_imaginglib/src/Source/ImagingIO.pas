{
  Vampyre Imaging Library
  by Marek Mauder
  https://github.com/galfar/imaginglib
  https://imaginglib.sourceforge.io
  - - - - -
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at https://mozilla.org/MPL/2.0.
} 

{ This unit contains default IO functions for reading from/writing to
  files, streams and memory.}
unit ImagingIO;

{$I ImagingOptions.inc}

interface

uses
  SysUtils, Classes, ImagingTypes, Imaging, ImagingUtility;

type
  TMemoryIORec = record
    Data: ImagingUtility.PByteArray;
    Position: LongInt;
    Size: LongInt;
  end;
  PMemoryIORec = ^TMemoryIORec;

var
  OriginalFileIO: TIOFunctions;
  FileIO: TIOFunctions;
  StreamIO: TIOFunctions;
  MemoryIO: TIOFunctions;

{ Helper function that returns size of input (from current position to the end)
  represented by Handle (and opened and operated on by members of IOFunctions).}
function GetInputSize(const IOFunctions: TIOFunctions; Handle: TImagingHandle): Int64;
{ Helper function that initializes TMemoryIORec with given params.}
function PrepareMemIO(Data: Pointer; Size: LongInt): TMemoryIORec;
{ Reads one text line from input (CR+LF, CR, or LF as line delimiter).}
function ReadLine(const IOFunctions: TIOFunctions; Handle: TImagingHandle;
  out Line: AnsiString; FailOnControlChars: Boolean = False): Boolean;
{ Writes one text line to input with optional line delimiter.}
procedure WriteLine(const IOFunctions: TIOFunctions; Handle: TImagingHandle;
  const Line: AnsiString; const LineEnding: AnsiString = sLineBreak);

type
  TReadMemoryStream = class(TCustomMemoryStream)
  public
    constructor Create(Data: Pointer; Size: Integer);
    class function CreateFromIOHandle(const IOFunctions: TIOFunctions; Handle: TImagingHandle): TReadMemoryStream;
  end;

  TImagingIOStream = class(TStream)
  private
    FIO: TIOFunctions;
    FHandle: TImagingHandle;
  public
    constructor Create(const IOFunctions: TIOFunctions; Handle: TImagingHandle);
  end;

implementation

const
  DefaultBufferSize = 16 * 1024;

type
  { Based on TaaBufferedStream
    Copyright (c) Julian M Bucknall 1997, 1999 }
  TBufferedStream = class
  private
    FBuffer: PByteArray;
    FBufSize: Integer;
    FBufStart: Integer;
    FBufPos: Integer;
    FBytesInBuf: Integer;
    FSize: Integer;
    FDirty: Boolean;
    FStream: TStream;
    function GetPosition: Integer;
    function GetSize: Integer;
    procedure ReadBuffer;
    procedure WriteBuffer;
    procedure SetPosition(const Value: Integer);
  public
    constructor Create(AStream: TStream);
    destructor Destroy; override;
    function Read(var Buffer; Count: Integer): Integer;
    function Write(const Buffer; Count: Integer): Integer;
    function Seek(Offset: Integer; Origin: Word): Integer;
    procedure Commit;
    property Stream: TStream read FStream;
    property Position: Integer read GetPosition write SetPosition;
    property Size: Integer read GetSize;
  end;

constructor TBufferedStream.Create(AStream: TStream);
begin
  inherited Create;
  FStream := AStream;
  FBufSize := DefaultBufferSize;
  GetMem(FBuffer, FBufSize);
  FBufPos := 0;
  FBytesInBuf := 0;
  FBufStart := 0;
  FDirty := False;
  FSize := AStream.Size;
end;

destructor TBufferedStream.Destroy;
begin
  if FBuffer <> nil then
  begin
    Commit;
    FreeMem(FBuffer);
  end;
  FStream.Position := Position; // Make sure source stream has right position
  inherited Destroy;
end;

function TBufferedStream.GetPosition: Integer;
begin
  Result := FBufStart + FBufPos;
end;

procedure TBufferedStream.SetPosition(const Value: Integer);
begin
  Seek(Value, soFromCurrent);
end;

function TBufferedStream.GetSize: Integer;
begin
  Result := FSize;
end;

procedure TBufferedStream.ReadBuffer;
var
  SeekResult: Integer;
begin
  SeekResult := FStream.Seek(FBufStart, soBeginning);
  if SeekResult = -1 then
    raise Exception.Create('TBufferedStream.ReadBuffer: seek failed');
  FBytesInBuf := FStream.Read(FBuffer^, FBufSize);
  if FBytesInBuf <= 0 then
    raise Exception.Create('TBufferedStream.ReadBuffer: read failed');
end;

procedure TBufferedStream.WriteBuffer;
var
  SeekResult: Integer;
  BytesWritten: Integer;
begin
  SeekResult := FStream.Seek(FBufStart, soBeginning);
  if SeekResult = -1 then
    raise Exception.Create('TBufferedStream.WriteBuffer: seek failed');
  BytesWritten := FStream.Write(FBuffer^, FBytesInBuf);
  if BytesWritten <> FBytesInBuf then
    raise Exception.Create('TBufferedStream.WriteBuffer: write failed');
end;

procedure TBufferedStream.Commit;
begin
  if FDirty then
  begin
    WriteBuffer;
    FDirty := False;
  end;
end;

function TBufferedStream.Read(var Buffer; Count: Integer): Integer;
var
  BufAsBytes: TByteArray absolute Buffer;
  BufIdx, BytesToGo, BytesToRead: Integer;
begin
  // Calculate the actual number of bytes we can read - this depends on
  // the current position and size of the stream as well as the number
  // of bytes requested.
  BytesToGo := Count;
  if FSize < (FBufStart + FBufPos + Count) then
    BytesToGo := FSize - (FBufStart + FBufPos);

  if BytesToGo <= 0 then
  begin
    Result := 0;
    Exit;
  end;
  // Remember to return the result of our calculation
  Result := BytesToGo;

  BufIdx := 0;
  if FBytesInBuf = 0 then
    ReadBuffer;
  // Calculate the number of bytes we can read prior to the loop
  BytesToRead := FBytesInBuf - FBufPos;
  if BytesToRead > BytesToGo then
    BytesToRead := BytesToGo;
  // Copy from the stream buffer to the caller's buffer
  Move(FBuffer^[FBufPos], BufAsBytes[BufIdx], BytesToRead);
  // Calculate the number of bytes still to read}
  Dec(BytesToGo, BytesToRead);

  // while we have bytes to read, read them
  while BytesToGo > 0 do
  begin
    Inc(BufIdx, BytesToRead);
    // As we've exhausted this buffer-full, advance to the next, check
    //  to see whether we need to write the buffer out first
    if FDirty then
    begin
      WriteBuffer;
      FDirty := false;
    end;
    Inc(FBufStart, FBufSize);
    FBufPos := 0;
    ReadBuffer;
    // Calculate the number of bytes we can read in this cycle
    BytesToRead := FBytesInBuf;
    if BytesToRead > BytesToGo then
      BytesToRead := BytesToGo;
    // Copy from the stream buffer to the caller's buffer
    Move(FBuffer^, BufAsBytes[BufIdx], BytesToRead);
    // Calculate the number of bytes still to read
    Dec(BytesToGo, BytesToRead);
  end;
  // Remember our new position
  Inc(FBufPos, BytesToRead);
  if FBufPos = FBufSize then
  begin
    Inc(FBufStart, FBufSize);
    FBufPos := 0;
    FBytesInBuf := 0;
  end;
end;

function TBufferedStream.Seek(Offset: Integer; Origin: Word): Integer;
var
  NewBufStart, NewPos: Integer;
begin
  // Calculate the new position
  case Origin of
    soFromBeginning : NewPos := Offset;
    soFromCurrent   : NewPos := FBufStart + FBufPos + Offset;
    soFromEnd       : NewPos := FSize + Offset;
  else
    raise Exception.Create('TBufferedStream.Seek: invalid origin');
  end;

  if (NewPos < 0) or (NewPos > FSize) then
  begin
    //NewPos := ClampInt(NewPos, 0, FSize); don't do this - for writing
  end;
  // Calculate which page of the file we need to be at
  NewBufStart := NewPos and not Pred(FBufSize);
  // If the new page is different than the old, mark the buffer as being
  // ready to be replenished, and if need be write out any dirty data
  if NewBufStart <> FBufStart then
  begin
    if FDirty then
    begin
      WriteBuffer;
      FDirty := False;
    end;
    FBufStart := NewBufStart;
    FBytesInBuf := 0;
  end;
  // Save the new position
  FBufPos := NewPos - NewBufStart;
  Result := NewPos;
end;

function TBufferedStream.Write(const Buffer; Count: Integer): Integer;
var
  BufAsBytes: TByteArray absolute Buffer;
  BufIdx, BytesToGo, BytesToWrite: Integer;
begin
  // When we write to this stream we always assume that we can write the
  // requested number of bytes: if we can't (eg, the disk is full) we'll
  // get an exception somewhere eventually.
  BytesToGo := Count;
  // Remember to return the result of our calculation
  Result := BytesToGo;

  BufIdx := 0;
  if (FBytesInBuf = 0) and (FSize > FBufStart) then
    ReadBuffer;
  // Calculate the number of bytes we can write prior to the loop
  BytesToWrite := FBufSize - FBufPos;
  if BytesToWrite > BytesToGo then
    BytesToWrite := BytesToGo;
  // Copy from the caller's buffer to the stream buffer
  Move(BufAsBytes[BufIdx], FBuffer^[FBufPos], BytesToWrite);
  // Mark our stream buffer as requiring a save to the actual stream,
  // note that this will suffice for the rest of the routine as well: no
  // inner routine will turn off the dirty flag.
  FDirty := True;
  // Calculate the number of bytes still to write
  Dec(BytesToGo, BytesToWrite);

  // While we have bytes to write, write them
  while BytesToGo > 0 do
  begin
    Inc(BufIdx, BytesToWrite);
    // As we've filled this buffer, write it out to the actual stream
    // and advance to the next buffer, reading it if required
    FBytesInBuf := FBufSize;
    WriteBuffer;
    Inc(FBufStart, FBufSize);
    FBufPos := 0;
    FBytesInBuf := 0;
    if FSize > FBufStart then
      ReadBuffer;
    // Calculate the number of bytes we can write in this cycle
    BytesToWrite := FBufSize;
    if BytesToWrite > BytesToGo then
      BytesToWrite := BytesToGo;
    // Copy from the caller's buffer to our buffer
    Move(BufAsBytes[BufIdx], FBuffer^, BytesToWrite);
    // Calculate the number of bytes still to write
    Dec(BytesToGo, BytesToWrite);
  end;
  // Remember our new position
  Inc(FBufPos, BytesToWrite);
  // Make sure the count of valid bytes is correct
  if FBytesInBuf < FBufPos then
    FBytesInBuf := FBufPos;
  // Make sure the stream size is correct
  if FSize < (FBufStart + FBytesInBuf) then
    FSize := FBufStart + FBytesInBuf;
  // If we're at the end of the buffer, write it out and advance to the
  // start of the next page
  if FBufPos = FBufSize then
  begin
    WriteBuffer;
    FDirty := False;
    Inc(FBufStart, FBufSize);
    FBufPos := 0;
    FBytesInBuf := 0;
  end;
end;

{ File IO functions }

function FileOpen(FileName: PChar; Mode: TOpenMode): TImagingHandle; cdecl;
var
  Stream: TStream;
begin
  Stream := nil;

  case Mode of
    omReadOnly:  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
    omCreate:    Stream := TFileStream.Create(FileName, fmCreate);
    omReadWrite:
      begin
        if FileExists(FileName) then
          Stream := TFileStream.Create(FileName, fmOpenReadWrite or fmShareExclusive)
        else
          Stream := TFileStream.Create(FileName, fmCreate);
      end;
  end;

  Assert(Stream <> nil);
  Result := TBufferedStream.Create(Stream);
end;

procedure FileClose(Handle: TImagingHandle); cdecl;
var
  Stream: TStream;
begin
  Stream := TBufferedStream(Handle).Stream;
  TBufferedStream(Handle).Free;
  Stream.Free;
end;

function FileEof(Handle: TImagingHandle): Boolean; cdecl;
begin
  Result := TBufferedStream(Handle).Position = TBufferedStream(Handle).Size;
end;

function FileSeek(Handle: TImagingHandle; Offset: Int64; Mode: TSeekMode): Int64; cdecl;
begin
  Result := TBufferedStream(Handle).Seek(Offset, LongInt(Mode));
end;

function FileTell(Handle: TImagingHandle): Int64; cdecl;
begin
  Result := TBufferedStream(Handle).Position;
end;

function FileRead(Handle: TImagingHandle; Buffer: Pointer; Count: LongInt): LongInt; cdecl;
begin
  Result := TBufferedStream(Handle).Read(Buffer^, Count);
end;

function FileWrite(Handle: TImagingHandle; Buffer: Pointer; Count: LongInt): LongInt; cdecl;
begin
  Result := TBufferedStream(Handle).Write(Buffer^, Count);
end;

{ Stream IO functions }

function StreamOpen(FileName: PChar; Mode: TOpenMode): TImagingHandle; cdecl;
begin
  Result := FileName;
end;

procedure StreamClose(Handle: TImagingHandle); cdecl;
begin
end;

function StreamEof(Handle: TImagingHandle): Boolean; cdecl;
begin
  Result := TStream(Handle).Position = TStream(Handle).Size;
end;

function StreamSeek(Handle: TImagingHandle; Offset: Int64; Mode: TSeekMode): Int64; cdecl;
begin
  Result := TStream(Handle).Seek(Offset, Word(Mode));
end;

function StreamTell(Handle: TImagingHandle): Int64; cdecl;
begin
  Result := TStream(Handle).Position;
end;

function StreamRead(Handle: TImagingHandle; Buffer: Pointer; Count: LongInt):
  LongInt; cdecl;
begin
  Result := TStream(Handle).Read(Buffer^, Count);
end;

function StreamWrite(Handle: TImagingHandle; Buffer: Pointer; Count: LongInt): LongInt; cdecl;
begin
  Result := TStream(Handle).Write(Buffer^, Count);
end;

{ Memory IO functions }

function MemoryOpen(FileName: PChar; Mode: TOpenMode): TImagingHandle; cdecl;
begin
  Result := FileName;
end;

procedure MemoryClose(Handle: TImagingHandle); cdecl;
begin
end;

function MemoryEof(Handle: TImagingHandle): Boolean; cdecl;
begin
  Result := PMemoryIORec(Handle).Position = PMemoryIORec(Handle).Size;
end;

function MemorySeek(Handle: TImagingHandle; Offset: Int64; Mode: TSeekMode): Int64; cdecl;
begin
  Result := PMemoryIORec(Handle).Position;
  case Mode of
    smFromBeginning: Result := Offset;
    smFromCurrent:   Result := PMemoryIORec(Handle).Position + Offset;
    smFromEnd:       Result := PMemoryIORec(Handle).Size + Offset;
  end;
  //Result := ClampInt(Result, 0, PMemoryIORec(Handle).Size); don't do this - some file formats use it
  PMemoryIORec(Handle).Position := Result;
end;

function MemoryTell(Handle: TImagingHandle): Int64; cdecl;
begin
  Result := PMemoryIORec(Handle).Position;
end;

function MemoryRead(Handle: TImagingHandle; Buffer: Pointer; Count: LongInt):
  LongInt; cdecl;
var
  Rec: PMemoryIORec;
begin
  Rec := PMemoryIORec(Handle);
  Result := Count;
  if Rec.Position + Count > Rec.Size then
    Result := Rec.Size - Rec.Position;
  Move(Rec.Data[Rec.Position], Buffer^, Result);
  Rec.Position := Rec.Position + Result;
end;

function MemoryWrite(Handle: TImagingHandle; Buffer: Pointer; Count: LongInt): LongInt; cdecl;
var
  Rec: PMemoryIORec;
begin
  Rec := PMemoryIORec(Handle);
  Result := Count;
  if Rec.Position + Count > Rec.Size then
    Result := Rec.Size - Rec.Position;
  Move(Buffer^, Rec.Data[Rec.Position], Result);
  Rec.Position := Rec.Position + Result;
end;

{ Helper IO functions }

function GetInputSize(const IOFunctions: TIOFunctions; Handle: TImagingHandle): Int64;
var
  OldPos: Int64;
begin
  OldPos := IOFunctions.Tell(Handle);
  IOFunctions.Seek(Handle, 0, smFromEnd);
  Result := IOFunctions.Tell(Handle);
  IOFunctions.Seek(Handle, OldPos, smFromBeginning);
end;

function PrepareMemIO(Data: Pointer; Size: LongInt): TMemoryIORec;
begin
  Result.Data := Data;
  Result.Position := 0;
  Result.Size := Size;
end;

function ReadLine(const IOFunctions: TIOFunctions; Handle: TImagingHandle;
  out Line: AnsiString; FailOnControlChars: Boolean): Boolean;
const
  MaxLine = 1024;
var
  EolPos, Pos: Integer;
  C: AnsiChar;
  EolReached: Boolean;
  Endings: set of AnsiChar;
begin
  Line := '';
  Pos := 0;
  EolPos := 0;
  EolReached := False;
  Endings := [#10, #13];
  Result := True;

  while not IOFunctions.Eof(Handle) do
  begin
    IOFunctions.Read(Handle, @C, SizeOf(C));

    if FailOnControlChars and (Byte(C) < $20) then
    begin
      Break;
    end;

    if not (C in Endings) then
    begin
      if EolReached then
      begin
        IOFunctions.Seek(Handle, EolPos, smFromBeginning);
        Exit;
      end
      else
      begin
        SetLength(Line, Length(Line) + 1);
        Line[Length(Line)] := C;
      end;
    end
    else if not EolReached then
    begin
      EolReached := True;
      EolPos := IOFunctions.Tell(Handle);
    end;

    Inc(Pos);
    if Pos >= MaxLine then
    begin
      Break;
    end;
  end;

  Result := False;
  IOFunctions.Seek(Handle, -Pos, smFromCurrent);
end;

procedure WriteLine(const IOFunctions: TIOFunctions; Handle: TImagingHandle;
  const Line: AnsiString; const LineEnding: AnsiString);
var
  ToWrite: AnsiString;
begin
  ToWrite := Line + LineEnding;
  IOFunctions.Write(Handle, @ToWrite[1], Length(ToWrite));
end;

{ TReadMemoryStream }

constructor TReadMemoryStream.Create(Data: Pointer; Size: Integer);
begin
  SetPointer(Data, Size);
end;

class function TReadMemoryStream.CreateFromIOHandle(const IOFunctions: TIOFunctions; Handle: TImagingHandle): TReadMemoryStream;
var
  Data: Pointer;
  Size: Integer;
begin
  Size := GetInputSize(IOFunctions, Handle);
  GetMem(Data, Size);
  IOFunctions.Read(Handle, Data, Size);
  Result := TReadMemoryStream.Create(Data, Size);
end;

{ TImagingIOStream }

constructor TImagingIOStream.Create(const IOFunctions: TIOFunctions;
  Handle: TImagingHandle);
begin

end;

initialization
  OriginalFileIO.Open := FileOpen;
  OriginalFileIO.Close := FileClose;
  OriginalFileIO.Eof := FileEof;
  OriginalFileIO.Seek := FileSeek;
  OriginalFileIO.Tell := FileTell;
  OriginalFileIO.Read := FileRead;
  OriginalFileIO.Write := FileWrite;

  StreamIO.Open := StreamOpen;
  StreamIO.Close := StreamClose;
  StreamIO.Eof := StreamEof;
  StreamIO.Seek := StreamSeek;
  StreamIO.Tell := StreamTell;
  StreamIO.Read := StreamRead;
  StreamIO.Write := StreamWrite;

  MemoryIO.Open := MemoryOpen;
  MemoryIO.Close := MemoryClose;
  MemoryIO.Eof := MemoryEof;
  MemoryIO.Seek := MemorySeek;
  MemoryIO.Tell := MemoryTell;
  MemoryIO.Read := MemoryRead;
  MemoryIO.Write := MemoryWrite;

  ResetFileIO;

{
  File Notes:

  -- TODOS ----------------------------------------------------
    - nothing now

  -- 0.77.3 ---------------------------------------------------
   - IO functions now have 64bit sizes and offsets.
   - Added helper classes TReadMemoryStream and TImagingIOStream.

  -- 0.77.1 ---------------------------------------------------
   - Updated IO Open functions according to changes in ImagingTypes.
   - Added ReadLine and WriteLine functions.

  -- 0.23 Changes/Bug Fixes -----------------------------------
    - Added merge between buffered read-only and write-only file
      stream adapters - TIFF saving needed both reading and writing.
    - Fixed bug causing wrong value of TBufferedWriteFile.Size
      (needed to add buffer pos to size).

  -- 0.21 Changes/Bug Fixes -----------------------------------
    - Removed TMemoryIORec.Written, use Position to get proper memory
      position (Written didn't take Seeks into account).
    - Added TBufferedReadFile and TBufferedWriteFile classes for
      buffered file reading/writing. File IO functions now use these
      classes resulting in performance increase mainly in file formats
      that read/write many small chunks. 
    - Added fmShareDenyWrite to FileOpenRead. You can now read
      files opened for reading by Imaging from other apps.
    - Added GetInputSize and PrepareMemIO helper functions.

  -- 0.19 Changes/Bug Fixes -----------------------------------
    - changed behaviour of MemorySeek to act as TStream
      based Seeks
}
end.

