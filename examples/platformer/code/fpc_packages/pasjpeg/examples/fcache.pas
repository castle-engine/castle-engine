Unit FCache;

interface


{ ---------------------- File Cache -------------------------- }

{  implements a simple file cache and mimic C getc and ungetc
   functions. }

const
  BufMemSize = 4096;
  EOF = ^Z;

type
  Cache = record
    active : boolean;
    BildOffset : LongInt;
    Buffer : array[0..BufMemSize-1] of byte;
    FVarPtr : ^file;
    FileOfs : LongInt;
    BufPos : integer;
    BufSize : integer;
  end;

Procedure fc_Init(var fc : Cache;
                  var f : file; FPos : LongInt);

Procedure fc_Close(var fc : Cache);

Procedure fc_Done(var fc : Cache;
                  var f : file);

Procedure fc_ReadBlock(var fc : Cache);

Function fc_getc(var fc : Cache) : Byte;
{ Read a byte at the current buffer read-index, increment the buffer
  read-index }

function fc_ungetc (var fc  : Cache; ch : char) : Byte;
{ Read a byte at the current buffer read-index, increment the buffer
  read-index }

procedure fc_WriteTo(var fc : Cache;
                     var Buf; Count : Word);

implementation

{$IFDEF USE_DOS}
uses
  Dos;
{$ENDIF}


Procedure fc_Init(var fc : Cache;
                  var f : file; FPos : LongInt);
begin
  with fc do
  begin
    active := false;
    FVarPtr := @f;
    FileOfs := FPos;
    BufSize := 0;
    BufPos := 0;
    {$IFDEF USE_DOS}
    if TFileRec(f).Mode <> fmClosed  then
    {$ENDIF}
    begin
      {$PUSH} {$I-}
      Seek(f, FPos);
      BlockRead(f, Buffer, BufMemSize, BufSize);
      {$POP}
      if (IOResult = 0) and (BufSize <> 0) then
        active := true;
    end;
  end;
end;

Procedure fc_Done(var fc : Cache;
                  var f : file);
begin
  with fc do
  if FVarPtr = @f then
  begin
    active := false;
    FVarPtr := NIL;
    FileOfs := 0;
    BufSize := 0;
    BufPos := 0;
  end;
end;

Procedure fc_Close(var fc : Cache);
begin
  with fc do
  begin
    if Assigned(FVarPtr) then
      Close(FVarPtr^);
    fc_Done(fc, FVarPtr^);
  end;
end;

Procedure fc_ReadBlock(var fc : Cache);
Begin
  with fc do
  if active then
  begin
    {$push}{$I-}
    Seek(FVarPtr^, FileOfs);
    BlockRead(FVarPtr^, Buffer, BufMemSize, BufSize);
    {$pop}
    BufPos := 0;
    active := (IOResult = 0) and (BufSize <> 0);
  end;
End;

Function fc_getc(var fc : Cache) : Byte;
{ Read a byte at the current buffer read-index, increment the buffer
  read-index }
begin
  with fc do
  if active then
  begin
    fc_GetC := Buffer[BufPos];
    Inc(BufPos);
    if BufPos = BufSize then
    begin
      Inc(FileOfs, BufSize);
      fc_ReadBlock(fc);
    end;
  end
  else
    fc_getc := Byte(EOF);
end;

function fc_ungetc (var fc  : Cache; ch : char) : Byte;
{ Read a byte at the current buffer read-index, increment the buffer
  read-index }
begin
  with fc do
  begin
    fc_UnGetC := Byte(EOF);
    if active and (FileOfs > 0) then
    begin
      if BufPos = 0 then
      begin
        Dec(FileOfs);
        fc_ReadBlock(fc);
      end;

      if BufPos > 0 then
      begin
        Dec(BufPos);
        fc_UnGetC := Buffer[BufPos];
      end;
    end;
  end;
end;

procedure fc_WriteTo(var fc : Cache;
                     var Buf; Count : Word);
type
  PByte = ^Byte;
var
  ChunkSize : Word;
  DestPtr : PByte;
Begin
  with fc do
  if active then
  begin
    ChunkSize := BufSize - BufPos;
    DestPtr := PByte(@Buf);
    if Count > ChunkSize then
    begin
      { the amount we need to read straddles a buffer boundary,
        we need two or more chunks. This implementation doesn't try
        to read more than two chunks. }

      Move(Buffer[BufPos], Buf, ChunkSize);
      Inc(DestPtr, ChunkSize);
      Dec(count, ChunkSize);
      Inc(FileOfs, BufSize);
      fc_ReadBlock(fc);
    end;
    { we are now completely within the buffer boundary,
      do a simple mem move }
    Move(Buffer[BufPos], DestPtr^, count);
  end;
End;

{ ---------------------- End File Cache -------------------------- }
end.
