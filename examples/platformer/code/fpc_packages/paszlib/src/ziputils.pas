unit ZipUtils;

{ ziputils.pas - IO on .zip files using zlib
  - definitions, declarations and routines used by both
    zip.pas and unzip.pas
    The file IO is implemented here.

  based on work by Gilles Vollant

  March 23th, 2000,
  Copyright (C) 2000 Jacques Nomssi Nzali }

interface

{$undef UseStream}

{$ifdef WIN32}
  {$define Delphi}
  {$ifdef UseStream}
    {$define Streams}
  {$endif}
{$endif}

//uses  Classes, SysUtils;

{ -------------------------------------------------------------- }
{$ifdef Streams}
type
  FILEptr = TFileStream;
{$else}
type
  FILEptr = ^file;
{$endif}

type
  seek_mode = (SEEK_SET, SEEK_CUR, SEEK_END);
  open_mode = (fopenread, fopenwrite, fappendwrite);

function fopen(filename: PChar; mode: open_mode): FILEptr;

procedure fclose(fp: FILEptr);

function fseek(fp: FILEptr; recPos: longint; mode: seek_mode): longint;

function fread(buf: pointer; recSize: longint; recCount: longint; fp: FILEptr): longint;

function fwrite(buf: pointer; recSize: longint; recCount: longint; fp: FILEptr): longint;

function ftell(fp: FILEptr): longint;  { ZIP }

function feof(fp: FILEptr): longint;   { MiniZIP }

{ ------------------------------------------------------------------- }

type
  zipFile = pointer;
  unzFile = pointer;

type
  z_off_t = longint;

{ tm_zip contain date/time info }
type
  tm_zip = record
    tm_sec:  longint;            { seconds after the minute - [0,59] }
    tm_min:  longint;            { minutes after the hour - [0,59] }
    tm_hour: longint;            { hours since midnight - [0,23] }
    tm_mday: longint;            { day of the month - [1,31] }
    tm_mon:  longint;            { months since January - [0,11] }
    tm_year: longint;            { years - [1980..2044] }
  end;

  tm_unz = tm_zip;

const
  Z_BUFSIZE = (16384);
  Z_MAXFILENAMEINZIP = (256);

const
  CENTRALHEADERMAGIC = $02014b50;

const
  SIZECENTRALDIRITEM = $2e;
  SIZEZIPLOCALHEADER = $1e;

const
  Paszip_copyright: PChar = ' Paszip Copyright 2000 Jacques Nomssi Nzali ';

implementation

{$ifdef Streams}
{ ---------------------------------------------------------------- }

function fopen(filename: PChar; mode: open_mode): FILEptr;
var
  fp: FILEptr;
begin
  fp := nil;
  try
    case mode of
      fopenread: fp  := TFileStream.Create(strpas(filename), fmOpenRead or fmShareDenyWrite);
      fopenwrite: fp := TFileStream.Create(strpas(filename), fmCreate);
      fappendwrite:
      begin
        fp := TFileStream.Create(strpas(filename), fmOpenReadWrite);
        fp.Seek(soFromEnd, 0);
      end;
    end;
  except
    on EFOpenError do
      fp := nil;
  end;
  fopen := fp;
end;

procedure fclose(fp: FILEptr);
begin
  fp.Free;
end;

function fread(buf: pointer; recSize: longint; recCount: longint; fp: FILEptr): longint;
var
  totalSize, readcount: longint;
begin
  if Assigned(buf) then
  begin
    totalSize := recCount * longint(recSize);
    readCount := fp.Read(buf^, totalSize);
    if (readcount <> totalSize) then
      fread := readcount div recSize
    else
      fread := recCount;
  end
  else
    fread := 0;
end;

function fwrite(buf: pointer; recSize: longint; recCount: longint; fp: FILEptr): longint;
var
  totalSize, written: longint;
begin
  if Assigned(buf) then
  begin
    totalSize := recCount * longint(recSize);
    written   := fp.Write(buf^, totalSize);
    if (written <> totalSize) then
      fwrite := written div recSize
    else
      fwrite := recCount;
  end
  else
    fwrite := 0;
end;

function fseek(fp: FILEptr; recPos: longint; mode: seek_mode): int;
const
  fsmode: array[seek_mode] of word = (soFromBeginning, soFromCurrent, soFromEnd);
begin
  fp.Seek(recPos, fsmode[mode]);
  fseek := 0; { = 0 for success }
end;

function ftell(fp: FILEptr): longint;
begin
  ftell := fp.Position;
end;

function feof(fp: FILEptr): longint;
begin
  feof := 0;
  if Assigned(fp) then
    if fp.Position = fp.Size then
      feof := 1
    else
      feof := 0;
end;

{$else}
{ ---------------------------------------------------------------- }

function fopen(filename : PChar; mode : open_mode) : FILEptr;
var
  fp : FILEptr;
  OldFileMode : byte;
begin
  fp := NIL;
  OldFileMode := FileMode;

  GetMem(fp, SizeOf(file));
  Assign(fp^, strpas(filename));
  {$push}{$i-}
  Case mode of
  fopenread:
    begin
      FileMode := 0;
      Reset(fp^, 1);
    end;
  fopenwrite:
    begin
      FileMode := 1;
      ReWrite(fp^, 1);
    end;
  fappendwrite :
    begin
      FileMode := 2;
      Reset(fp^, 1);
      Seek(fp^, FileSize(fp^));
    end;
  end;
  FileMode := OldFileMode;
  {$pop}
  if IOresult<>0 then
  begin
    FreeMem(fp, SizeOf(file));
    fp := NIL;
  end;

  fopen := fp;
end;

procedure fclose(fp : FILEptr);
begin
  if Assigned(fp) then
  begin
    {$push}{$i-}
    system.close(fp^);
    {$pop}
    if IOresult=0 then;
    FreeMem(fp, SizeOf(file));
  end;
end;

function fread(buf : pointer;
               recSize : LongInt;
               recCount : LongInt;
               fp : FILEptr) : LongInt;
var
  totalSize, readcount : LongInt;
begin
  if Assigned(buf) then
  begin
    totalSize := recCount * LongInt(recSize);
    {$push}{$i-}
    system.BlockRead(fp^, buf^, totalSize, readcount);
    if (readcount <> totalSize) then
      fread := readcount div recSize
    else
      fread := recCount;
    {$pop}
  end
  else
    fread := 0;
end;

function fwrite(buf : pointer;
                recSize : LongInt;
                recCount : LongInt;
                fp : FILEptr) : LongInt;
var
  totalSize, written : LongInt;
begin
  if Assigned(buf) then
  begin
    totalSize := recCount * LongInt(recSize);
    {$push}{$i-}
    system.BlockWrite(fp^, buf^, totalSize, written);
    if (written <> totalSize) then
      fwrite := written div recSize
    else
      fwrite := recCount;
    {$pop}
  end
  else
    fwrite := 0;
end;

function fseek(fp : FILEptr;
               recPos : LongInt;
               mode : seek_mode) : longint;
begin
  {$push}{$i-}
  case mode of
    SEEK_SET : system.Seek(fp^, recPos);
    SEEK_CUR : system.Seek(fp^, FilePos(fp^)+recPos);
    SEEK_END : system.Seek(fp^, FileSize(fp^)-1-recPos); { ?? check }
  end;
  {$pop}
  fseek := IOresult; { = 0 for success }
end;

function ftell(fp : FILEptr) : LongInt;
begin
  ftell := FilePos(fp^);
end;

function feof(fp : FILEptr) : LongInt;
begin
  feof := 0;
  if Assigned(fp) then
    if eof(fp^) then
      feof := 1
    else
      feof := 0;
end;

{$endif}
{ ---------------------------------------------------------------- }

end.
