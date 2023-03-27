unit Unzip;

{$mode tp}

{ ----------------------------------------------------------------- }
{ unzip.c -- IO on .zip files using zlib
   Version 0.15 beta, Mar 19th, 1998,
  unzip.h -- IO for uncompress .zip files using zlib
  Version 0.15 beta, Mar 19th, 1998,

  Copyright (C) 1998 Gilles Vollant <info@winimage.com>
  http://www.winimage.com/zLibDll/zip.htm

   This unzip package allow extract file from .ZIP file, compatible
   with PKZip 2.04g, WinZip, InfoZip tools and compatible.
   Encryption and multi volume ZipFile (span) are not supported.
   Old compressions used by old PKZip 1.x are not supported

  Pascal tranlastion
  Copyright (C) 2000 by Jacques Nomssi Nzali
  For conditions of distribution and use, see copyright notice in readme.txt }


interface

{$ifdef WIN32}
  {$define Delphi}
{$endif}

uses
  //zutil,
  zbase,
  //zLib,
  ziputils;

const
  UNZ_OK    = (0);
  UNZ_END_OF_LIST_OF_FILE = (-100);
   UNZ_ERRNO = (Z_ERRNO);
  UNZ_EOF   = (0);
  UNZ_PARAMERROR = (-102);
  UNZ_BADZIPFILE = (-103);
  UNZ_INTERNALERROR = (-104);
  UNZ_CRCERROR = (-105);
(*
{ tm_unz contain date/time info }
type
 tm_unz = record
   tm_sec : integer;       { seconds after the minute - [0,59] }
   tm_min : integer;       { minutes after the hour - [0,59] }
   tm_hour : integer;      { hours since midnight - [0,23] }
   tm_mday : integer;      { day of the month - [1,31] }
   tm_mon : integer;       { months since January - [0,11] }
   tm_year : integer;      { years - [1980..2044] }
  end;
*)
{ unz_global_info structure contain global data about the ZIPfile
  These data comes from the end of central dir }
type
  unz_global_info = record
    number_entry: longint;   { total number of entries in
                              the central dir on this disk }
    size_comment: longint;   { size of the global comment of the zipfile }
  end;


{ unz_file_info contain information about a file in the zipfile }
type
  unz_file_info = record
    version: longint;                  { version made by                 2 bytes }
    version_needed: longint;           { version needed to extract       2 bytes }
    flag:    longint;                  { general purpose bit flag        2 bytes }
    compression_method: longint;       { compression method              2 bytes }
    dosDate: longint;                  { last mod file date in Dos fmt   4 bytes }
    crc:     longint;                  { crc-32                          4 bytes }
    compressed_size: longint;          { compressed size                 4 bytes }
    uncompressed_size: longint;        { uncompressed size               4 bytes }
    size_filename: longint;            { filename length                 2 bytes }
    size_file_extra: longint;          { extra field length              2 bytes }
    size_file_comment: longint;        { file comment length             2 bytes }

    disk_num_start: longint;          { disk number start               2 bytes }
    internal_fa:    longint;          { internal file attributes        2 bytes }
    external_fa:    longint;          { external file attributes        4 bytes }

    tmu_date: tm_unz;
  end;
  unz_file_info_ptr = ^unz_file_info;


function unzStringFileNameCompare(const fileName1: PChar; const fileName2: PChar; iCaseSensitivity: longint): longint;
{ Compare two filename (fileName1,fileName2).
  If iCaseSenisivity = 1 (1=true),
    comparision is case sensitive (like strcmp)
  If iCaseSenisivity = 2 (0=false),
    comparision is not case sensitive (like strcmpi or strcasecmp)
  If iCaseSenisivity = 0, case sensitivity is defaut of your
    operating system like 1 on Unix, 2 on Windows)
}


function unzOpen(const path: PChar): unzFile;

{ Open a Zip file. path contain the full pathname (by example,
  on a Windows NT computer "c:\\zlib\\zlib111.zip" or on an Unix computer
  "zlib/zlib111.zip".
  If the zipfile cannot be opened (file don't exist or in not valid), the
  return value is NIL.
  Else, the return value is a unzFile Handle, usable with other function
     of this unzip package.
}

function unzClose(afile: unzFile): longint;

{ Close a ZipFile opened with unzipOpen.
  If there are files inside the .Zip opened with unzOpenCurrentFile()
  (see later), these files MUST be closed with unzipCloseCurrentFile()
  before a call unzipClose.
  return UNZ_OK if there is no problem. }

function unzGetGlobalInfo(afile: unzFile; var pglobal_info: unz_global_info): longint;

{ Write info about the ZipFile in the *pglobal_info structure.
  No preparation of the structure is needed
  return UNZ_OK if there is no problem. }

function unzGetGlobalComment(afile: unzFile; szComment: PChar; uSizeBuf: longint): longint;

{ Get the global comment string of the ZipFile, in the szComment buffer.
  uSizeBuf is the size of the szComment buffer.
  return the number of byte copied or an error code <0 }

 {***************************************************************************}
 { Unzip package allow you browse the directory of the zipfile }

function unzGoToFirstFile(afile: unzFile): longint;

{ Set the current file of the zipfile to the first file.
  return UNZ_OK if there is no problem }

function unzGoToNextFile(afile: unzFile): longint;

{ Set the current file of the zipfile to the next file.
  return UNZ_OK if there is no problem
  return UNZ_END_OF_LIST_OF_FILE if the actual file was the latest. }


function unzLocateFile(afile: unzFile; const szFileName: PChar; iCaseSensitivity: longint): longint; { ZEXPORT }

{ Try locate the file szFileName in the zipfile.
  For the iCaseSensitivity signification, see unzStringFileNameCompare

  return value :
  UNZ_OK if the file is found. It becomes the current file.
  UNZ_END_OF_LIST_OF_FILE if the file is not found }


function unzGetCurrentFileInfo(afile: unzFile; pfile_info: unz_file_info_ptr; szFileName: PChar; fileNameBufferSize: longint; extraField: pointer; extraFieldBufferSize: longint; szComment: PChar; commentBufferSize: longint): longint; { ZEXPORT }

{ Get Info about the current file
  if pfile_info<>NIL, the pfile_info^ structure will contain somes
  info about the current file
  if szFileName<>NIL, the filemane string will be copied in szFileName
      (fileNameBufferSize is the size of the buffer)
  if extraField<>NIL, the extra field information will be copied in
    extraField  (extraFieldBufferSize is the size of the buffer).
    This is the Central-header version of the extra field
  if szComment<>NIL, the comment string of the file will be copied in
    szComment (commentBufferSize is the size of the buffer) }


{***************************************************************************}
{* for reading the content of the current zipfile, you can open it, read data
   from it, and close it (you can close it before reading all the file) }


function unzOpenCurrentFile(afile: unzFile): longint; { ZEXPORT }

{ Open for reading data the current file in the zipfile.
  If there is no error, the return value is UNZ_OK. }


function unzCloseCurrentFile(afile: unzFile): longint; { ZEXPORT }

{ Close the file in zip opened with unzOpenCurrentFile
  Return UNZ_CRCERROR if all the file was read but the CRC is not good }


function unzReadCurrentFile(afile: unzFile; buf: pointer; len: cardinal): longint; { ZEXPORT }

{ Read bytes from the current file (opened by unzOpenCurrentFile)
  buf contain buffer where data must be copied
  len the size of buf.

  return the number of byte copied if somes bytes are copied
  return 0 if the end of file was reached
  return <0 with error code if there is an error
    (UNZ_ERRNO for IO error, or zLib error for uncompress error) }

function unztell(afile: unzFile): z_off_t;

{ Give the current position in uncompressed data }

function unzeof(afile: unzFile): longint;

{ return 1 if the end of file was reached, 0 elsewhere
  ! checks for valid params }

function unzGetLocalExtrafield(afile: unzFile; buf: pointer; len: cardinal): longint;
{ Read extra field from the current file (opened by unzOpenCurrentFile)
  This is the local-header version of the extra field (sometimes, there is
    more info in the local-header version than in the central-header)

  if buf=NIL, it return the size of the local extra field

  if buf<>NIL, len is the size of the buffer, the extra header is copied in
  buf.
  the return value is the number of bytes copied in buf, or (if <0)
  the error code }


{ ----------------------------------------------------------------- }

implementation

uses
  {$ifdef Delphi}
  SysUtils,
  {$else}
  strings,
  {$endif}
  zInflate, crc;

{$ifdef unix and not def (CASESENSITIVITYDEFAULT_YES) and \
                      !defined(CASESENSITIVITYDEFAULT_NO)}
{$define CASESENSITIVITYDEFAULT_NO}
{$endif}


const
  UNZ_BUFSIZE = Z_BUFSIZE;
  UNZ_MAXFILENAMEINZIP = Z_MAXFILENAMEINZIP;

const
  unz_copyright: PChar = ' unzip 0.15 Copyright 1998 Gilles Vollant ';

{ unz_file_info_internal contain internal info about a file in zipfile }
type
  unz_file_info_internal = record
    offset_curfile: longint; { relative offset of local header 4 bytes }
  end;
  unz_file_info_internal_ptr = ^unz_file_info_internal;


{ file_in_zip_read_info_s contain internal information about a file
  in zipfile, when reading and decompress it }
type
  file_in_zip_read_info_s = record
    read_buffer: PChar;       { internal buffer for compressed data }
    stream:      z_stream;    { zLib stream structure for inflate }

    pos_in_zipfile:     longint;       { position in byte on the zipfile, for fseek}
    stream_initialised: boolean;     { flag set if stream structure is initialised}

    offset_local_extrafield: longint;   { offset of the local extra field }
    size_local_extrafield:   smallint;    { size of the local extra field }
    pos_local_extrafield:    longint;   { position in the local extra field in read}

    crc32:      longint;                { crc32 of all data uncompressed }
    crc32_wait: longint;                { crc32 we must obtain after decompress all }
    rest_read_compressed: longint;      { number of byte to be decompressed }
    rest_read_uncompressed: longint;    {number of byte to be obtained after decomp}
    afile:      FILEptr;              { io structure of the zipfile }
    compression_method: longint;        { compression method (0=store) }
    byte_before_the_zipfile: longint;   { byte before the zipfile, (>0 for sfx) }
  end;
  file_in_zip_read_info_s_ptr = ^file_in_zip_read_info_s;


{ unz_s contain internal information about the zipfile }
type
  unz_s = record
    afile: FILEptr;                 { io structore of the zipfile }
    gi:    unz_global_info;         { public global information }
    byte_before_the_zipfile: longint; { byte before the zipfile, (>0 for sfx)}
    num_file: longint;                { number of the current file in the zipfile}
    pos_in_central_dir: longint;      { pos of the current file in the central dir}
    current_file_ok: boolean;       { flag about the usability of the current file}
    central_pos: longint;             { position of the beginning of the central dir}

    size_central_dir:   longint;     { size of the central directory  }
    offset_central_dir: longint;   { offset of start of central directory with
                                   respect to the starting disk number }

    cur_file_info:     unz_file_info; { public info about the current file in zip}
    cur_file_info_internal: unz_file_info_internal; { private info about it}
    pfile_in_zip_read: file_in_zip_read_info_s_ptr; { structure about the current
                                      file if we are decompressing it }
  end;
  unz_s_ptr = ^unz_s;


{ ===========================================================================
  Read a byte from a gz_stream; update next_in and avail_in. Return EOF
  for end of file.
  IN assertion: the stream s has been sucessfully opened for reading. }


function unzlocal_getByte(fin: FILEptr; var pi: longint): longint;
var
  c:   byte;
  err: longint;
begin
  err := fread(@c, 1, 1, fin);

  if (err = 1) then
  begin
    pi := longint(c);
    unzlocal_getByte := UNZ_OK;
    {exit;}
  end
  else
  if feof(fin) = 1 then    {if ferror(fin) then}
    unzlocal_getByte := UNZ_ERRNO
  else
    unzlocal_getByte := UNZ_EOF{exit;};
end;


{ ===========================================================================
   Reads a long in LSB order from the given gz_stream. Sets }

function unzlocal_getShort(fin: FILEptr; var pX: longint): longint;
var
  x:   longint;
  i:   longint;
  err: longint;
begin
  err := unzlocal_getByte(fin, i);
  x   := longint(i);

  if (err = UNZ_OK) then
    err := unzlocal_getByte(fin, i);
  Inc(x, longint(i) shl 8);

  if (err = UNZ_OK) then
    pX := x
  else
    pX := 0;
  unzlocal_getShort := err;
end;

function unzlocal_getLong(fin: FILEptr; var pX: longint): longint;
var
  x:   longint;
  i:   longint;
  err: longint;
begin
  err := unzlocal_getByte(fin, i);
  x   := longint(i);

  if (err = UNZ_OK) then
    err := unzlocal_getByte(fin, i);
  Inc(x, longint(i) shl 8);

  if (err = UNZ_OK) then
    err := unzlocal_getByte(fin, i);
  Inc(x, longint(i) shl 16);

  if (err = UNZ_OK) then
    err := unzlocal_getByte(fin, i);
  Inc(x, longint(i) shl 24);

  if (err = UNZ_OK) then
    pX := x
  else
    pX := 0;
  unzlocal_getLong := err;
end;


{ My own strcmpi / strcasecmp }
function strcmpcasenosensitive_internal(fileName1: PChar; fileName2: PChar): longint;
var
  c1, c2: char;
begin
  repeat
    c1 := fileName1^;
    Inc(fileName1);
    c2 := fileName2^;
    Inc(fileName2);
    if (c1 >= 'a') and (c1 <= 'z') then
      Dec(c1, $20);
    if (c2 >= 'a') and (c2 <= 'z') then
      Dec(c2, $20);
    if (c1 = #0) then
    begin
      if c2 = #0 then
        strcmpcasenosensitive_internal := 0
      else
        strcmpcasenosensitive_internal := -1;
      exit;
    end;
    if (c2 = #0) then
    begin
      strcmpcasenosensitive_internal := 1;
      exit;
    end;
    if (c1 < c2) then
    begin
      strcmpcasenosensitive_internal := -1;
      exit;
    end;
    if (c1 > c2) then
    begin
      strcmpcasenosensitive_internal := 1;
      exit;
    end;
  until False;
end;


const
  CASESENSITIVITYDEFAULTVALUE = 2;

function unzStringFileNameCompare(const fileName1: PChar; const fileName2: PChar; iCaseSensitivity: longint): longint; { ZEXPORT }
{ Compare two filename (fileName1,fileName2).
  If iCaseSenisivity = 1 (1=true),
    comparision is case sensitive (like strcmp)
  If iCaseSenisivity = 2 (0=false),
    comparision is not case sensitive (like strcmpi or strcasecmp)
  If iCaseSenisivity = 0, case sensitivity is defaut of your
    operating system like 1 on Unix, 2 on Windows)
}
begin
  if (iCaseSensitivity = 0) then
    iCaseSensitivity := CASESENSITIVITYDEFAULTVALUE;

  if (iCaseSensitivity = 1) then
  begin
    unzStringFileNameCompare := strComp(fileName1, fileName2);
    exit;
  end;

  unzStringFileNameCompare := strcmpcasenosensitive_internal(fileName1, fileName2);
end;

const
  BUFREADCOMMENT = $400;

{ Locate the Central directory of a zipfile (at the end, just before
  the global comment) }

function unzlocal_SearchCentralDir(fin: FILEptr): longint;
var
  buf: Pbytearray;
  uSizeFile: longint;
  uBackRead: longint;
  uMaxBack: longint;
  uPosFound: longint;
var
  uReadSize, uReadPos: longint;
  i: longint;
begin
  uMaxBack  := $ffff; { maximum size of global comment }
  uPosFound := 0;

  if (fseek(fin, 0, SEEK_END) <> 0) then
  begin
    unzlocal_SearchCentralDir := 0;
    exit;
  end;

  uSizeFile := ftell(fin);

  if (uMaxBack > uSizeFile) then
    uMaxBack := uSizeFile;

  buf := Pbytearray(AllocMem(BUFREADCOMMENT + 4));
  if (buf = nil) then
  begin
    unzlocal_SearchCentralDir := 0;
    exit;
  end;

  uBackRead := 4;
  while (uBackRead < uMaxBack) do
  begin

    if (uBackRead + BUFREADCOMMENT > uMaxBack) then
      uBackRead := uMaxBack
    else
      Inc(uBackRead, BUFREADCOMMENT);
    uReadPos := uSizeFile - uBackRead;

    if ((BUFREADCOMMENT + 4) < (uSizeFile - uReadPos)) then
      uReadSize := (BUFREADCOMMENT + 4)
    else
      uReadSize := (uSizeFile - uReadPos);

    if fseek(fin, uReadPos, SEEK_SET) <> 0 then
      break;

    if fread(buf, uReadSize, 1, fin) <> 1 then
      break;

    i := uReadSize - 3;
    while (i > 0) do
    begin
      Dec(i);
      if (buf^[i] = $50) and (buf^[i + 1] = $4b) and    { ENDHEADERMAGIC }
        (buf^[i + 2] = $05) and (buf^[i + 3] = $06) then
      begin
        uPosFound := uReadPos + i;
        break;
      end;
    end;

    if (uPosFound <> 0) then
      break;
  end;
  FreeMem(buf);
  unzlocal_SearchCentralDir := uPosFound;
end;


{ Open a Zip file. path contain the full pathname (by example,
  on a Windows NT computer "c:\\zlib\\zlib111.zip" or on an Unix computer
  "zlib/zlib111.zip".
  If the zipfile cannot be opened (file don't exist or in not valid), the
  return value is NIL.
  Else, the return value is a unzFile Handle, usable with other function
     of this unzip package.
}

function unzOpen(const path: PChar): unzFile; { ZEXPORT }
var
  us:  unz_s;
  s:   unz_s_ptr;
  central_pos, uL: longint;
  fin: FILEptr;

  number_disk:     longint; { number of the current dist, used for spaning ZIP,
                         unsupported, always 0 }
  number_disk_with_CD: longint; { number the the disk with central dir,
                        used for spaning ZIP, unsupported, always 0 }
  number_entry_CD: longint; { total number of entries in the central dir
                                 (same than number_entry on nospan) }

  err: longint;
begin
  err := UNZ_OK;

  if (unz_copyright[0] <> ' ') then
  begin
    unzOpen := nil;
    exit;
  end;

  fin := fopen(path, fopenread);
  if (fin = nil) then
  begin
    unzOpen := nil;
    exit;
  end;

  central_pos := unzlocal_SearchCentralDir(fin);
  if (central_pos = 0) then
    err := UNZ_ERRNO;

  if (fseek(fin, central_pos, SEEK_SET) <> 0) then
    err := UNZ_ERRNO;

  { the signature, already checked }
  if (unzlocal_getLong(fin, uL) <> UNZ_OK) then
    err := UNZ_ERRNO;

  { number of this disk }
  if (unzlocal_getShort(fin, number_disk) <> UNZ_OK) then
    err := UNZ_ERRNO;

  { number of the disk with the start of the central directory }
  if (unzlocal_getShort(fin, number_disk_with_CD) <> UNZ_OK) then
    err := UNZ_ERRNO;

  { total number of entries in the central dir on this disk }
  if (unzlocal_getShort(fin, us.gi.number_entry) <> UNZ_OK) then
    err := UNZ_ERRNO;

  { total number of entries in the central dir }
  if (unzlocal_getShort(fin, number_entry_CD) <> UNZ_OK) then
    err := UNZ_ERRNO;

  if ((number_entry_CD <> us.gi.number_entry) or
    (number_disk_with_CD <> 0) or
    (number_disk <> 0)) then
    err := UNZ_BADZIPFILE;

  { size of the central directory }
  if (unzlocal_getLong(fin, us.size_central_dir) <> UNZ_OK) then
    err := UNZ_ERRNO;

  { offset of start of central directory with respect to the
        starting disk number }
  if (unzlocal_getLong(fin, us.offset_central_dir) <> UNZ_OK) then
    err := UNZ_ERRNO;

  { zipfile comment length }
  if (unzlocal_getShort(fin, us.gi.size_comment) <> UNZ_OK) then
    err := UNZ_ERRNO;

  if ((central_pos < us.offset_central_dir + us.size_central_dir) and
    (err = UNZ_OK)) then
    err := UNZ_BADZIPFILE;

  if (err <> UNZ_OK) then
  begin
    fclose(fin);
    unzOpen := nil;
    exit;
  end;

  us.afile := fin;
  us.byte_before_the_zipfile := central_pos -
    (us.offset_central_dir + us.size_central_dir);
  us.central_pos := central_pos;
  us.pfile_in_zip_read := nil;

  s  := unz_s_ptr(AllocMem(sizeof(unz_s)));
  s^ := us;
  unzGoToFirstFile(unzFile(s));
  unzOpen := unzFile(s);
end;


{ Close a ZipFile opened with unzipOpen.
  If there are files inside the .Zip opened with unzOpenCurrentFile()
  (see later), these files MUST be closed with unzipCloseCurrentFile()
  before a call unzipClose.
  return UNZ_OK if there is no problem. }

function unzClose(afile: unzFile): longint; { ZEXPORT }
var
  s: unz_s_ptr;
begin
  if (afile = nil) then
  begin
    unzClose := UNZ_PARAMERROR;
    exit;
  end;
  s := unz_s_ptr(afile);

  if (s^.pfile_in_zip_read <> nil) then
    unzCloseCurrentFile(afile);

  fclose(s^.afile);
  FreeMem(s);
  unzClose := UNZ_OK;
end;

{ Write info about the ZipFile in the pglobal_info structure.
  No preparation of the structure is needed
  return UNZ_OK if there is no problem. }

function unzGetGlobalInfo(afile: unzFile; var pglobal_info: unz_global_info): longint; { ZEXPORT }
var
  s: unz_s_ptr;
begin
  if (afile = nil) then
  begin
    unzGetGlobalInfo := UNZ_PARAMERROR;
    exit;
  end;
  s := unz_s_ptr(afile);
  pglobal_info := s^.gi;
  unzGetGlobalInfo := UNZ_OK;
end;


{ Translate date/time from Dos format to tm_unz (more easily readable) }
procedure unzlocal_DosDateToTmuDate(ulDosDate: longint; var ptm: tm_unz);
var
  uDate: longint;
begin
  uDate      := longint(ulDosDate shr 16);
  ptm.tm_mday := integer(uDate and $1f);
  ptm.tm_mon := integer((((uDate) and $1E0) div $20) - 1);
  ptm.tm_year := integer(((uDate and $0FE00) div $0200) + 1980);

  ptm.tm_hour := integer((ulDosDate and $F800) div $800);
  ptm.tm_min  := integer((ulDosDate and $7E0) div $20);
  ptm.tm_sec  := integer(2 * (ulDosDate and $1f));
end;

{ Get Info about the current file in the zipfile, with internal only info }
function unzlocal_GetCurrentFileInfoInternal(afile: unzFile; pfile_info: unz_file_info_ptr; pfile_info_internal: unz_file_info_internal_ptr; szFileName: PChar; fileNameBufferSize: longint; extraField: pointer; extraFieldBufferSize: longint; szComment: PChar; commentBufferSize: longint): longint;
var
  s:      unz_s_ptr;
  file_info: unz_file_info;
  file_info_internal: unz_file_info_internal;
  err:    longint;
  uMagic: longint;
  lSeek:  longint;
var
  uSizeRead: longint;
begin
  err   := UNZ_OK;
  lSeek := 0;
  if (afile = nil) then
  begin
    unzlocal_GetCurrentFileInfoInternal := UNZ_PARAMERROR;
    exit;
  end;
  s := unz_s_ptr(afile);

  if (fseek(s^.afile,
    s^.pos_in_central_dir + s^.byte_before_the_zipfile, SEEK_SET) <> 0) then
    err := UNZ_ERRNO;

  { we check the magic }
  if (err = UNZ_OK) then
    if (unzlocal_getLong(s^.afile, uMagic) <> UNZ_OK) then
      err := UNZ_ERRNO
    else
    if (uMagic <> CENTRALHEADERMAGIC) then
      err := UNZ_BADZIPFILE;

  if (unzlocal_getShort(s^.afile, file_info.version) <> UNZ_OK) then
    err := UNZ_ERRNO;

  if (unzlocal_getShort(s^.afile, file_info.version_needed) <> UNZ_OK) then
    err := UNZ_ERRNO;

  if (unzlocal_getShort(s^.afile, file_info.flag) <> UNZ_OK) then
    err := UNZ_ERRNO;

  if (unzlocal_getShort(s^.afile, file_info.compression_method) <> UNZ_OK) then
    err := UNZ_ERRNO;

  if (unzlocal_getLong(s^.afile, file_info.dosDate) <> UNZ_OK) then
    err := UNZ_ERRNO;

  unzlocal_DosDateToTmuDate(file_info.dosDate, file_info.tmu_date);

  if (unzlocal_getLong(s^.afile, file_info.crc) <> UNZ_OK) then
    err := UNZ_ERRNO;

  if (unzlocal_getLong(s^.afile, file_info.compressed_size) <> UNZ_OK) then
    err := UNZ_ERRNO;

  if (unzlocal_getLong(s^.afile, file_info.uncompressed_size) <> UNZ_OK) then
    err := UNZ_ERRNO;

  if (unzlocal_getShort(s^.afile, file_info.size_filename) <> UNZ_OK) then
    err := UNZ_ERRNO;

  if (unzlocal_getShort(s^.afile, file_info.size_file_extra) <> UNZ_OK) then
    err := UNZ_ERRNO;

  if (unzlocal_getShort(s^.afile, file_info.size_file_comment) <> UNZ_OK) then
    err := UNZ_ERRNO;

  if (unzlocal_getShort(s^.afile, file_info.disk_num_start) <> UNZ_OK) then
    err := UNZ_ERRNO;

  if (unzlocal_getShort(s^.afile, file_info.internal_fa) <> UNZ_OK) then
    err := UNZ_ERRNO;

  if (unzlocal_getLong(s^.afile, file_info.external_fa) <> UNZ_OK) then
    err := UNZ_ERRNO;

  if (unzlocal_getLong(s^.afile, file_info_internal.offset_curfile) <> UNZ_OK) then
    err := UNZ_ERRNO;

  Inc(lSeek, file_info.size_filename);
  if ((err = UNZ_OK) and (szFileName <> nil)) then
  begin
    if (file_info.size_filename < fileNameBufferSize) then
    begin
      (szFileName +file_info.size_filename)^ := #0;
      uSizeRead := file_info.size_filename;
    end
    else
      uSizeRead := fileNameBufferSize;

    if (file_info.size_filename > 0) and (fileNameBufferSize > 0) then
      if fread(szFileName, uSizeRead, 1, s^.afile) <> 1 then
        err := UNZ_ERRNO;
    Dec(lSeek, uSizeRead);
  end;

  if ((err = UNZ_OK) and (extraField <> nil)) then
  begin
    if (file_info.size_file_extra < extraFieldBufferSize) then
      uSizeRead := file_info.size_file_extra
    else
      uSizeRead := extraFieldBufferSize;

    if (lSeek <> 0) then
      if (fseek(s^.afile, lSeek, SEEK_CUR) = 0) then
        lSeek := 0
      else
        err   := UNZ_ERRNO;

    if ((file_info.size_file_extra > 0) and (extraFieldBufferSize > 0)) then
      if fread(extraField, uSizeRead, 1, s^.afile) <> 1 then
        err := UNZ_ERRNO;
    Inc(lSeek, file_info.size_file_extra - uSizeRead);
  end
  else
    Inc(lSeek, file_info.size_file_extra);

  if ((err = UNZ_OK) and (szComment <> nil)) then
  begin
    if (file_info.size_file_comment < commentBufferSize) then
    begin
      (szComment +file_info.size_file_comment)^ := #0;
      uSizeRead := file_info.size_file_comment;
    end
    else
      uSizeRead := commentBufferSize;

    if (lSeek <> 0) then
      if (fseek(s^.afile, lSeek, SEEK_CUR) = 0) then
        lSeek := 0
      else
        err   := UNZ_ERRNO;
    if ((file_info.size_file_comment > 0) and (commentBufferSize > 0)) then
      if fread(szComment, uSizeRead, 1, s^.afile) <> 1 then
        err := UNZ_ERRNO;
    Inc(lSeek, file_info.size_file_comment - uSizeRead);
  end
  else
    Inc(lSeek, file_info.size_file_comment);

  if ((err = UNZ_OK) and (pfile_info <> nil)) then
    pfile_info^ := file_info;

  if ((err = UNZ_OK) and (pfile_info_internal <> nil)) then
    pfile_info_internal^ := file_info_internal;

  unzlocal_GetCurrentFileInfoInternal := err;
end;


{ Write info about the ZipFile in the *pglobal_info structure.
  No preparation of the structure is needed
  return UNZ_OK if there is no problem. }

function unzGetCurrentFileInfo(afile: unzFile; pfile_info: unz_file_info_ptr; szFileName: PChar; fileNameBufferSize: longint; extraField: pointer; extraFieldBufferSize: longint; szComment: PChar; commentBufferSize: longint): longint; { ZEXPORT }

{ Get Info about the current file
  if pfile_info<>NIL, the pfile_info^ structure will contain somes
  info about the current file
  if szFileName<>NIL, the filemane string will be copied in szFileName
      (fileNameBufferSize is the size of the buffer)
  if extraField<>NIL, the extra field information will be copied in
    extraField  (extraFieldBufferSize is the size of the buffer).
    This is the Central-header version of the extra field
  if szComment<>NIL, the comment string of the file will be copied in
    szComment (commentBufferSize is the size of the buffer) }

begin
  unzGetCurrentFileInfo := unzlocal_GetCurrentFileInfoInternal(afile,
    pfile_info, nil, szFileName, fileNameBufferSize, extraField,
    extraFieldBufferSize, szComment, commentBufferSize);
end;


{ Set the current file of the zipfile to the first file.
  return UNZ_OK if there is no problem }

function unzGoToFirstFile(afile: unzFile): longint;  { ZEXPORT }
var
  err: longint;
  s:   unz_s_ptr;
begin
  if (afile = nil) then
  begin
    unzGoToFirstFile := UNZ_PARAMERROR;
    exit;
  end;
  s   := unz_s_ptr(afile);
  s^.pos_in_central_dir := s^.offset_central_dir;
  s^.num_file := 0;
  err := unzlocal_GetCurrentFileInfoInternal(afile, @s^.cur_file_info, @s^.cur_file_info_internal, nil, 0, nil, 0, nil, 0);
  s^.current_file_ok := (err = UNZ_OK);
  unzGoToFirstFile := err;
end;


{ Set the current file of the zipfile to the next file.
  return UNZ_OK if there is no problem
  return UNZ_END_OF_LIST_OF_FILE if the actual file was the latest. }

function unzGoToNextFile(afile: unzFile): longint; { ZEXPORT }
var
  s:   unz_s_ptr;
  err: longint;
begin
  if (afile = nil) then
  begin
    unzGoToNextFile := UNZ_PARAMERROR;
    exit;
  end;
  s := unz_s_ptr(afile);
  if not s^.current_file_ok then
  begin
    unzGoToNextFile := UNZ_END_OF_LIST_OF_FILE;
    exit;
  end;
  if (s^.num_file + 1 = s^.gi.number_entry) then
  begin
    unzGoToNextFile := UNZ_END_OF_LIST_OF_FILE;
    exit;
  end;

  Inc(s^.pos_in_central_dir,
    SIZECENTRALDIRITEM + s^.cur_file_info.size_filename +
    s^.cur_file_info.size_file_extra + s^.cur_file_info.size_file_comment);
  Inc(s^.num_file);
  err := unzlocal_GetCurrentFileInfoInternal(afile, @s^.cur_file_info, @s^.cur_file_info_internal, nil, 0, nil, 0, nil, 0);
  s^.current_file_ok := (err = UNZ_OK);
  unzGoToNextFile := err;
end;


{ Try locate the file szFileName in the zipfile.
  For the iCaseSensitivity signification, see unzStringFileNameCompare

  return value :
  UNZ_OK if the file is found. It becomes the current file.
  UNZ_END_OF_LIST_OF_FILE if the file is not found }

function unzLocateFile(afile: unzFile; const szFileName: PChar; iCaseSensitivity: longint): longint; { ZEXPORT }
var
  s:   unz_s_ptr;
  err: longint;
  num_fileSaved: longint;
  pos_in_central_dirSaved: longint;
var
  szCurrentFileName: array[0..UNZ_MAXFILENAMEINZIP + 1 - 1] of char;
begin
  if (afile = nil) then
  begin
    unzLocateFile := UNZ_PARAMERROR;
    exit;
  end;

  if (strlen(szFileName) >= UNZ_MAXFILENAMEINZIP) then
  begin
    unzLocateFile := UNZ_PARAMERROR;
    exit;
  end;

  s := unz_s_ptr(afile);
  if (not s^.current_file_ok) then
  begin
    unzLocateFile := UNZ_END_OF_LIST_OF_FILE;
    exit;
  end;
  num_fileSaved := s^.num_file;
  pos_in_central_dirSaved := s^.pos_in_central_dir;

  err := unzGoToFirstFile(afile);

  while (err = UNZ_OK) do
  begin
    unzGetCurrentFileInfo(afile, nil,
      szCurrentFileName, sizeof(szCurrentFileName) - 1, nil, 0, nil, 0);
    if (unzStringFileNameCompare(szCurrentFileName,
      szFileName, iCaseSensitivity) = 0) then
    begin
      unzLocateFile := UNZ_OK;
      exit;
    end;
    err := unzGoToNextFile(afile);
  end;

  s^.num_file   := num_fileSaved;
  s^.pos_in_central_dir := pos_in_central_dirSaved;
  unzLocateFile := err;
end;


{ Read the local header of the current zipfile
  Check the coherency of the local header and info in the end of central
        directory about this file
  store in *piSizeVar the size of extra info in local header
        (filename and size of extra field data) }

function unzlocal_CheckCurrentFileCoherencyHeader(s: unz_s_ptr; var piSizeVar: longint; var poffset_local_extrafield: longint; var psize_local_extrafield: integer): longint;
var
  uMagic, uData, uFlags: longint;
  size_filename: longint;
  size_extra_field: longint;
  err: longint;
begin
  err := UNZ_OK;

  piSizeVar := 0;
  poffset_local_extrafield := 0;
  psize_local_extrafield := 0;

  if (fseek(s^.afile, s^.cur_file_info_internal.offset_curfile +
    s^.byte_before_the_zipfile, SEEK_SET) <> 0) then
  begin
    unzlocal_CheckCurrentFileCoherencyHeader := UNZ_ERRNO;
    exit;
  end;

  if (err = UNZ_OK) then
    if (unzlocal_getLong(s^.afile, uMagic) <> UNZ_OK) then
      err := UNZ_ERRNO
    else
    if (uMagic <> $04034b50) then
      err := UNZ_BADZIPFILE;

  if (unzlocal_getShort(s^.afile, uData) <> UNZ_OK) then
    err := UNZ_ERRNO;
{
  else
    if ((err=UNZ_OK) and (uData<>s^.cur_file_info.wVersion)) then
      err := UNZ_BADZIPFILE;
}
  if (unzlocal_getShort(s^.afile, uFlags) <> UNZ_OK) then
    err := UNZ_ERRNO;

  if (unzlocal_getShort(s^.afile, uData) <> UNZ_OK) then
    err := UNZ_ERRNO
  else
  if ((err = UNZ_OK) and (uData <> s^.cur_file_info.compression_method)) then
    err := UNZ_BADZIPFILE;

  if ((err = UNZ_OK) and (s^.cur_file_info.compression_method <> 0) and
    (s^.cur_file_info.compression_method <> Z_DEFLATED)) then
    err := UNZ_BADZIPFILE;

  if (unzlocal_getLong(s^.afile, uData) <> UNZ_OK) then { date/time }
    err := UNZ_ERRNO;

  if (unzlocal_getLong(s^.afile, uData) <> UNZ_OK) then { crc }
    err := UNZ_ERRNO
  else
  if ((err = UNZ_OK) and (uData <> s^.cur_file_info.crc) and
    ((uFlags and 8) = 0)) then
    err := UNZ_BADZIPFILE;

  if (unzlocal_getLong(s^.afile, uData) <> UNZ_OK) then { size compr }
    err := UNZ_ERRNO
  else
  if ((err = UNZ_OK) and (uData <> s^.cur_file_info.compressed_size) and
    ((uFlags and 8) = 0)) then
    err := UNZ_BADZIPFILE;

  if (unzlocal_getLong(s^.afile, uData) <> UNZ_OK) then { size uncompr }
    err := UNZ_ERRNO
  else
  if ((err = UNZ_OK) and (uData <> s^.cur_file_info.uncompressed_size) and
    ((uFlags and 8) = 0)) then
    err := UNZ_BADZIPFILE;


  if (unzlocal_getShort(s^.afile, size_filename) <> UNZ_OK) then
    err := UNZ_ERRNO
  else
  if ((err = UNZ_OK) and (size_filename <> s^.cur_file_info.size_filename)) then
    err := UNZ_BADZIPFILE;

  Inc(piSizeVar, integer(size_filename));

  if (unzlocal_getShort(s^.afile, size_extra_field) <> UNZ_OK) then
    err := UNZ_ERRNO;
  poffset_local_extrafield := s^.cur_file_info_internal.offset_curfile +
    SIZEZIPLOCALHEADER + size_filename;
  psize_local_extrafield := integer(size_extra_field);

  Inc(piSizeVar, integer(size_extra_field));

  unzlocal_CheckCurrentFileCoherencyHeader := err;
end;

{ Open for reading data the current file in the zipfile.
  If there is no error, the return value is UNZ_OK. }

function unzOpenCurrentFile(afile: unzFile): longint; { ZEXPORT }
var
  err: longint;
  Store: boolean;
  iSizeVar: longint;
  s: unz_s_ptr;
  pfile_in_zip_read_info: file_in_zip_read_info_s_ptr;
  offset_local_extrafield: longint;  { offset of the local extra field }
  size_local_extrafield: smallint;     { size of the local extra field }
begin
  err := UNZ_OK;

  if (afile = nil) then
  begin
    unzOpenCurrentFile := UNZ_PARAMERROR;
    exit;
  end;
  s := unz_s_ptr(afile);
  if not s^.current_file_ok then
  begin
    unzOpenCurrentFile := UNZ_PARAMERROR;
    exit;
  end;

  if (s^.pfile_in_zip_read <> nil) then
    unzCloseCurrentFile(afile);

  if (unzlocal_CheckCurrentFileCoherencyHeader(s, iSizeVar,
    offset_local_extrafield, size_local_extrafield) <> UNZ_OK) then
  begin
    unzOpenCurrentFile := UNZ_BADZIPFILE;
    exit;
  end;

  pfile_in_zip_read_info := file_in_zip_read_info_s_ptr(
    AllocMem(sizeof(file_in_zip_read_info_s)));
  if (pfile_in_zip_read_info = nil) then
  begin
    unzOpenCurrentFile := UNZ_INTERNALERROR;
    exit;
  end;

  pfile_in_zip_read_info^.read_buffer := PChar(AllocMem(UNZ_BUFSIZE));
  pfile_in_zip_read_info^.offset_local_extrafield := offset_local_extrafield;
  pfile_in_zip_read_info^.size_local_extrafield := size_local_extrafield;
  pfile_in_zip_read_info^.pos_local_extrafield := 0;

  if (pfile_in_zip_read_info^.read_buffer = nil) then
  begin
    FreeMem(pfile_in_zip_read_info);
    unzOpenCurrentFile := UNZ_INTERNALERROR;
    exit;
  end;

  pfile_in_zip_read_info^.stream_initialised := False;

  if ((s^.cur_file_info.compression_method <> 0) and
    (s^.cur_file_info.compression_method <> Z_DEFLATED)) then
    err := UNZ_BADZIPFILE;
  Store := s^.cur_file_info.compression_method = 0;

  pfile_in_zip_read_info^.crc32_wait := s^.cur_file_info.crc;
  pfile_in_zip_read_info^.crc32      := 0;
  pfile_in_zip_read_info^.compression_method := s^.cur_file_info.compression_method;
  pfile_in_zip_read_info^.afile      := s^.afile;
  pfile_in_zip_read_info^.byte_before_the_zipfile := s^.byte_before_the_zipfile;

  pfile_in_zip_read_info^.stream.total_out := 0;

  if (not Store) then
  begin
    err := inflateInit2(pfile_in_zip_read_info^.stream, -MAX_WBITS);

    if (err = Z_OK) then
      pfile_in_zip_read_info^.stream_initialised := True;
        { windowBits is passed < 0 to tell that there is no zlib header.
          Note that in this case inflate *requires* an extra "dummy" byte
          after the compressed stream in order to complete decompression and
          return Z_STREAM_END.
          In unzip, i don't wait absolutely Z_STREAM_END because I known the
          size of both compressed and uncompressed data }
  end;
  pfile_in_zip_read_info^.rest_read_compressed   := s^.cur_file_info.compressed_size;
  pfile_in_zip_read_info^.rest_read_uncompressed := s^.cur_file_info.uncompressed_size;


  pfile_in_zip_read_info^.pos_in_zipfile :=
    s^.cur_file_info_internal.offset_curfile + SIZEZIPLOCALHEADER + iSizeVar;

  pfile_in_zip_read_info^.stream.avail_in := 0;


  s^.pfile_in_zip_read := pfile_in_zip_read_info;
  unzOpenCurrentFile   := UNZ_OK;
end;


{ Read bytes from the current file (opened by unzOpenCurrentFile)
  buf contain buffer where data must be copied
  len the size of buf.

  return the number of byte copied if somes bytes are copied
  return 0 if the end of file was reached
  return <0 with error code if there is an error
    (UNZ_ERRNO for IO error, or zLib error for uncompress error) }

function unzReadCurrentFile(afile: unzFile; buf: pointer; len: cardinal): longint; { ZEXPORT }

var
  err:   longint;
  iRead: Longint;
  s:     unz_s_ptr;
  pfile_in_zip_read_info: file_in_zip_read_info_s_ptr;
var
  uReadThis: longint;
var
  uDoCopy, i: longint;
var
  uTotalOutBefore, uTotalOutAfter: longint;
  bufBefore: pbyte;
  uOutThis: longint;
  flush: longint;
begin
  err   := UNZ_OK;
  iRead := 0;
  if (afile = nil) then
  begin
    unzReadCurrentFile := UNZ_PARAMERROR;
    exit;
  end;
  s := unz_s_ptr(afile);
  pfile_in_zip_read_info := s^.pfile_in_zip_read;

  if (pfile_in_zip_read_info = nil) then
  begin
    unzReadCurrentFile := UNZ_PARAMERROR;
    exit;
  end;

  if ((pfile_in_zip_read_info^.read_buffer = nil)) then
  begin
    unzReadCurrentFile := UNZ_END_OF_LIST_OF_FILE;
    exit;
  end;

  if (len = 0) then
  begin
    unzReadCurrentFile := 0;
    exit;
  end;

  pfile_in_zip_read_info^.stream.next_out := pbyte(buf);

  pfile_in_zip_read_info^.stream.avail_out := len;

  if (len > pfile_in_zip_read_info^.rest_read_uncompressed) then
    pfile_in_zip_read_info^.stream.avail_out :=
      pfile_in_zip_read_info^.rest_read_uncompressed;

  while (pfile_in_zip_read_info^.stream.avail_out > 0) do
  begin
    if ((pfile_in_zip_read_info^.stream.avail_in = 0) and
      (pfile_in_zip_read_info^.rest_read_compressed > 0)) then
    begin
      uReadThis := UNZ_BUFSIZE;
      if (pfile_in_zip_read_info^.rest_read_compressed < uReadThis) then
        uReadThis := pfile_in_zip_read_info^.rest_read_compressed;
      if (uReadThis = 0) then
      begin
        unzReadCurrentFile := UNZ_EOF;
        exit;
      end;
      if (fseek(pfile_in_zip_read_info^.afile,
        pfile_in_zip_read_info^.pos_in_zipfile +
        pfile_in_zip_read_info^.byte_before_the_zipfile, SEEK_SET) <> 0) then
      begin
        unzReadCurrentFile := UNZ_ERRNO;
        exit;
      end;
      if fread(pfile_in_zip_read_info^.read_buffer, uReadThis, 1,
        pfile_in_zip_read_info^.afile) <> 1 then
      begin
        unzReadCurrentFile := UNZ_ERRNO;
        exit;
      end;
      Inc(pfile_in_zip_read_info^.pos_in_zipfile, uReadThis);

      Dec(pfile_in_zip_read_info^.rest_read_compressed, uReadThis);

      pfile_in_zip_read_info^.stream.next_in  :=
        pbyte(pfile_in_zip_read_info^.read_buffer);
      pfile_in_zip_read_info^.stream.avail_in := uReadThis;
    end;

    if (pfile_in_zip_read_info^.compression_method = 0) then
    begin
      if (pfile_in_zip_read_info^.stream.avail_out <
        pfile_in_zip_read_info^.stream.avail_in) then
        uDoCopy := pfile_in_zip_read_info^.stream.avail_out
      else
        uDoCopy := pfile_in_zip_read_info^.stream.avail_in;

      for i := 0 to uDoCopy - 1 do
        Pbytearray(pfile_in_zip_read_info^.stream.next_out)^[i] :=
          Pbytearray(pfile_in_zip_read_info^.stream.next_in)^[i];

      pfile_in_zip_read_info^.crc32 := crc32(pfile_in_zip_read_info^.crc32,
        pfile_in_zip_read_info^.stream.next_out, uDoCopy);
      Dec(pfile_in_zip_read_info^.rest_read_uncompressed, uDoCopy);
      Dec(pfile_in_zip_read_info^.stream.avail_in, uDoCopy);
      Dec(pfile_in_zip_read_info^.stream.avail_out, uDoCopy);
      Inc(pfile_in_zip_read_info^.stream.next_out, uDoCopy);
      Inc(pfile_in_zip_read_info^.stream.next_in, uDoCopy);
      Inc(pfile_in_zip_read_info^.stream.total_out, uDoCopy);
      Inc(iRead, uDoCopy);
    end
    else
    begin
      flush := Z_SYNC_FLUSH;

      uTotalOutBefore := pfile_in_zip_read_info^.stream.total_out;
      bufBefore := pfile_in_zip_read_info^.stream.next_out;

      {
      if ((pfile_in_zip_read_info^.rest_read_uncompressed =
     pfile_in_zip_read_info^.stream.avail_out) and
    (pfile_in_zip_read_info^.rest_read_compressed = 0)) then
        flush := Z_FINISH;
      }
      err := inflate(pfile_in_zip_read_info^.stream, flush);

      uTotalOutAfter := pfile_in_zip_read_info^.stream.total_out;
      uOutThis := uTotalOutAfter - uTotalOutBefore;

      pfile_in_zip_read_info^.crc32 :=
        crc32(pfile_in_zip_read_info^.crc32, bufBefore, uOutThis);

      Dec(pfile_in_zip_read_info^.rest_read_uncompressed, uOutThis);

      Inc(iRead, uTotalOutAfter - uTotalOutBefore);

      if (err = Z_STREAM_END) then
      begin
        if iRead = 0 then
          unzReadCurrentFile := UNZ_EOF
        else
          unzReadCurrentFile := iRead;
        exit;
      end;
      if (err <> Z_OK) then
        break;
    end;
  end; { while }

  if (err = Z_OK) then
  begin
    unzReadCurrentFile := iRead;
    exit;
  end;
  unzReadCurrentFile := err;
end;

{ Give the current position in uncompressed data }

function unztell(afile: unzFile): z_off_t; { ZEXPORT }
var
  s: unz_s_ptr;
  pfile_in_zip_read_info: file_in_zip_read_info_s_ptr;
begin
  if (afile = nil) then
  begin
    unztell := UNZ_PARAMERROR;
    exit;
  end;

  s := unz_s_ptr(afile);
  pfile_in_zip_read_info := s^.pfile_in_zip_read;

  if (pfile_in_zip_read_info = nil) then
  begin
    unztell := UNZ_PARAMERROR;
    exit;
  end;

  unztell := z_off_t(pfile_in_zip_read_info^.stream.total_out);
end;


{ return 1 (TRUE) if the end of file was reached, 0 elsewhere }

function unzeof(afile: unzFile): longint;
var
  s: unz_s_ptr;
  pfile_in_zip_read_info: file_in_zip_read_info_s_ptr;
begin
  if (afile = nil) then
  begin
    unzeof := UNZ_PARAMERROR;
    exit;
  end;

  s := unz_s_ptr(afile);
  pfile_in_zip_read_info := s^.pfile_in_zip_read;

  if (pfile_in_zip_read_info = nil) then
  begin
    unzeof := UNZ_PARAMERROR;
    exit;
  end;

  if (pfile_in_zip_read_info^.rest_read_uncompressed = 0) then
    unzeof := 1
  else
    unzeof := 0;
end;


{ Read extra field from the current file (opened by unzOpenCurrentFile)
  This is the local-header version of the extra field (sometimes, there is
    more info in the local-header version than in the central-header)

  if buf=NIL, it return the size of the local extra field

  if buf<>NIL, len is the size of the buffer, the extra header is copied in
  buf.
  the return value is the number of bytes copied in buf, or (if <0)
  the error code }

function unzGetLocalExtrafield(afile: unzFile; buf: pointer; len: cardinal): longint;
var
  s: unz_s_ptr;
  pfile_in_zip_read_info: file_in_zip_read_info_s_ptr;
  read_now: longint;
  size_to_read: longint;
begin
  if (afile = nil) then
  begin
    unzGetLocalExtrafield := UNZ_PARAMERROR;
    exit;
  end;

  s := unz_s_ptr(afile);
  pfile_in_zip_read_info := s^.pfile_in_zip_read;

  if (pfile_in_zip_read_info = nil) then
  begin
    unzGetLocalExtrafield := UNZ_PARAMERROR;
    exit;
  end;

  size_to_read := (pfile_in_zip_read_info^.size_local_extrafield -
    pfile_in_zip_read_info^.pos_local_extrafield);

  if (buf = nil) then
  begin
    unzGetLocalExtrafield := longint(size_to_read);
    exit;
  end;

  if (len > size_to_read) then
    read_now := size_to_read
  else
    read_now := len;

  if (read_now = 0) then
  begin
    unzGetLocalExtrafield := 0;
    exit;
  end;

  if (fseek(pfile_in_zip_read_info^.afile,
    pfile_in_zip_read_info^.offset_local_extrafield +
    pfile_in_zip_read_info^.pos_local_extrafield, SEEK_SET) <> 0) then
  begin
    unzGetLocalExtrafield := UNZ_ERRNO;
    exit;
  end;

  if fread(buf, size_to_read, 1, pfile_in_zip_read_info^.afile) <> 1 then
  begin
    unzGetLocalExtrafield := UNZ_ERRNO;
    exit;
  end;

  unzGetLocalExtrafield := longint(read_now);
end;

{ Close the file in zip opened with unzOpenCurrentFile
  Return UNZ_CRCERROR if all the file was read but the CRC is not good }

function unzCloseCurrentFile(afile: unzFile): longint; { ZEXPORT }
var
  err: longint;
  s:   unz_s_ptr;
  pfile_in_zip_read_info: file_in_zip_read_info_s_ptr;
begin
  err := UNZ_OK;

  if (afile = nil) then
  begin
    unzCloseCurrentFile := UNZ_PARAMERROR;
    exit;
  end;
  s := unz_s_ptr(afile);
  pfile_in_zip_read_info := s^.pfile_in_zip_read;

  if (pfile_in_zip_read_info = nil) then
  begin
    unzCloseCurrentFile := UNZ_PARAMERROR;
    exit;
  end;


  if (pfile_in_zip_read_info^.rest_read_uncompressed = 0) then
    if (pfile_in_zip_read_info^.crc32 <> pfile_in_zip_read_info^.crc32_wait) then
      err := UNZ_CRCERROR;


  FreeMem(pfile_in_zip_read_info^.read_buffer);
  pfile_in_zip_read_info^.read_buffer := nil;
  if (pfile_in_zip_read_info^.stream_initialised) then
    inflateEnd(pfile_in_zip_read_info^.stream);

  pfile_in_zip_read_info^.stream_initialised := False;
  FreeMem(pfile_in_zip_read_info);

  s^.pfile_in_zip_read := nil;

  unzCloseCurrentFile := err;
end;


{ Get the global comment string of the ZipFile, in the szComment buffer.
  uSizeBuf is the size of the szComment buffer.
  return the number of byte copied or an error code <0 }

function unzGetGlobalComment(afile: unzFile; szComment: PChar; uSizeBuf: longint): longint; { ZEXPORT }

var
  s: unz_s_ptr;
  uReadThis: longint;
begin
  if (afile = nil) then
  begin
    unzGetGlobalComment := UNZ_PARAMERROR;
    exit;
  end;
  s := unz_s_ptr(afile);

  uReadThis := uSizeBuf;
  if (uReadThis > s^.gi.size_comment) then
    uReadThis := s^.gi.size_comment;

  if (fseek(s^.afile, s^.central_pos + 22, SEEK_SET) <> 0) then
  begin
    unzGetGlobalComment := UNZ_ERRNO;
    exit;
  end;

  if (uReadThis > 0) then
  begin
    szComment^ := #0;
    if fread(szComment, uReadThis, 1, s^.afile) <> 1 then
    begin
      unzGetGlobalComment := UNZ_ERRNO;
      exit;
    end;
  end;

  if ((szComment <> nil) and (uSizeBuf > s^.gi.size_comment)) then
    (szComment +s^.gi.size_comment)^ := #0;

  unzGetGlobalComment := longint(uReadThis);
end;

end.
