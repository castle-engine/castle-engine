unit Zip;

{ zip.c -- IO on .zip files using zlib
  zip.h -- IO for compress .zip files using zlib
   Version 0.15 alpha, Mar 19th, 1998,

   Copyright (C) 1998 Gilles Vollant

   This package allows to create .ZIP file, compatible with PKZip 2.04g
     WinZip, InfoZip tools and compatible.
   Encryption and multi volume ZipFile (span) are not supported.
   Old compressions used by old PKZip 1.x are not supported

  For decompression of .zip files, look at unzip.pas

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
  ZIP_OK    = (0);
  ZIP_ERRNO = (Z_ERRNO);
  ZIP_PARAMERROR = (-102);
  ZIP_INTERNALERROR = (-104);
  Z_DEFAULT_COMPRESSION = -(1);
  Z_DEFLATED = 8;

(*
{ tm_zip contain date/time info }
type
  tm_zip = record
     tm_sec : integer;            { seconds after the minute - [0,59] }
     tm_min : integer;            { minutes after the hour - [0,59] }
     tm_hour : integer;           { hours since midnight - [0,23] }
     tm_mday : integer;           { day of the month - [1,31] }
     tm_mon : integer;            { months since January - [0,11] }
     tm_year : integer;           { years - [1980..2044] }
  end;
*)
type
  zip_fileinfo = record
    tmz_date: tm_zip;        { date in understandable format           }
    dosDate:  longword;         { if dos_date = 0, tmu_date is used       }
    {   flag : longint;       }{ general purpose bit flag        2 bytes }

    internal_fa: longint;    { internal file attributes        2 bytes }
    external_fa: longint;    { external file attributes        4 bytes }
  end;
  zip_fileinfo_ptr = ^zip_fileinfo;

function zipOpen(const pathname: PChar; append: longint): zipFile; {ZEXPORT}
{ Create a zipfile.
  pathname contain on Windows NT a filename like "c:\\zlib\\zlib111.zip" or on
  an Unix computer "zlib/zlib111.zip".
  if the file pathname exist and append=1, the zip will be created at the end
  of the file. (useful if the file contain a self extractor code)
  If the zipfile cannot be opened, the return value is NIL.
  Else, the return value is a zipFile Handle, usable with other function
  of this zip package. }

function zipOpenNewFileInZip(afile: zipFile;
  {const} filename: PChar; const zipfi: zip_fileinfo_ptr; const extrafield_local: pointer; size_extrafield_local: integer; const extrafield_global: pointer; size_extrafield_global: integer; const comment: PChar; method: longint; level: longint): longint; {ZEXPORT}
{ Open a file in the ZIP for writing.
  filename : the filename in zip (if NIL, '-' without quote will be used
  zipfi^ contain supplemental information
  if extrafield_local<>NIL and size_extrafield_local>0, extrafield_local
    contains the extrafield data the the local header
  if extrafield_global<>NIL and size_extrafield_global>0, extrafield_global
    contains the extrafield data the the local header
  if comment <> NIL, comment contain the comment string
  method contain the compression method (0 for store, Z_DEFLATED for deflate)
  level contain the level of compression (can be Z_DEFAULT_COMPRESSION) }

function zipWriteInFileInZip(afile: zipFile; const buf: pointer; len: cardinal): longint; {ZEXPORT}
{ Write data in the zipfile }

function zipCloseFileInZip(afile: zipFile): longint; {ZEXPORT}
 { Close the current file in the zipfile }

function zipClose(afile: zipFile; const global_comment: PChar): longint; {ZEXPORT}
 { Close the zipfile }

implementation

uses
  {$ifdef Delphi}
  SysUtils,
  {$else}
  strings,
  {$endif}
  zDeflate, crc;

const
  VERSIONMADEBY = ($0); { platform depedent }

const
  zip_copyright: PChar = ' zip 0.15 Copyright 1998 Gilles Vollant ';


const
  SIZEDATA_INDATABLOCK = (4096 - (4 * 4));

  LOCALHEADERMAGIC = $04034b50;
  {CENTRALHEADERMAGIC = $02014b50;}
  ENDHEADERMAGIC   = $06054b50;

  FLAG_LOCALHEADER_OFFSET = $06;
  CRC_LOCALHEADER_OFFSET  = $0e;

  SIZECENTRALHEADER = $2e; { 46 }

type
  linkedlist_datablock_internal_ptr = ^linkedlist_datablock_internal;

  linkedlist_datablock_internal = record
    next_datablock: linkedlist_datablock_internal_ptr;
    avail_in_this_block: longint;
    filled_in_this_block: longint;
    unused: longint; { for future use and alignement }
    Data:   array[0..SIZEDATA_INDATABLOCK - 1] of byte;
  end;

type
  linkedlist_data = record
    first_block: linkedlist_datablock_internal_ptr;
    last_block:  linkedlist_datablock_internal_ptr;
  end;
  linkedlist_data_ptr = ^linkedlist_data;

type
  curfile_info = record
    stream: z_stream;            { zLib stream structure for inflate }
    stream_initialised: boolean; { TRUE is stream is initialised }
    pos_in_buffered_data: integer;  { last written byte in buffered_data }

    pos_local_header: longint;     { offset of the local header of the file
                                    currenty writing }
    central_header: PChar;       { central header data for the current file }
    size_centralheader: longint;   { size of the central header for cur file }
    flag: longint;                 { flag of the file currently writing }

    method:  longint;                { compression method of file currenty wr.}
    buffered_data: array[0..Z_BUFSIZE - 1] of byte;{ buffer contain compressed data to be written}
    dosDate: longint;
    crc32:   longint;
  end;

type
  zip_internal = record
    filezip: FILEptr;
    central_dir: linkedlist_data;  { datablock with central dir in construction}
    in_opened_file_inzip: boolean; { TRUE if a file in the zip is currently writ.}
    ci: curfile_info;              { info on the file curretly writing }

    begin_pos:    longint;            { position of the beginning of the zipfile }
    number_entry: longint;
  end;
  zip_internal_ptr = ^zip_internal;

function allocate_new_datablock: linkedlist_datablock_internal_ptr;
var
  ldi: linkedlist_datablock_internal_ptr;
begin
  ldi := linkedlist_datablock_internal_ptr(GetMem(sizeof(linkedlist_datablock_internal)));
  if (ldi <> nil) then
  begin
    ldi^.next_datablock      := nil;
    ldi^.filled_in_this_block := 0;
    ldi^.avail_in_this_block := SIZEDATA_INDATABLOCK;
  end;
  allocate_new_datablock := ldi;
end;

procedure free_datablock(var ldi: linkedlist_datablock_internal_ptr);
var
  ldinext: linkedlist_datablock_internal_ptr;
begin
  while (ldi <> nil) do
  begin
    ldinext := ldi^.next_datablock;
    FreeMem(ldi);
    ldi := ldinext;
  end;
end;

procedure init_linkedlist(var ll: linkedlist_data);
begin
  ll.last_block  := nil;
  ll.first_block := nil;
end;

procedure free_linkedlist(var ll: linkedlist_data);
begin
  free_datablock(ll.first_block);
  ll.last_block  := nil;
  ll.first_block := nil;
end;

function add_data_in_datablock(ll: linkedlist_data_ptr; const buf: pointer; len: longint): longint;
var
  ldi: linkedlist_datablock_internal_ptr;
  from_copy: {const} Pbyte;
var
  copy_this: integer;
  i:   integer;
  to_copy: Pbyte;
begin
  if (ll = nil) then
  begin
    add_data_in_datablock := ZIP_INTERNALERROR;
    exit;
  end;

  if (ll^.last_block = nil) then
  begin
    ll^.last_block  := allocate_new_datablock;
    ll^.first_block := ll^.last_block;
    if (ll^.first_block = nil) then
    begin
      add_data_in_datablock := ZIP_INTERNALERROR;
      exit;
    end;
  end;

  ldi := ll^.last_block;
  from_copy := Pbyte(buf);

  while (len > 0) do
  begin
    if (ldi^.avail_in_this_block = 0) then
    begin
      ldi^.next_datablock := allocate_new_datablock;
      if (ldi^.next_datablock = nil) then
      begin
        add_data_in_datablock := ZIP_INTERNALERROR;
        exit;
      end;
      ldi := ldi^.next_datablock;
      ll^.last_block := ldi;
    end;

    if (ldi^.avail_in_this_block < len) then
      copy_this := integer(ldi^.avail_in_this_block)
    else
      copy_this := integer(len);

    to_copy := @(ldi^.Data[ldi^.filled_in_this_block]);

    for i := 0 to copy_this - 1 do
      Pbytearray(to_copy)^[i] := Pbytearray(from_copy)^[i];

    Inc(ldi^.filled_in_this_block, copy_this);
    Dec(ldi^.avail_in_this_block, copy_this);
    Inc(from_copy, copy_this);
    Dec(len, copy_this);
  end;
  add_data_in_datablock := ZIP_OK;
end;


function write_datablock(fout: FILEptr; ll: linkedlist_data_ptr): longint;
var
  ldi: linkedlist_datablock_internal_ptr;
begin
  ldi := ll^.first_block;
  while (ldi <> nil) do
  begin
    if (ldi^.filled_in_this_block > 0) then
      if (fwrite(@ldi^.Data, integer(ldi^.filled_in_this_block), 1, fout) <> 1) then
      begin
        write_datablock := ZIP_ERRNO;
        exit;
      end;
    ldi := ldi^.next_datablock;
  end;
  write_datablock := ZIP_OK;
end;

{**************************************************************************}

{ ===========================================================================
   Outputs a long in LSB order to the given file
   nbByte = 1, 2 or 4 (byte, short or long)  }

function ziplocal_putValue(afile: FILEptr; x: longint; nbByte: longint): longint;
var
  buf: array[0..4 - 1] of byte;
  n:   longint;
begin
  for n := 0 to nbByte - 1 do
  begin
    buf[n] := byte(x and $ff);
    x      := x shr 8;
  end;
  if (fwrite(@buf, nbByte, 1, afile) <> 1) then
    ziplocal_putValue := ZIP_ERRNO
  else
    ziplocal_putValue := ZIP_OK;
end;

procedure ziplocal_putValue_inmemory(dest: pointer; x: longint; nbByte: longint);
var
  buf: Pbytearray;
  n:   longint;
begin
  buf := Pbytearray(dest);
  for n := 0 to nbByte - 1 do
  begin
    buf^[n] := Byte(x and $ff);
    x := x shr 8;
  end;
end;

{**************************************************************************}


function ziplocal_TmzDateToDosDate(var ptm: tm_zip; dosDate: longint): longint;
var
  year: longint;
begin
  year := longint(ptm.tm_year);
  if (year > 1980) then
    Dec(year, 1980)
  else
  if (year > 80) then
    Dec(year, 80);
  ziplocal_TmzDateToDosDate := longint(
    ((ptm.tm_mday) + (32 * (ptm.tm_mon + 1)) + (512 * year)) shl 16) or
    ((ptm.tm_sec div 2) + (32 * ptm.tm_min) + (2048 * longint(ptm.tm_hour)));
end;


{**************************************************************************}

function zipOpen(const pathname: PChar; append: longint): zipFile; {ZEXPORT}
var
  ziinit: zip_internal;
  zi:     zip_internal_ptr;
begin
  if (append = 0) then
    ziinit.filezip := fopen(pathname, fopenwrite)
  else
    ziinit.filezip := fopen(pathname, fappendwrite);

  if (ziinit.filezip = nil) then
  begin
    zipOpen := nil;
    exit;
  end;
  ziinit.begin_pos    := ftell(ziinit.filezip);
  ziinit.in_opened_file_inzip := False;
  ziinit.ci.stream_initialised := False;
  ziinit.number_entry := 0;
  init_linkedlist(ziinit.central_dir);

  zi := zip_internal_ptr(AllocMem(sizeof(zip_internal)));
  if (zi = nil) then
  begin
    fclose(ziinit.filezip);
    zipOpen := nil;
    exit;
  end;

  zi^     := ziinit;
  zipOpen := zipFile(zi);
end;

function zipOpenNewFileInZip(afile: zipFile;
  {const} filename: PChar; const zipfi: zip_fileinfo_ptr; const extrafield_local: pointer; size_extrafield_local: integer; const extrafield_global: pointer; size_extrafield_global: integer; const comment: PChar; method: longint; level: longint): longint; {ZEXPORT}
var
  zi:  zip_internal_ptr;
  size_filename: integer;
  size_comment: integer;
  i:   integer;
  err: longint;
begin
  err := ZIP_OK;
  if (afile = nil) then
  begin
    zipOpenNewFileInZip := ZIP_PARAMERROR;
    exit;
  end;
  if ((method <> 0) and (method <> Z_DEFLATED)) then
  begin
    zipOpenNewFileInZip := ZIP_PARAMERROR;
    exit;
  end;

  zi := zip_internal_ptr(afile);

  if (zi^.in_opened_file_inzip = True) then
  begin
    err := zipCloseFileInZip(afile);
    if (err <> ZIP_OK) then
    begin
      zipOpenNewFileInZip := err;
      exit;
    end;
  end;

  if (filename = nil) then
    filename := '-';

  if (comment = nil) then
    size_comment := 0
  else
    size_comment := strlen(comment);

  size_filename := strlen(filename);

  if (zipfi = nil) then
    zi^.ci.dosDate := 0
  else
  if (zipfi^.dosDate <> 0) then
    zi^.ci.dosDate := zipfi^.dosDate
  else
    zi^.ci.dosDate := ziplocal_TmzDateToDosDate(zipfi^.tmz_date, zipfi^.dosDate);
  zi^.ci.flag := 0;
  if ((level = 8) or (level = 9)) then
    zi^.ci.flag := zi^.ci.flag or 2;
  if ((level = 2)) then
    zi^.ci.flag := zi^.ci.flag or 4;
  if ((level = 1)) then
    zi^.ci.flag := zi^.ci.flag or 6;

  zi^.ci.crc32  := 0;
  zi^.ci.method := method;
  zi^.ci.stream_initialised := False;
  zi^.ci.pos_in_buffered_data := 0;
  zi^.ci.pos_local_header := ftell(zi^.filezip);
  zi^.ci.size_centralheader := SIZECENTRALHEADER + size_filename +
    size_extrafield_global + size_comment;
  zi^.ci.central_header := PChar(AllocMem(integer(zi^.ci.size_centralheader)));

  ziplocal_putValue_inmemory(zi^.ci.central_header, longint(CENTRALHEADERMAGIC), 4);
  { version info }
  ziplocal_putValue_inmemory(zi^.ci.central_header + 4, longint(VERSIONMADEBY), 2);
  ziplocal_putValue_inmemory(zi^.ci.central_header + 6, longint(20), 2);
  ziplocal_putValue_inmemory(zi^.ci.central_header + 8, longint(zi^.ci.flag), 2);
  ziplocal_putValue_inmemory(zi^.ci.central_header + 10, longint(zi^.ci.method), 2);
  ziplocal_putValue_inmemory(zi^.ci.central_header + 12, longint(zi^.ci.dosDate), 4);
  ziplocal_putValue_inmemory(zi^.ci.central_header + 16, longint(0), 4); {crc}
  ziplocal_putValue_inmemory(zi^.ci.central_header + 20, longint(0), 4); {compr size}
  ziplocal_putValue_inmemory(zi^.ci.central_header + 24, longint(0), 4); {uncompr size}
  ziplocal_putValue_inmemory(zi^.ci.central_header + 28, longint(size_filename), 2);
  ziplocal_putValue_inmemory(zi^.ci.central_header + 30, longint(size_extrafield_global), 2);
  ziplocal_putValue_inmemory(zi^.ci.central_header + 32, longint(size_comment), 2);
  ziplocal_putValue_inmemory(zi^.ci.central_header + 34, longint(0), 2); {disk nm start}

  if (zipfi = nil) then
    ziplocal_putValue_inmemory(zi^.ci.central_header + 36, longint(0), 2)
  else
    ziplocal_putValue_inmemory(zi^.ci.central_header + 36, longint(zipfi^.internal_fa), 2);

  if (zipfi = nil) then
    ziplocal_putValue_inmemory(zi^.ci.central_header + 38, longint(0), 4)
  else
    ziplocal_putValue_inmemory(zi^.ci.central_header + 38, longint(zipfi^.external_fa), 4);

  ziplocal_putValue_inmemory(zi^.ci.central_header + 42, longint(zi^.ci.pos_local_header), 4);

  i := 0;
  while (i < size_filename) do
  begin
    (zi^.ci.central_header +SIZECENTRALHEADER + i)^ := (filename + i)^;
    Inc(i);
  end;

  i := 0;
  while (i < size_extrafield_global) do
  begin
    (zi^.ci.central_header +SIZECENTRALHEADER + size_filename + i)^ :=
      ({const} PChar(extrafield_global) + i)^;
    Inc(i);
  end;

  i := 0;
  while (i < size_comment) do
  begin
    (zi^.ci.central_header +SIZECENTRALHEADER + size_filename + size_extrafield_global + i)^ := (filename + i)^;
    Inc(i);
  end;
  if (zi^.ci.central_header = nil) then
  begin
    zipOpenNewFileInZip := ZIP_INTERNALERROR;
    exit;
  end;

  { write the local header }
  err := ziplocal_putValue(zi^.filezip, longint(LOCALHEADERMAGIC), 4);

  if (err = ZIP_OK) then
    err := ziplocal_putValue(zi^.filezip, longint(20), 2); { version needed to extract }
  if (err = ZIP_OK) then
    err := ziplocal_putValue(zi^.filezip, longint(zi^.ci.flag), 2);

  if (err = ZIP_OK) then
    err := ziplocal_putValue(zi^.filezip, longint(zi^.ci.method), 2);

  if (err = ZIP_OK) then
    err := ziplocal_putValue(zi^.filezip, longint(zi^.ci.dosDate), 4);

  if (err = ZIP_OK) then
    err := ziplocal_putValue(zi^.filezip, longint(0), 4); { crc 32, unknown }
  if (err = ZIP_OK) then
    err := ziplocal_putValue(zi^.filezip, longint(0), 4); { compressed size, unknown }
  if (err = ZIP_OK) then
    err := ziplocal_putValue(zi^.filezip, longint(0), 4); { uncompressed size, unknown }

  if (err = ZIP_OK) then
    err := ziplocal_putValue(zi^.filezip, longint(size_filename), 2);

  if (err = ZIP_OK) then
    err := ziplocal_putValue(zi^.filezip, longint(size_extrafield_local), 2);

  if ((err = ZIP_OK) and (size_filename > 0)) then
    if (fwrite(filename, integer(size_filename), 1, zi^.filezip) <> 1) then
      err := ZIP_ERRNO;

  if ((err = ZIP_OK) and (size_extrafield_local > 0)) then
    if (fwrite(extrafield_local, integer(size_extrafield_local), 1, zi^.filezip) <> 1) then
      err := ZIP_ERRNO;

  zi^.ci.stream.avail_in  := integer(0);
  zi^.ci.stream.avail_out := integer(Z_BUFSIZE);
  zi^.ci.stream.next_out  := Pbyte(@zi^.ci.buffered_data);
  zi^.ci.stream.total_in  := 0;
  zi^.ci.stream.total_out := 0;

  if ((err = ZIP_OK) and (zi^.ci.method = Z_DEFLATED)) then
  begin
    err := deflateInit2(zi^.ci.stream, level,
      Z_DEFLATED, -MAX_WBITS, DEF_MEM_LEVEL, 0);

    if (err = Z_OK) then
      zi^.ci.stream_initialised := True;
  end;

  if (err = Z_OK) then
    zi^.in_opened_file_inzip := True;
  zipOpenNewFileInZip := err;
end;

function zipWriteInFileInZip(afile: zipFile; const buf: pointer; len: cardinal): longint; {ZEXPORT}
var
  zi:  zip_internal_ptr;
  err: longint;
var
  uTotalOutBefore: longint;
var
  copy_this, i: integer;
begin
  err := ZIP_OK;

  if (afile = nil) then
  begin
    zipWriteInFileInZip := ZIP_PARAMERROR;
    exit;
  end;
  zi := zip_internal_ptr(afile);

  if (zi^.in_opened_file_inzip = False) then
  begin
    zipWriteInFileInZip := ZIP_PARAMERROR;
    exit;
  end;

  zi^.ci.stream.next_in := buf;
  zi^.ci.stream.avail_in := len;
  zi^.ci.crc32 := crc32(zi^.ci.crc32, buf, len);

  while ((err = ZIP_OK) and (zi^.ci.stream.avail_in > 0)) do
  begin
    if (zi^.ci.stream.avail_out = 0) then
    begin
      if fwrite(@zi^.ci.buffered_data, integer(zi^.ci.pos_in_buffered_data), 1, zi^.filezip) <> 1 then
        err := ZIP_ERRNO;
      zi^.ci.pos_in_buffered_data := 0;
      zi^.ci.stream.avail_out := integer(Z_BUFSIZE);
      zi^.ci.stream.next_out  := Pbyte(@zi^.ci.buffered_data);
    end;

    if (zi^.ci.method = Z_DEFLATED) then
    begin
      uTotalOutBefore := zi^.ci.stream.total_out;
      err := deflate(zi^.ci.stream, Z_NO_FLUSH);
      Inc(zi^.ci.pos_in_buffered_data, integer(zi^.ci.stream.total_out - uTotalOutBefore));
    end
    else
    begin
      if (zi^.ci.stream.avail_in < zi^.ci.stream.avail_out) then
        copy_this := zi^.ci.stream.avail_in
      else
        copy_this := zi^.ci.stream.avail_out;

      for i := 0 to copy_this - 1 do
        (PChar(zi^.ci.stream.next_out) +i)^ :=
          ( {const} PChar(zi^.ci.stream.next_in) + i)^;


      Dec(zi^.ci.stream.avail_in, copy_this);
      Dec(zi^.ci.stream.avail_out, copy_this);
      Inc(zi^.ci.stream.next_in, copy_this);
      Inc(zi^.ci.stream.next_out, copy_this);
      Inc(zi^.ci.stream.total_in, copy_this);
      Inc(zi^.ci.stream.total_out, copy_this);
      Inc(zi^.ci.pos_in_buffered_data, copy_this);
    end;
  end;

  zipWriteInFileInZip := 0;
end;

function zipCloseFileInZip(afile: zipFile): longint; {ZEXPORT}
var
  zi:  zip_internal_ptr;
  err: longint;
var
  uTotalOutBefore: longint;
var
  cur_pos_inzip: longint;
begin
  err := ZIP_OK;

  if (afile = nil) then
  begin
    zipCloseFileInZip := ZIP_PARAMERROR;
    exit;
  end;
  zi := zip_internal_ptr(afile);

  if (zi^.in_opened_file_inzip = False) then
  begin
    zipCloseFileInZip := ZIP_PARAMERROR;
    exit;
  end;
  zi^.ci.stream.avail_in := 0;

  if (zi^.ci.method = Z_DEFLATED) then
    while (err = ZIP_OK) do
    begin
      if (zi^.ci.stream.avail_out = 0) then
      begin
        if fwrite(@zi^.ci.buffered_data, integer(zi^.ci.pos_in_buffered_data), 1, zi^.filezip) <> 1 then
          err := ZIP_ERRNO;
        zi^.ci.pos_in_buffered_data := 0;
        zi^.ci.stream.avail_out := integer(Z_BUFSIZE);
        zi^.ci.stream.next_out  := Pbyte(@zi^.ci.buffered_data);
      end;
      uTotalOutBefore := zi^.ci.stream.total_out;
      err := deflate(zi^.ci.stream, Z_FINISH);
      Inc(zi^.ci.pos_in_buffered_data, integer(zi^.ci.stream.total_out - uTotalOutBefore));
    end;

  if (err = Z_STREAM_END) then
    err := ZIP_OK; { this is normal }

  if (zi^.ci.pos_in_buffered_data > 0) and (err = ZIP_OK) then
    if fwrite(@zi^.ci.buffered_data, integer(zi^.ci.pos_in_buffered_data), 1, zi^.filezip) <> 1 then
      err := ZIP_ERRNO;

  if ((zi^.ci.method = Z_DEFLATED) and (err = ZIP_OK)) then
  begin
    err := deflateEnd(zi^.ci.stream);
    zi^.ci.stream_initialised := False;
  end;

  ziplocal_putValue_inmemory(zi^.ci.central_header + 16, longint(zi^.ci.crc32), 4); {crc}
  ziplocal_putValue_inmemory(zi^.ci.central_header + 20, longint(zi^.ci.stream.total_out), 4); {compr size}
  ziplocal_putValue_inmemory(zi^.ci.central_header + 24, longint(zi^.ci.stream.total_in), 4); {uncompr size}

  if (err = ZIP_OK) then
    err := add_data_in_datablock(@zi^.central_dir, zi^.ci.central_header, longint(zi^.ci.size_centralheader));

  FreeMem(zi^.ci.central_header);
  zi^.ci.central_header := nil;

  if (err = ZIP_OK) then
  begin
    cur_pos_inzip := ftell(zi^.filezip);
    if fseek(zi^.filezip, zi^.ci.pos_local_header + 14, SEEK_SET) <> 0 then
      err := ZIP_ERRNO;

    if (err = ZIP_OK) then
      err := ziplocal_putValue(zi^.filezip, longint(zi^.ci.crc32), 4); { crc 32, unknown }

    if (err = ZIP_OK) then { compressed size, unknown }
      err := ziplocal_putValue(zi^.filezip, longint(zi^.ci.stream.total_out), 4);

    if (err = ZIP_OK) then { uncompressed size, unknown }
      err := ziplocal_putValue(zi^.filezip, longint(zi^.ci.stream.total_in), 4);

    if fseek(zi^.filezip, cur_pos_inzip, SEEK_SET) <> 0 then
      err := ZIP_ERRNO;
  end;

  Inc(zi^.number_entry);
  zi^.in_opened_file_inzip := False;

  zipCloseFileInZip := err;
end;

function zipClose(afile: zipFile; const global_comment: PChar): longint; {ZEXPORT}
var
  zi:  zip_internal_ptr;
  err: longint;
  size_centraldir: longint;
  centraldir_pos_inzip: longint;
  size_global_comment: integer;
var
  ldi: linkedlist_datablock_internal_ptr;
begin
  err := 0;
  size_centraldir := 0;
  if (afile = nil) then
  begin
    zipClose := ZIP_PARAMERROR;
    exit;
  end;
  zi := zip_internal_ptr(afile);

  if (zi^.in_opened_file_inzip = True) then
    err := zipCloseFileInZip(afile);

  if (global_comment = nil) then
    size_global_comment := 0
  else
    size_global_comment := strlen(global_comment);

  centraldir_pos_inzip := ftell(zi^.filezip);
  if (err = ZIP_OK) then
  begin
    ldi := zi^.central_dir.first_block;
    while (ldi <> nil) do
    begin
      if ((err = ZIP_OK) and (ldi^.filled_in_this_block > 0)) then
        if fwrite(@ldi^.Data, integer(ldi^.filled_in_this_block), 1, zi^.filezip) <> 1 then
          err := ZIP_ERRNO;

      Inc(size_centraldir, ldi^.filled_in_this_block);
      ldi := ldi^.next_datablock;
    end;
  end;
  free_datablock(zi^.central_dir.first_block);

  if (err = ZIP_OK) then { Magic End }
    err := ziplocal_putValue(zi^.filezip, longint(ENDHEADERMAGIC), 4);

  if (err = ZIP_OK) then { number of this disk }
    err := ziplocal_putValue(zi^.filezip, longint(0), 2);

  if (err = ZIP_OK) then { number of the disk with the start of the central directory }
    err := ziplocal_putValue(zi^.filezip, longint(0), 2);

  if (err = ZIP_OK) then { total number of entries in the central dir on this disk }
    err := ziplocal_putValue(zi^.filezip, longint(zi^.number_entry), 2);

  if (err = ZIP_OK) then { total number of entries in the central dir }
    err := ziplocal_putValue(zi^.filezip, longint(zi^.number_entry), 2);

  if (err = ZIP_OK) then { size of the central directory }
    err := ziplocal_putValue(zi^.filezip, longint(size_centraldir), 4);

  if (err = ZIP_OK) then { offset of start of central directory with respect to the
                          starting disk number }
    err := ziplocal_putValue(zi^.filezip, longint(centraldir_pos_inzip), 4);

  if (err = ZIP_OK) then { zipfile comment length }
    err := ziplocal_putValue(zi^.filezip, longint(size_global_comment), 2);

  if ((err = ZIP_OK) and (size_global_comment > 0)) then
    if fwrite(global_comment, integer(size_global_comment), 1, zi^.filezip) <> 1 then
      err := ZIP_ERRNO;
  fclose(zi^.filezip);
  FreeMem(zi);

  zipClose := err;
end;

end.
