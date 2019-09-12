{
    $Id: zstream.pp,v 1.6 2005/02/14 17:13:15 peter Exp $
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by the Free Pascal development team

    Implementation of compression streams.

    This file is adapted from the FPC RTL source code, as such
    the license and copyright information of FPC RTL applies here.
    That said, the license of FPC RTL happens to be *exactly*
    the same as used by the "Castle Game Engine": LGPL (version 2.1)
    with "static linking exception" (with exactly the same wording
    of the "static linking exception").
    See the file COPYING.txt, included in this distribution, for details about
    the copyright of "Castle Game Engine".
    See http://www.freepascal.org/faq.var#general-license about the copyright
    of FPC RTL.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{ Streams using zlib compression.

  This is adjusted from FPC sources unit fcl/inc/zstream.pp,
  - to use our CastleInternalGzio unit.
  - it is cut down to TGZFileStream, the only class useful for reading/writing gz files.
  - works on Delphi too. }
unit CastleInternalZStream;

{$I castleconf.inc}

interface

uses
  Sysutils, Classes
  {$ifdef FPC} , CastleInternalGzio {$else}, Zlib {$endif};

type
  EZlibError = class(EStreamError);

  TGZFileStream =
  {$ifdef FPC}
  class(TOwnerStream)
  private
    FWriteMode : boolean;
    FFIle : gzfile;
  public
    Constructor Create(const Stream: TStream; const AWriteMode: boolean);
    Destructor Destroy;override;
    Function Read(Var Buffer; Count : longint): longint;override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;
  {$else}
  class(TDecompressionStream)
    constructor Create(const Stream: TStream; const AWriteMode: boolean);
  {$endif}
  end;

implementation

{$ifdef FPC}

Const
  SReadOnlyStream = 'Decompression streams are read-only';
  SWriteOnlyStream = 'Compression streams are write-only';
  SSeekError = 'Compression stream seek error';

// TGZFileStream

Constructor TGZFileStream.Create(const Stream: TStream; const AWriteMode: boolean);
begin
  inherited Create(Stream);
  SourceOwner := true;
  FWriteMode := AWriteMode;
  FFile:=gzopen (Stream, AWriteMode);
end;

Destructor TGZFileStream.Destroy;
begin
  If FFile <> nil then
    gzclose(FFile);
  Inherited Destroy;
end;

Function TGZFileStream.Read(Var Buffer; Count : longint): longint;
begin
  If FWriteMode then
    Raise ezliberror.create(SWriteOnlyStream);
  Result:=gzRead(FFile,@Buffer,Count);
  if Result < 0 then
    raise EZlibError.Create('Gzip decompression error: ' + gzerror(FFile));
end;

function TGZFileStream.Write(const Buffer; Count: Longint): Longint;
begin
  If not FWriteMode then
    Raise EzlibError.Create(SReadonlyStream);
  Result:=gzWrite(FFile,@Buffer,Count);
  if Result < 0 then
    raise EZlibError.Create('Gzip compression error: ' + gzerror(FFile));
end;

function TGZFileStream.Seek(Offset: Longint; Origin: Word): Longint;
begin
  Result:=gzseek(FFile,Offset,Origin);
  If Result=-1 then
    Raise eZlibError.Create(SSeekError);
end;

{$else}

constructor TGZFileStream.Create(const Stream: TStream; const AWriteMode: boolean);
begin
  if AWriteMode then
    raise EZlibError.Create('Writing gzip data on Delphi not implemented yet');

  inherited Create(Stream,
    { Simple way to read gzip-compressed data with Delphi,
      see https://stackoverflow.com/questions/31221429/delphi-xe-8-how-to-decompress-a-gzip-file .
      31 bit wide window means that we require GZIP header (and not just Zlib header).
    }
    15 + 16,
    { OwnsStream = true, to behave just like TGZFileStream that descends
      from TOwnerStream. }
    true);
end;

{$endif}

end.
