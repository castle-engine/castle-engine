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
  to use our CastleGzioInternal unit. It is also cut down to TGZFileStream,
  the only class useful for reading/writing gz files. }
unit CastleZStream;

interface

uses
  Sysutils, Classes, CastleGzioInternal;

type
  EZlibError = class(EStreamError);

  TGZFileStream = Class(TOwnerStream)
  private
    FWriteMode : boolean;
    FFIle : gzfile;
  public
    Constructor Create(const Stream: TStream; const AWriteMode: boolean);
    Destructor Destroy;override;
    Function Read(Var Buffer; Count : longint): longint;override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;
  end;

implementation

Const
  SCouldntOpenFile = 'Couldn''t open file : %s';
  SReadOnlyStream = 'Decompression streams are read-only';
  SWriteOnlyStream = 'Compression streams are write-only';
  SSeekError = 'Compression stream seek error';
  SInvalidSeek = 'Invalid Compression seek operation';

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

end.
