{%MainUnit castledownload.pas}
{
  Copyright 2013-2020 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_interface}

{ Open a proper stream to read a file, fast (with buffering) and with seeking.
  This gives you a stream most comfortable for reading (buffering means
  that you can read small, comfortable pieces of it; seeking means
  you can jump freely to various file positions, back and forward).

  On different OSes or even compilers this may require a little different
  stream, so it's safest to just use this function. For example,
  traditional Classes.TFileStream doesn't do buffering. Although under Linux,
  the buffering of file handles is done at kernel level (so everything
  works fast), on Windows the slowdown is noticeable.
  This function will always create
  proper stream descendant, eventually wrapping some standard stream
  in a buffered stream with full seeking capability.

  @deprecated Instead of this, use @link(Download) with [soForceMemoryStream]. }
function CreateReadFileStream(const Url: String): TStream; deprecated 'use Download(Url, [soForceMemoryStream])';

{ Save the contents of given Stream to an Url. }
procedure StreamSaveToFile(Stream: TStream; const Url: String);

{$endif}

{$ifdef read_implementation}

function CreateReadFileStream(const Url: String): TStream;
begin
  Result := Download(Url, [soForceMemoryStream]);
end;

procedure StreamSaveToFile(Stream: TStream; const Url: String);
const
  BufSize = 100000;
var
  S : TStream;
  Buffer: Pointer;
  ReadCount: Integer;
begin
  { optimized implementation for TMemoryStream }
  if Stream is TMemoryStream then
  begin
    TMemoryStream(Stream).SaveToFile(UriToFilenameSafe(Url));
    Exit;
  end;

  Buffer := GetMem(BufSize);
  try
    S := UrlSaveStream(Url);
    try
      repeat
        ReadCount := Stream.Read(Buffer^, BufSize);
        if ReadCount = 0 then
          Break else
          S.WriteBuffer(Buffer^, ReadCount);
      until false;
    finally
      S.free;
    end;
  finally FreeMem(Buffer) end;
end;

{$endif}
