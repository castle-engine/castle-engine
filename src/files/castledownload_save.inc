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

type
  { Options for the @link(UrlSaveStream) function. }
  TSaveStreamOption = (
    { Filter the contents through gzip compression. }
    ssoGzip
  );
  TSaveStreamOptions = set of TSaveStreamOption;

{ Create a stream to save (write to) a given URL.

  For example, to save a file.
  When you use URLSaveStream with URL with a @code(file) protocol,
  or just with a regular filename, it will return a TFileStream instance
  to write this file.

  This function supports any CGE custom protocol registered by @link(RegisterUrlProtocol),
  if only you provided @code(WriteEvent: TUrlWriteEvent) when registering that
  protocol.

  Note: When saving to @code(castle-data) URL, remember that on some platforms
  (like Android) or installation methods (like system-wide installation
  on Linux or Windows) the game data is read-only. To be portable,
  you should never use URLSaveStream with the @code(castle-data) protocol.
  It is only useful in limited cases when you know that you distribute your
  game in such way (and users install it in such way) and on such platforms,
  that the "data" directory is writable.

  On Android, you should use the "write_external_storage"
  service to be able to write storage files (e.g. to SD card).
  This means files accessed by the 'file' protocol.
  See https://castle-engine.io/android-Services .

  Example usage:

  @longCode(#
  var
    Stream: TStream;
    I: Uint32;
  begin
    Stream := URLSaveStream('file:///tmp/foo.txt', []);
    try
      // write some strings
      WritelnStr(Stream, 'Some string');
      WritelnStr(Stream, 'Another string');
      // write some binary data
      I := 666;
      Stream.WriteBuffer(I, SizeOf(I));
    finally FreeAndNil(Stream) end;
  end;
  #)

  @raises(ESaveError In case of problems saving this URL.)

  @raises(Exception Various TStream instances (used internally by this
    function) may raise exceptions in case the stream cannot be created
    for saving.
    Right now, we simply let these exceptions to "pass through" from this function
    (instead of catching and re-raising).
    So be ready that this function may raise @italic(any) Exception class.)
}
function UrlSaveStream(const Url: String; const Options: TSaveStreamOptions = []): TStream;

{$endif}

{$ifdef read_implementation}

function UrlSaveStream(const Url: String; const Options: TSaveStreamOptions): TStream;
var
  P: String;
  RegisteredProtocol: TRegisteredProtocol;
  UnderlyingStream: TStream;
begin
  P := URIProtocol(Url);

  { Handle early castle-data:, protocol to access application data,
    https://castle-engine.io/data }
  if P = 'castle-data' then
  begin
    Result := UrlSaveStream(ResolveCastleDataUrl(Url), Options);
    Exit;
  end;

  RegisteredProtocol := RegisteredProtocols.Find(P);
  if RegisteredProtocol <> nil then
  begin
    if Assigned(RegisteredProtocol.WriteEvent) then
    begin
      UnderlyingStream := RegisteredProtocol.WriteEvent(Url);
      // Compress using gzip, if requested in Options
      if ssoGzip in Options then
        Result := TGZFileStream.Create(UnderlyingStream, true)
      else
        Result := UnderlyingStream;
    end else
      raise EDownloadError.CreateFmt('Cannot write URLs with protocol "%s"', [P]);
  end else
    raise ESaveError.CreateFmt('Saving of URL with protocol "%s" not possible', [P]);
end;

{$endif}
