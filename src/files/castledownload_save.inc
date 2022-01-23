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
  { Options for the @link(URLSaveStream) function. }
  TSaveStreamOption = (
    { Filter the contents through gzip compression. }
    ssoGzip
  );
  TSaveStreamOptions = set of TSaveStreamOption;

{ Create a stream to save a given URL, for example create a TFileStream
  to save a file for a @code(file) URL. In other words, perform @italic(upload).

  Right now, this only works for @code(file) and @code(castle-data) URLs,
  and (as all engine functions) can also accept simple filenames.

  When saving to @code(castle-data) URL, remember that on some platforms
  the game data is read-only. Use this only during development on desktop,
  when you know that "data" is just your regular data directory.

  On Android, you should use the "write_external_storage"
  service to be able to write storage files (e.g. to SD card).
  This means files accessed by the 'file' protocol.
  See https://castle-engine.io/android-Services .

  @raises(ESaveError In case of problems saving this URL.)

  @raises(Exception Various TStream instances (used internally by this
    function) may raise exceptions in case the stream cannot be created
    for saving.
    Right now, we simply let these exceptions to "pass through" from this function
    (instead of catching and re-raising).
    So be ready that this function may raise @italic(any) Exception class.)
}
function URLSaveStream(const URL: string; const Options: TSaveStreamOptions = []): TStream;

{$endif}

{$ifdef read_implementation}

function URLSaveStream(const URL: string; const Options: TSaveStreamOptions): TStream;
var
  P: String;
  RegisteredProtocol: TRegisteredProtocol;
  UnderlyingStream: TStream;
begin
  P := URIProtocol(URL);

  { Handle early castle-data:, protocol to access application data,
    https://castle-engine.io/manual_data_directory.php }
  if P = 'castle-data' then
  begin
    Result := URLSaveStream(ResolveCastleDataURL(URL), Options);
    Exit;
  end;

  RegisteredProtocol := RegisteredProtocols.Find(P);
  if RegisteredProtocol <> nil then
  begin
    if Assigned(RegisteredProtocol.WriteEvent) then
    begin
      UnderlyingStream := RegisteredProtocol.WriteEvent(URL);
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
