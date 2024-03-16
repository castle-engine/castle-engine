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
  TStringsHelper = class helper for TStrings
    { Load the contents from URL.
      Uses Castle Game Engine @link(Download) to get the contents,
      then uses standard LoadFromStream to load them.

      The meaning (and default behavior) of optional AEncoding
      is the same as for TStrings.LoadFromFile and TStrings.LoadFromStream. }
    procedure LoadFromUrl(const Url: String); overload;
    procedure LoadFromUrl(const Url: String; const AEncoding: TEncoding); overload;

    { Save the contents to URL.
      Uses standard SaveToStream combined with
      Castle Game Engine @link(UrlSaveStream) to save the contents. }
    procedure SaveToUrl(const Url: String);
  end;

{$endif}

{$ifdef read_implementation}

{ TStringsHelper ---------------------------------------------------------- }

procedure TStringsHelper.LoadFromUrl(const Url: String);
var
  Stream: TStream;
begin
  Stream := Download(Url);
  try
    LoadFromStream(Stream);
  finally FreeAndNil(Stream) end;
end;

procedure TStringsHelper.LoadFromUrl(const Url: String; const AEncoding: TEncoding);
var
  Stream: TStream;
begin
  Stream := Download(Url);
  try
    LoadFromStream(Stream {$ifndef VER3_0}, AEncoding {$endif});
  finally FreeAndNil(Stream) end;
end;

procedure TStringsHelper.SaveToUrl(const Url: String);
var
  Stream: TStream;
begin
  Stream := UrlSaveStream(Url);
  try
    SaveToStream(Stream);
  finally FreeAndNil(Stream) end;
end;

{$endif}
