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
    procedure LoadFromURL(const URL: string); overload;
    procedure LoadFromURL(const URL: string; const AEncoding: TEncoding); overload;

    { Save the contents to URL.
      Uses standard SaveToStream combined with
      Castle Game Engine @link(URLSaveStream) to save the contents. }
    procedure SaveToURL(const URL: string);
  end;

{$endif}

{$ifdef read_implementation}

{ TStringsHelper ---------------------------------------------------------- }

procedure TStringsHelper.LoadFromURL(const URL: string);
var
  Stream: TStream;
begin
  Stream := Download(URL);
  try
    LoadFromStream(Stream);
  finally FreeAndNil(Stream) end;
end;

procedure TStringsHelper.LoadFromURL(const URL: string; const AEncoding: TEncoding);
var
  Stream: TStream;
begin
  Stream := Download(URL);
  try
    LoadFromStream(Stream {$ifndef VER3_0}, AEncoding {$endif});
  finally FreeAndNil(Stream) end;
end;

procedure TStringsHelper.SaveToURL(const URL: string);
var
  Stream: TStream;
begin
  Stream := URLSaveStream(URL);
  try
    SaveToStream(Stream);
  finally FreeAndNil(Stream) end;
end;

{$endif}
