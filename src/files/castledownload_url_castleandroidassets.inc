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

{$endif}

{$ifdef read_implementation}

{ Handle castle-android-assets URLs. }

{$ifdef ANDROID}

{ TUrlCastleAndroidAssets ------------------------------------------------------------------- }

type
  TUrlCastleAndroidAssets = class
    class function Read(const Url: String; out MimeType: string): TStream;
  end;

class function TUrlCastleAndroidAssets.Read(const Url: String; out MimeType: string): TStream;
begin
  CheckFileAccessSafe(Url);
  Result := TReadAssetStream.Create(URIToAssetPath(Url));
  MimeType := UriMimeType(Url);
end;

{$endif ANDROID}

{$endif}
