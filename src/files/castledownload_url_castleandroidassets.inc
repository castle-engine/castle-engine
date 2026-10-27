{%MainUnit castledownload.pas}
{
  Copyright 2013-2020 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.md,
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
    class function Exists(const Url: String): TUriExists;
    class procedure FindFiles(const UrlPath, Mask: String;
      const FileEvent: TFoundFileMethod; var StopSearch: Boolean);
  end;

class function TUrlCastleAndroidAssets.Read(const Url: String; out MimeType: string): TStream;
begin
  CheckFileAccessSafe(Url);
  Result := TReadAssetStream.Create(UriToAssetPath(Url));
  MimeType := UriMimeType(Url);
end;

class function TUrlCastleAndroidAssets.Exists(const Url: String): TUriExists;
begin
  Result := AndroidAssetExists(UriToAssetPath(Url));
end;

class procedure TUrlCastleAndroidAssets.FindFiles(const UrlPath, Mask: String;
  const FileEvent: TFoundFileMethod; var StopSearch: Boolean);
begin
  FindFilesInAndroidAssets(UriToAssetPath(UrlPath), Mask, FileEvent, StopSearch);
end;

{$endif ANDROID}

{$endif}
