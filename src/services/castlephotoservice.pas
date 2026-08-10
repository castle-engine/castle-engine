{
  Copyright 2017-2024 Michalis Kamburelis and Jan Adamec.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Operating system Photo library integration (TPhotoService). }
unit CastlePhotoService;

{$I castleconf.inc}

interface

uses Classes;

type
  { TPhotoService enables to store/retrieve images to/from the system Photos
    app. It may also request user to take a new photo with the camera.

    At the moment, only storing images is implemented. }
  TPhotoService = class(TComponent)
  public
    class procedure StoreImage(const ImageUrl: string);
  end;

implementation

uses CastleMessaging, CastleUriUtils, CastleLog;

class procedure TPhotoService.StoreImage(const ImageUrl: string);
var
  ResolvedUrl: String;
  {$if defined(ANDROID) or defined(CASTLE_IOS)}
  ResolvedFileName: String;
  {$endif}
begin
  {$if defined(ANDROID) or defined(CASTLE_IOS)}
  { Android ( ServicePhotoService.java ) and iOS ( PhotoService.m )
    services handle URLs, but not Castle Game Engine URLs with protocols
    like "castle-config", only URLs with standard protocols like "file".

    So resolve them to a filename and back, knowing that 'castle-config:/'
    is always just resolved to regular files on Android and iOS.
    This means doing funny UriToFilenameSafe + FilenameToUriSafe.

    Note: When ApplicationConfigOverride was set, we just cannot
    guarantee that TPhotoService works in the current approach.

    Note: Alternative URL conversion would use ResolveCastleConfigUrl,
    though then we need to list all possible CGE-specific protocols,
    which can be application specific (if user registers TCastleZip
    or TCastleMemoryFileSystem).
    See https://castle-engine.io/url .
  }
  ResolvedFileName := UriToFilenameSafe(ImageUrl);
  if ResolvedFileName = '' then
  begin
    WritelnWarning('TPhotoService.StoreImage: Unable to resolve ImageUrl "%s" to a filename, which we need for Android and iOS.', [ImageUrl]);
    Exit;
  end;
  ResolvedUrl := FilenameToUriSafe(ResolvedFileName);
  {$else}
  ResolvedUrl := ImageUrl;
  {$endif}

  Messaging.Send([
    'photoservice-store-image',
    ResolvedUrl,
    UriMimeType(ResolvedUrl)
  ]);
end;

end.
