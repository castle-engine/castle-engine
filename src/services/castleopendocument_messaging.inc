{%MainUnit castleopendocument.pas}
{
  Copyright 2012-2024 Michalis Kamburelis and Lazarus developers.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.
  Parts of this file are based on Lazarus LCL code, which has
  exactly the same license as our "Castle Game Engine":
  LGPL with static linking exception, see COPYING.txt for details.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ OpenUrl and OpenDocument implementation using CastleMessaging messages.
  This assumes the application is compiled with a service
  (available for Android or iOS, see
  https://castle-engine.io/android_services
  https://castle-engine.io/ios_services )
  that receives the message and acts on it. }

{$ifdef read_implementation}

function OpenUrl(AUrl: String): Boolean;
begin
  Messaging.Send(['view-url', AUrl]);
  Result := true;
end;

function OpenDocument(APath: String): Boolean;
begin
  Result := OpenUrl(FilenameToUriSafe(APath));
end;

{$endif read_implementation}
