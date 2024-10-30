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

{ Opening files and URLs. }
unit CastleOpenDocument;

{$include castleconf.inc}

interface

resourcestring
  SCannotOpenUrl = 'Browser not found on your system.';

{ Open URL with the suitable application.

  This detects and handles also local files (as filenames, or URLs with "file:"
  protocol).

  On Android and iOS, it uses the OS functions to open the URL,
  supporting all URL types that are handled by the installed applications.
  For example, it will support the market:// URLs on Android.

  To use this on Android, declare your Android project type as "integrated",
  see https://castle-engine.io/android-Project-Services-Integrated-with-Castle-Game-Engine . }
function OpenUrl(AUrl: String): Boolean;

{ Open a local file or directory.
  @deprecated You should instead use OpenUrl,
  that automatically detects local filenames and URLs leading to local filenames. }
function OpenDocument(APath: String): Boolean;

{ Share a text/link through user-choosen application.

  This works only on Android and iOS right now.
  For Android, you need to declare the project type as "integrated":
  See https://castle-engine.io/android-Project-Services-Integrated-with-Castle-Game-Engine .

  @param(Title The short title of the share.)
  @param(Subject Used as an email subject, and any other app on Android
    that interprets EXTRA_SUBJECT parameter.)
  @param(Content Multi-line share text content, possibly with URL inside.)
}
procedure ShareText(const Title, Subject, Content: string);

{ Show the application in the application store (Google Play on Android,
  AppStore on iOS). Ignored on other platforms now.

  @unorderedList(
    @itemSpacing Compact
    @item(On Android, ApplicationId should be the qualitied name of the application
      (same thing you use as qualified_name in CastleEngineManifest.xml).

      To include the necessary integration code in your Android project,
      you must declare your Android project type as "integrated".
      See https://castle-engine.io/android-Project-Services-Integrated-with-Castle-Game-Engine .
    )
    @item(On iOS, ApplicationId has to be the "Apple ID" number of your application
      (you can see it e.g. in https://itunesconnect.apple.com/ page of your application).
    )
  ) }
procedure OpenApplicationStore(const ApplicationId: string);

{ Vibrate the device.

  Available on Android, iOS and Nintendo Switch now. Ignored on other platforms.

  To include the necessary integration code in your Android project,
  declare your Android project type as "integrated" with
  the "vibrate" service inside CastleEngineManifest.xml.
  See https://castle-engine.io/android-Project-Services-Integrated-with-Castle-Game-Engine . }
procedure Vibrate(const Miliseconds: Cardinal);

{ Simple on-screen notification using Android "toast" call.

  This is available only on Android right now, ignored elsewhere.
  To include the necessary integration code in your Android project,
  you must declare your Android project type as "integrated".
  See https://castle-engine.io/android-Project-Services-Integrated-with-Castle-Game-Engine . }
procedure OnScreenNotification(const Message: string);
  deprecated 'This is Android-specific and probably will not be ever supported on other platforms. Better use CGE UI to make cros-platform UI notifications, like TCastleNotifications or just TCastleLabel with animated color/background.';

implementation

uses
  SysUtils, Classes,
  {$define read_uses}
  {$I castleopendocument_open.inc}
  {$undef read_uses}
  CastleUriUtils, CastleUtils, CastleFilesUtils, CastleLog, CastleMessaging;

{ Has URL any anchor at the end, like "index.html#chapter1".
  For such URLs, converting them to local filename may be possible
  but is lossy: anchor would be lost.

  Testcase: API docs to
  file:///..../doc/reference/CastleSoundEngine.TCastleSound.html#DefaultReferenceDistance
  would effectively open
  /..../doc/reference/CastleSoundEngine.TCastleSound.html
  (anchor lost). }
function UrlHasAnchor(const Url: String): Boolean;
var
  U, Anchor: String;
begin
  U := Url;
  URIExtractAnchor(U, Anchor);
  Result := Anchor <> '';
end;

{$define read_implementation}
{$I castleopendocument_open.inc}
{$undef read_implementation}

procedure ShareText(const Title, Subject, Content: string);
begin
  Messaging.Send(['share-text', Title, Subject, Content]);
end;

procedure OpenApplicationStore(const ApplicationId: string);
begin
  {$ifdef ANDROID} OpenUrl('market://details?id=' + ApplicationId); {$endif}

  {$ifdef CASTLE_IOS} Messaging.Send(['open-application-store', ApplicationId]); {$endif}
end;

procedure Vibrate(const Miliseconds: Cardinal);
begin
  Messaging.Send(['vibrate', IntToStr(Miliseconds)]);
end;

procedure OnScreenNotification(const Message: string);
begin
  Messaging.Send(['on-screen-notification', Message]);
end;

end.
