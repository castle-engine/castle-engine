{
  Copyright 2017-2025 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Initial Facebook SDK integration (TFacebook). }
unit CastleFacebook;

{$I castleconf.inc}

interface

uses Classes;

type
  { Facebook SDK integration.
    Right now, the main purpose of this integration (that works with both
    Android and iOS), is to log application start to Facebook Analytics.

    Some more logging features are available, some only on Android and
    some only on iOS, right now. Details below. Depending on your interest,
    we may extend this service to support much more features --
    @url(https://castle-engine.io/talk.php tell us about it).

    @unorderedList(
      @item(@url(https://github.com/castle-engine/castle-engine/blob/master/tools/build-tool/data/android/services/facebook/README.adoc
        Android))
      @item(@url(https://github.com/castle-engine/castle-engine/blob/master/tools/build-tool/data/ios/services/facebook/README.adoc
        iOS))
    )
  }
  TFacebook = class(TComponent)
  public
    { Login to Facebook from this application.
      At this point, it does not serve much purpose except checking that
      the integration actually works (e.g. you pointed to the correct Facebook
      app).
      Only available on iOS now. }
    class procedure LoginButton;

    { Log achieved level to Facebook Analytics.
      Only available on Android now. }
    class procedure LogAchievedLevel(const Level: String);
  end;

implementation

uses CastleMessaging;

class procedure TFacebook.LoginButton;
begin
  Messaging.Send(['facebook-login-button']);
end;

class procedure TFacebook.LogAchievedLevel(const Level: String);
begin
  Messaging.Send(['facebook-log-achieved-level', Level]);
end;

end.
