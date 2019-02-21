{
  Copyright 2017-2018 Michalis Kamburelis.

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
    On Android ( https://github.com/castle-engine/castle-engine/wiki/Android-Project-Services-Integrated-with-Castle-Game-Engine#facebook )
    and on iOS ( https://github.com/castle-engine/castle-engine/wiki/iOS-Services#facebook ).
  }
  TFacebook = class(TComponent)
  public
    class procedure LoginButton;
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
