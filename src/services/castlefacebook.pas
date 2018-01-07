{
  Copyright 2017-2017 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Initial Facebook SDK (https://developers.facebook.com/docs/ios/) integration (TFacebook). }
unit CastleFacebook;

{$I castleconf.inc}

interface

uses Classes;

type
  { Facebook SDK (https://developers.facebook.com/docs/ios/) integration. Only on iOS now. }
  TFacebook = class(TComponent)
  public
    class procedure LoginButton;
  end;

implementation

uses CastleMessaging;

class procedure TFacebook.LoginButton;
begin
  Messaging.Send(['facebook-login-button']);
end;

end.
