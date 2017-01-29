{
  Copyright 2016-2017 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Helpshift (http://helpshift.com/) integration (THelpshift). }
unit CastleHelpshift;

{$I castleconf.inc}

interface

uses Classes;

type
  { Helpshift (http://helpshift.com/) integration.
    Only on Android (will simply do nothing on other platforms).
    For now, only one trivial method. }
  THelpshift = class(TComponent)
  public
    class procedure ShowConversation;
    class procedure ShowFAQs;
  end;

implementation

uses CastleMessaging;

class procedure THelpshift.ShowConversation;
begin
  Messaging.Send(['helpshift-show-conversation']);
end;

class procedure THelpshift.ShowFAQs;
begin
  Messaging.Send(['helpshift-show-faqs']);
end;

end.
