{
  Copyright 2021-2021 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Tenjin integration (TCastleTenjin). }
unit CastleTenjin;

{$I castleconf.inc}

interface

uses Classes;

type
  { Tenjin ( https://www.tenjin.com/ ) integration.
    On Android ( https://github.com/castle-engine/castle-engine/blob/master/tools/build-tool/data/android/integrated-services/tenjin/README.md )
    and on iOS ( https://github.com/castle-engine/castle-engine/blob/master/tools/build-tool/data/ios/services/tenjin/README.md ).

    For now the only point of this class is a class method @link(SendEvent).
  }
  TCastleTenjin = class
  public
    { Send a custom event name to Tenjin. }
    class procedure SendEvent(const Name: String); static;
  end;

implementation

uses CastleMessaging;

class procedure TCastleTenjin.SendEvent(const Name: String);
begin
  Messaging.Send(['tenjin-send-event', Name]);
end;

end.
