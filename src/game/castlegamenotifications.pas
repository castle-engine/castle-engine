{
  Copyright 2006-2017 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Global instance for @link(Notifications).
  It also captures CastleScript calls of @code(writeln()), to display
  them as notifications. }
unit CastleGameNotifications;

{$I castleconf.inc}

interface

uses CastleNotifications;

var
  Notifications: TCastleNotifications;

implementation

uses SysUtils, CastleScript;

var
  PreviousOnScriptMessage: TCasScriptMessage;

initialization
  Notifications := TCastleNotifications.Create(nil);

  { replace OnScriptMessage to allow using Notifications from CastleScript }
  PreviousOnScriptMessage := OnScriptMessage;
  OnScriptMessage := @Notifications.Show;
finalization
  { restore original OnScriptMessage }
  if Notifications <> nil then
  begin
    if OnScriptMessage = @Notifications.Show then
      OnScriptMessage := PreviousOnScriptMessage;
  end;

  FreeAndNil(Notifications);
end.
