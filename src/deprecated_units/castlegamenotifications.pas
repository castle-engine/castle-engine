{
  Copyright 2006-2022 Michalis Kamburelis.

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
unit CastleGameNotifications
  deprecated 'create own instance of TCastleNotifications instead of relying in singleton in this unit';

{$I castleconf.inc}

interface

uses CastleNotifications;

var
  Notifications: TCastleNotifications;

implementation

uses SysUtils, CastleScript;

var
  PreviousOnScriptMessage: TCasScriptMessage;
  {$ifndef FPC}TestOnScriptMessage:TCasScriptMessage;{$endif}

initialization
  Notifications := TCastleNotifications.Create(nil);

  { replace OnScriptMessage to allow using Notifications from CastleScript }
  PreviousOnScriptMessage := OnScriptMessage;
  OnScriptMessage := {$ifdef FPC}@{$endif}Notifications.Show;
finalization

  { restore original OnScriptMessage }
  if Notifications <> nil then
  begin
    {$ifndef FPC}TestOnScriptMessage := Notifications.Show;{$endif}
    if {$ifndef FPC}@{$endif}OnScriptMessage = {$ifdef FPC}@Notifications.Show{$else}@TestOnScriptMessage{$endif}then
      OnScriptMessage := PreviousOnScriptMessage;
  end;

  FreeAndNil(Notifications);
end.
