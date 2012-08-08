{
  Copyright 2006-2012 Michalis Kamburelis.

  This file is part of "castle".

  "castle" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "castle" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "castle"; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

  ----------------------------------------------------------------------------
}

{ Global instance for @link(Notifications).
  It also captures CastleScript calls of @code(writeln()), to display
  them as notifications. }
unit CastleGameNotifications;

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
