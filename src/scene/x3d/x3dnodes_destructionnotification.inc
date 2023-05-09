{%MainUnit x3dnodes.pas}
{
  Copyright 2002-2018 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_interface}

  TNodeDestructionNotification = procedure (const Node: TX3DNode) of object;

  TNodeDestructionNotificationList = class({$ifdef FPC}specialize{$endif} TList<TNodeDestructionNotification>)
  public
    { Call all functions. }
    procedure ExecuteAll(const Node: TX3DNode);
  end;

{$endif read_interface}

{$ifdef read_implementation}

{ TNodeDestructionNotificationList ------------------------------------------- }

procedure TNodeDestructionNotificationList.ExecuteAll(const Node: TX3DNode);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I](Node);
end;

{$endif read_implementation}
