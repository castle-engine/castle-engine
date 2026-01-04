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

  {$ifdef CASTLE_LIST_METHODS_WORKAROUND}
    { Note that this will make warnings that our IndexOf hides ancestor,
      and it seems we cannot avoid them. }
    {$warnings off}
  {$endif}

  TNodeDestructionNotificationList = class({$ifdef FPC}specialize{$endif} TList<TNodeDestructionNotification>)
  public
    { Call all functions. }
    procedure ExecuteAll(const Node: TX3DNode);

    {$ifdef CASTLE_LIST_METHODS_WORKAROUND}
    function IndexOf(const Item: TNodeDestructionNotification): SizeInt;
    procedure Remove(const Item: TNodeDestructionNotification);
    {$endif}
  end;

  {$ifdef CASTLE_LIST_METHODS_WORKAROUND}
  {$warnings on}
  {$endif}

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

{$ifdef CASTLE_LIST_METHODS_WORKAROUND}
function TNodeDestructionNotificationList.IndexOf(const Item: TNodeDestructionNotification): SizeInt;
var
  I: SizeInt;
begin
  for I := 0 to Count - 1 do
    if SameMethods(TMethod(Item), TMethod(Items[I])) then
      Exit(I);
  Result := -1;
end;

procedure TNodeDestructionNotificationList.Remove(const Item: TNodeDestructionNotification);
var
  I: SizeInt;
begin
  I := IndexOf(Item);
  if I <> -1 then
    Delete(I);
end;
{$endif}

{$endif read_implementation}
