{%MainUnit castleutils.pas}
{
  Copyright 2002-2025 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ TMethodList }

{$ifdef read_interface}

type
  { List of methods (callbacks "of object").
    This is just TList class from Generics.Collections,
    with some useful helpers.

    In practice, for now, this just fixes the list for
    CASTLE_LIST_METHODS_WORKAROUND (avoids FPC 3.2.3 issues on 32-bit Arm). }
  {$ifdef FPC}generic{$endif}
  TMethodList<T> = class({$ifdef FPC}specialize{$endif} TList<T>)
  public
    {$ifdef CASTLE_LIST_METHODS_WORKAROUND}
    function IndexOf(const Item: T): SizeInt;
    procedure Remove(const Item: T);
    {$endif}
  end;

{$endif}

{$ifdef read_implementation}

{ TMethodList ---------------------------------------------------------------- }

{$ifdef CASTLE_LIST_METHODS_WORKAROUND}

function TMethodList{$ifndef FPC}<T>{$endif}.IndexOf(const Item: T): SizeInt;
var
  I: SizeInt;
begin
  for I := 0 to Count - 1 do
    if SameMethods(TMethod(Item), TMethod(Items[I])) then
      Exit(I);
  Result := -1;
end;

procedure TMethodList{$ifndef FPC}<T>{$endif}.Remove(const Item: T);
var
  I: SizeInt;
begin
  I := IndexOf(Item);
  if I <> -1 then
    Delete(I);
end;

{$endif}

{$endif read_implementation}
