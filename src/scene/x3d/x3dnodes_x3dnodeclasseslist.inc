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

  { List of TX3DNodeClass. }
  TX3DNodeClassesList = class({$ifdef FPC}specialize{$endif} TList<TX3DNodeClass>)
  public
    { Clear this list and make contents equal to given array. }
    procedure AssignArray(const AItemsArray: array of TX3DNodeClass);

    { Clear this list and make contents equal to given Source list. }
    procedure Assign(const Source: TX3DNodeClassesList);

    { Looks for a node class that is ancestor of given Node,
      in other words that satisfies the @code(Node is Items[Result]) condition.
      Contrast this with IndexOf method, which looks only for an exact
      class match.

      Returns -1 if not found. }
    function IndexOfAnyAncestor(const Node: TX3DNode): Integer;
  end;

{$endif read_interface}

{$ifdef read_implementation}

{ TX3DNodeClassesList ------------------------------------------------------- }

procedure TX3DNodeClassesList.AssignArray(
  const AItemsArray: array of TX3DNodeClass);
var
  I: Integer;
begin
  Count := High(AItemsArray) + 1;
  for I := 0 to High(AItemsArray) do
    Items[I] := AItemsArray[I];
end;

procedure TX3DNodeClassesList.Assign(const Source: TX3DNodeClassesList);
var
  I: Integer;
begin
  Count := Source.Count;
  { Assign without Move, to cause notifications.
    Though we don't promise the list notifications at this point. }
  // Move(Source.List^, List^, Count * SizeOf(Pointer));
  for I := 0 to Source.Count - 1 do
    Items[I] := Source[I];
end;

function TX3DNodeClassesList.IndexOfAnyAncestor(const Node: TX3DNode): Integer;
begin
  for Result := 0 to Count - 1 do
    if Node is Items[Result] then
      Exit;
  Result := -1;
end;

{$endif read_implementation}
