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

  TX3DNodeClassesList = class(TList)
  strict private
    function GetItems(Index: Integer): TX3DNodeClass;
    procedure SetItems(Index: Integer; Value: TX3DNodeClass);
  public
    property Items[Index: Integer]: TX3DNodeClass
      read GetItems write SetItems; default;
    procedure AssignArray(
      const AItemsArray: array of TX3DNodeClass);
    function IndexOf(NodeClass: TX3DNodeClass): Integer; overload;
    { Equivalent to IndexOf(NodeClass.ClassType),
      taking care of necessary typecasts. }
    function IndexOf(Node: TX3DNode): Integer; overload;

    { Looks for a node class that is ancestor of given Node,
      in other words that satisfies the @code(Node is Items[Result]) condition.
      Contrast this with IndexOf method, which looks only for an exact
      class match.

      Returns -1 if not found. }
    function IndexOfAnyAncestor(Node: TX3DNode): Integer;

    procedure Add(Value: TX3DNodeClass);
  end;

{$endif read_interface}

{$ifdef read_implementation}

{ TX3DNodeClassesList ------------------------------------------------------- }

function TX3DNodeClassesList.GetItems(Index: Integer): TX3DNodeClass;
begin
  Result := TX3DNodeClass(inherited Items[Index]);
end;

procedure TX3DNodeClassesList.SetItems(Index: Integer; Value: TX3DNodeClass);
begin
  inherited Items[Index] := Pointer(Value);
end;

procedure TX3DNodeClassesList.AssignArray(
  const AItemsArray: array of TX3DNodeClass);
var
  I: Integer;
begin
  Count := High(AItemsArray) + 1;
  for I := 0 to High(AItemsArray) do
    Items[I] := AItemsArray[I];
end;

function TX3DNodeClassesList.IndexOf(NodeClass: TX3DNodeClass): Integer;
begin
  for Result := 0 to Count - 1 do
    if Items[Result] = NodeClass then
      Exit;
  Result := -1;
end;

function TX3DNodeClassesList.IndexOf(Node: TX3DNode): Integer;
begin
  Result := IndexOf(TX3DNodeClass(Node.ClassType));
end;

function TX3DNodeClassesList.IndexOfAnyAncestor(Node: TX3DNode): Integer;
begin
  for Result := 0 to Count - 1 do
    if Node is TX3DNodeClass(Items[Result]) then
      Exit;
  Result := -1;
end;

procedure TX3DNodeClassesList.Add(Value: TX3DNodeClass);
begin
  inherited Add(Pointer(Value));
end;

{$endif read_implementation}
