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

  { Clipping plane, along with a transformation. }
  TClipPlane = record
    Node: TClipPlaneNode;
    Transform: TMatrix4;
  end;
  PClipPlane = ^TClipPlane;

  TClipPlaneList = class({$ifdef FPC}specialize{$endif} TStructList<TClipPlane>)
  public
    { Find record with given TClipPlaneNode, returns -1 if not found. }
    function IndexOfNode(Node: TClipPlaneNode): Integer;
    function Equals(SecondValue: TObject): boolean; {$ifdef TOBJECT_HAS_EQUALS} override; {$endif}
  end;

{$endif read_interface}

{$ifdef read_implementation}

{ TClipPlaneList --------------------------------------------------------- }

function TClipPlaneList.IndexOfNode(Node: TClipPlaneNode): Integer;
begin
  for Result := 0 to Count - 1 do
    if L[Result].Node = Node then
      Exit;
  Result := -1;
end;

function TClipPlaneList.Equals(SecondValue: TObject): boolean;
var
  I: Integer;
begin
  Result :=
    (SecondValue <> nil) and
    (SecondValue is TClipPlaneList) and
    (TClipPlaneList(SecondValue).Count = Count);

  if Result then
    for I := 0 to Count - 1 do
      if (L[I].Node <> TClipPlaneList(SecondValue).L[I].Node) or
         TMatrix4.PerfectlyEquals(
           L[I].Transform,
           TClipPlaneList(SecondValue).L[I].Transform) then
        Exit(false);
end;

{$endif read_implementation}
