{
  Copyright 2014-2017 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ JSON helpers. }

{$ifdef read_interface}
{$endif}

{$ifdef read_implementation}
procedure ReadVector2SingleList(const Json: TJSONObject; const Name: string;
  const List: TVector2SingleList);
var
  JsonArray: TJSONArray;
  I: Integer;
  Vec2: TVector2Single;
  Vec2Index: Integer;
begin
  JsonArray := Json.Find(Name, jtArray) as TJSONArray;
  if JsonArray <> nil then
  begin
    Vec2Index := 0;
    for I := 0 to JsonArray.Count - 1 do
      if JsonArray[I] is TJSONNumber then
      begin
        Vec2[Vec2Index] := JsonArray[I].AsFloat;
        Inc(Vec2Index);
        if Vec2Index = 2 then
        begin
          List.Add(Vec2);
          Vec2Index := 0;
        end;
      end;
    if Vec2Index <> 0 then
      WritelnWarning('Spine', 'Vector2 list (like uvs or vertices) ends in the middle of the vector');
  end;
end;

procedure ReadLongIntList(const Json: TJSONObject; const Name: string;
  const List: TLongIntList);
var
  JsonArray: TJSONArray;
  I: Integer;
begin
  JsonArray := Json.Find(Name, jtArray) as TJSONArray;
  if JsonArray <> nil then
    for I := 0 to JsonArray.Count - 1 do
    begin
      { On FPC 2.6.2, we have here TJSONInt64Number.
        On FPC 3.0.0, we have here TJSONIntegerNumber.
        (Did not test this on FPC 2.6.4 -- just do "Writeln(JsonArray[I].ClassName);" and open any Spine JSON with mesh.)
        So don't check "if JsonArray[I] is TJSONIntegerNumber then" or such,
        better just use [I].AsInteger (or Integers[I]).

        (Testcase: when this goes wrong, Spine mesh (e.g. hero wings in escape_universe) doesn't show,
        because IndexedTriangleSet remains unset.) }
      List.Add(JsonArray[I].AsInteger);
    end;
end;
{$endif}
