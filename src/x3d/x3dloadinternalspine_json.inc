{
  Copyright 2014-2014 Michalis Kamburelis.

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
      OnWarning(wtMajor, 'Spine', 'Vector2 list (like uvs or vertices) ends in the middle of the vector');
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
      if JsonArray[I] is TJSONIntegerNumber then
        List.Add(JsonArray[I].AsInteger);
end;
{$endif}
