{
  Copyright 2017-2018 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_interface}

type
  TX3DChildrenList = class({$ifdef FPC}specialize{$endif} TObjectList<TAbstractChildNode>)
    { Find node by name, @nil if not found. }
    function FindName(const Name: string): TAbstractChildNode;
  end;

procedure X3DChildrenList_FreeUnusedAndNil(var List: TX3DChildrenList); forward;

{$endif read_interface}

{$ifdef read_implementation}

function TX3DChildrenList.FindName(const Name: string): TAbstractChildNode;
var
  I: Integer;
begin
  if Name = '' then
    Exit(nil);

  for I := 0 to Count - 1 do
  begin
    Result := Items[I];
    if Result.X3DName = Name then
      Exit;
  end;
  Result := nil;
end;

procedure X3DChildrenList_FreeUnusedAndNil(var List: TX3DChildrenList);
var
  I: Integer;
begin
  if List <> nil then
  begin
    for I := 0 to List.Count - 1 do
      if List.Items[I].VRML1ParentsCount + List.Items[I].ParentFieldsCount = 0 then
        List.Items[I].Free;
    FreeAndNil(List);
  end;
end;

{$endif read_implementation}
