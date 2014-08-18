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

{ Spine bones. }

{$ifdef read_interface}
  TBone = class
    Name: string;
    Length: Single;
    XY: TVector2Single;
    Scale: TVector2Single;
    Rotation: Single;
    Parent: TBone;
    Node: TTransformNode;
    NodeUsedAsChild: boolean;
    destructor Destroy; override;
    procedure Parse(const Json: TJSONObject;
      const PossibleParents: TBoneList; const ExpectedParent: boolean);
    procedure BuildNodes(const BaseUrl: string);
  end;

  TBoneList = class(specialize TFPGObjectList<TBone>)
    { Find bone by name.
      @raises ESpineReadError If bone does not exist. }
    function Find(const BoneName: string): TBone;
    procedure Parse(const Json: TJSONObject; var Root: TBone);
    procedure BuildNodes(const BaseUrl: string);
  end;
{$endif}

{$ifdef read_implementation}

{ TBone ---------------------------------------------------------------------- }

procedure TBone.Parse(const Json: TJSONObject;
  const PossibleParents: TBoneList; const ExpectedParent: boolean);
var
  ParentName: string;
begin
  Name := Json.Get('name', '');
  Length := Json.Get('length', 0.0);
  XY[0] := Json.Get('x', 0.0);
  XY[1] := Json.Get('y', 0.0);
  Scale[0] := Json.Get('scaleX', 1.0);
  Scale[1] := Json.Get('scaleY', 1.0);
  Rotation := Json.Get('rotation', 0.0);

  ParentName := Json.Get('parent', '');
  if ParentName <> '' then
    Parent := PossibleParents.Find(ParentName);

  if ExpectedParent then
  begin
    if Parent = nil then
      raise ESpineReadError.CreateFmt('Parent for bone "%s" expected, but not specified', [Name]);
  end else
  begin
    if Parent <> nil then
      raise ESpineReadError.CreateFmt('Parent for bone "%s" not expected, but specified', [Name]);
  end;
end;

destructor TBone.Destroy;
begin
  if NodeUsedAsChild then
    Node := nil else
    FreeIfUnusedAndNil(Node);
  inherited;
end;

procedure TBone.BuildNodes(const BaseUrl: string);
begin
  Node := TTransformNode.Create('Bone_' + ToX3DName(Name), BaseUrl);
  Node.FdTranslation.Value := Vector3Single(XY[0], XY[1], 0);
  Node.FdScale.Value := Vector3Single(Scale[0], Scale[1], 1);
  Node.FdRotation.Value := Vector4Single(0, 0, 1, DegToRad(Rotation));
  if Parent <> nil then
  begin
    NodeUsedAsChild := true;
    Parent.Node.FdChildren.Add(Node);
  end;
end;

{ TBoneList ------------------------------------------------------------------ }

function TBoneList.Find(const BoneName: string): TBone;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if Items[I].Name = BoneName then
      Exit(Items[I]);
  raise ESpineReadError.CreateFmt('Bone name "%s" not found', [BoneName]);
end;

procedure TBoneList.Parse(const Json: TJSONObject; var Root: TBone);
var
  I: Integer;
  Bone: TBone;
  ChildArray: TJSONArray;
begin
  ChildArray := Json.Find('bones', jtArray) as TJSONArray;
  if ChildArray = nil then
    raise ESpineReadError.Create('Spine JSON skeleton: Missing "bones" array');

  for I := 0 to ChildArray.Count - 1 do
    if ChildArray[I] is TJSONObject then
    begin
      Bone := TBone.Create;
      Add(Bone);
      if Root = nil then
      begin
        { reading Root bone. It should have no parent. }
        Bone.Parse(TJSONObject(ChildArray[I]), Self, false);
        Root := Bone;
      end else
        { reading child bone. It must have a parent. }
        Bone.Parse(TJSONObject(ChildArray[I]), Self, true);
    end;
end;

procedure TBoneList.BuildNodes(const BaseUrl: string);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].BuildNodes(BaseUrl);
end;
{$endif}
