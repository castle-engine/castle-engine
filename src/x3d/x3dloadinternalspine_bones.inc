{
  Copyright 2014-2018 Michalis Kamburelis.

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
  strict private
    { World-space bone transformation, necessary for skinnedmesh calculations.
      WorldMatrix includes rotation and scale, from WorldRotation and WorldScale.
      @groupBegin }
    WorldXY: TVector2;
    WorldScale: TVector2;
    WorldRotation: Single;
    WorldMatrix, WorldMatrixInverse: TMatrix2;
    { @groupEnd }
    BackupXY: TVector2;
    BackupScale: TVector2;
    BackupRotation: Single;
    IsBackup: Boolean;
  public
    Name: String;
    Length: Single;
    XY: TVector2;
    Scale: TVector2;
    Rotation: Single;
    InheritScale, InheritRotation: Boolean;
    Parent: TBone;
    Node: TTransformNode;
    NodeUsedAsChild: Boolean;
    destructor Destroy; override;
    procedure Parse(const Json: TJSONObject;
      const PossibleParents: TBoneList; const ExpectedParent, DragonBonesFormat: Boolean);
    procedure BuildNodes(const BaseUrl: String);
    function ToWorldSpace(const V: TVector2): TVector2;
    function ToLocalSpace(V: TVector2): TVector2; overload;
    function ToLocalSpace(const V: TVector3): TVector3; overload;
    { Calculate world-space (well, at least in the space of whole JSON file)
      bone parameters.
      Similar to UpdateWorldTransform in spine-runtimes/spine-csharp/src/Bone.cs }
    procedure UpdateWorldTransform;
    { Backup bone's position, rotation and scale values }
    procedure Backup;
    { Restore bone's position, rotation and scale values }
    procedure Restore;
  end;

  TBoneList = class({$ifdef CASTLE_OBJFPC}specialize{$endif} TObjectList<TBone>)
    { Find bone by name.
      @raises ESpineReadError If bone does not exist. }
    function Find(const BoneName: String): TBone;
    { Find bone by name.
      @return nil if bone does not exist. }
    function FindByName(const BoneName: String): TBone;
    procedure Parse(const Json: TJSONObject; var Root: TBone;
      const DragonBonesFormat: Boolean);
    procedure BuildNodes(const BaseUrl: String);
    procedure Assign(const Source: TBoneList);
  end;
{$endif}

{$ifdef read_implementation}

{ TBone ---------------------------------------------------------------------- }

procedure TBone.Parse(const Json: TJSONObject;
  const PossibleParents: TBoneList; const ExpectedParent, DragonBonesFormat: Boolean);
var
  ParentName: String;
  Transform: TJSONObject;
begin
  Name := Json.Get('name', '');
  Length := Json.Get('length', 0.0);
  if DragonBonesFormat then
  begin
    Transform := Json.Find('transform', jtObject) as TJSONObject;
    if Transform = nil then
      raise ESpineReadError.CreateFmt('Missing "transform" of Dragon Bones bone "%s"', [Name]);
    XY[0] := Transform.Get('x', 0.0);
    XY[1] := Transform.Get('y', 0.0);
    Scale[0] := Transform.Get('scX', 1.0);
    Scale[1] := Transform.Get('scY', 1.0);
    Rotation := Json.Get('rotation', 0.0); // TODO: is this the correct name in Dragon Bones?
    InheritScale := Json.Get('inheritScale', true); // TODO: is this the correct name in Dragon Bones?
    InheritRotation := Json.Get('inheritRotation', true); // TODO: is this the correct name in Dragon Bones?
  end else
  begin
    XY[0] := Json.Get('x', 0.0);
    XY[1] := Json.Get('y', 0.0);
    Scale[0] := Json.Get('scaleX', 1.0);
    Scale[1] := Json.Get('scaleY', 1.0);
    Rotation := Json.Get('rotation', 0.0);
    InheritScale := Json.Get('inheritScale', true);
    InheritRotation := Json.Get('inheritRotation', true);
  end;

  // The support seems to be OK for Dragon Bones,
  // that uses this always, so don't warn by default anymore.
  if SpineVerboseWarnings and
     ((not InheritScale) or (not InheritRotation)) then
    WritelnWarning('Spine', 'bone inheritRotation=false or inheritScale=false is only partially supported');

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

  UpdateWorldTransform;
end;

destructor TBone.Destroy;
begin
  if NodeUsedAsChild then
    Node := nil else
    FreeIfUnusedAndNil(Node);
  inherited;
end;

procedure TBone.BuildNodes(const BaseUrl: String);
begin
  Node := TTransformNode.Create('Bone_' + Name, BaseUrl);
  Node.FdTranslation.Value := Vector3(XY[0], XY[1], 0);
  Node.FdScale.Value := Vector3(Scale[0], Scale[1], 1);
  Node.FdRotation.Value := Vector4(0, 0, 1, DegToRad(Rotation));
  if Parent <> nil then
  begin
    NodeUsedAsChild := true;
    Parent.Node.AddChildren(Node);
  end;
end;

procedure TBone.UpdateWorldTransform;
var
  C, S: Float;
begin
  WorldXY := XY;
  WorldScale := Scale;
  WorldRotation := Rotation;

  if Parent <> nil then
  begin
    WorldXY := Vector2(
      WorldXY[0] * Parent.WorldMatrix[0, 0] + WorldXY[1] * Parent.WorldMatrix[0, 1],
      WorldXY[0] * Parent.WorldMatrix[1, 0] + WorldXY[1] * Parent.WorldMatrix[1, 1]);
    WorldXY := WorldXY + Parent.WorldXY;
    if InheritScale then
      WorldScale := WorldScale * Parent.WorldScale;
    if InheritRotation then
      WorldRotation := WorldRotation + Parent.WorldRotation;
  end;

  SinCos(DegToRad(WorldRotation), S, C);
  WorldMatrix[0, 0] :=  C * WorldScale[0];
  WorldMatrix[0, 1] := -S * WorldScale[1];
  WorldMatrix[1, 0] :=  S * WorldScale[0];
  WorldMatrix[1, 1] :=  C * WorldScale[1];

  { inverse of 2D rotation matrix is it's transposition. Also, invert scale }
  WorldMatrixInverse[0, 0] :=  C / WorldScale[0];
  WorldMatrixInverse[1, 0] := -S / WorldScale[1];
  WorldMatrixInverse[0, 1] :=  S / WorldScale[0];
  WorldMatrixInverse[1, 1] :=  C / WorldScale[1];
end;

function TBone.ToWorldSpace(const V: TVector2): TVector2;
begin
  Result := Vector2(
    V[0] * WorldMatrix[0, 0] + V[1] * WorldMatrix[0, 1],
    V[0] * WorldMatrix[1, 0] + V[1] * WorldMatrix[1, 1]);
  Result := Result + WorldXY;
end;

function TBone.ToLocalSpace(V: TVector2): TVector2;
begin
  V := V - WorldXY;
  Result := Vector2(
    V[0] * WorldMatrixInverse[0, 0] + V[1] * WorldMatrixInverse[0, 1],
    V[0] * WorldMatrixInverse[1, 0] + V[1] * WorldMatrixInverse[1, 1]);
end;

function TBone.ToLocalSpace(const V: TVector3): TVector3;
var
  Result2: TVector2 absolute Result;
begin
  Result := V;
  Result2 := ToLocalSpace(Result2);
end;

procedure TBone.Backup;
begin
  if not IsBackup then
  begin
    BackupXY := XY;
    BackupRotation := Rotation;
    BackupScale := Scale;
    IsBackup := True;
  end;
end;

procedure TBone.Restore;
begin
  if IsBackup then
  begin
    XY := BackupXY;
    Rotation := BackupRotation;
    Scale := BackupScale;
    IsBackup := False;
  end;
end;

{ TBoneList ------------------------------------------------------------------ }

function TBoneList.Find(const BoneName: String): TBone;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if Items[I].Name = BoneName then
      Exit(Items[I]);
  raise ESpineReadError.CreateFmt('Bone name "%s" not found', [BoneName]);
end;

function TBoneList.FindByName(const BoneName: String): TBone;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if Items[I].Name = BoneName then
      Exit(Items[I]);
  Exit(nil);
end;

procedure TBoneList.Parse(const Json: TJSONObject; var Root: TBone; const DragonBonesFormat: Boolean);
var
  I: Integer;
  Bone: TBone;
  ChildArray: TJSONArray;
  ArrayName: String;
begin
  if DragonBonesFormat then
    ArrayName := 'bone'
  else
    ArrayName := 'bones';
  ChildArray := Json.Find(ArrayName, jtArray) as TJSONArray;
  if ChildArray = nil then
    raise ESpineReadError.CreateFmt('Spine / Dragon Bones JSON skeleton: Missing "%s" array', [ArrayName]);

  for I := 0 to ChildArray.Count - 1 do
    if ChildArray[I] is TJSONObject then
    begin
      Bone := TBone.Create;
      Add(Bone);
      if Root = nil then
      begin
        { reading Root bone. It should have no parent. }
        Bone.Parse(TJSONObject(ChildArray[I]), Self, false, DragonBonesFormat);
        Root := Bone;
      end else
        { reading child bone. It must have a parent. }
        Bone.Parse(TJSONObject(ChildArray[I]), Self, true, DragonBonesFormat);
    end;
end;

procedure TBoneList.BuildNodes(const BaseUrl: String);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].BuildNodes(BaseUrl);
end;

procedure TBoneList.Assign(const Source: TBoneList);
begin
  Clear;
  AddRange(Source);
end;

{$endif}
