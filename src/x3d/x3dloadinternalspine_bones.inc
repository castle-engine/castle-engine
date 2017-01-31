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

{ Spine bones. }

{$ifdef read_interface}
  TBonesAnimated = class
    Translation: TBoneList;
    Scale: TBoneList;
    Rotation: TBoneList;
    constructor Create;
    destructor Destroy; override;
    procedure Assign(const Source: TBonesAnimated);
  end;

  TBone = class
  strict private
    { World-space bone transformation, necessary for skinnedmesh calculations.
      WorldMatrix includes rotation and scale, from WorldRotation and WorldScale.
      @groupBegin }
    WorldXY: TVector2Single;
    WorldScale: TVector2Single;
    WorldRotation: Single;
    WorldMatrix, WorldMatrixInverse: TMatrix2Single;
    { @groupEnd }

    { Calculate world-space (well, at least in the space of whole JSON file)
      bone parameters.
      Similar to UpdateWorldTransform in spine-runtimes/spine-csharp/src/Bone.cs }
    procedure UpdateWorldTransform;
  public
    Name: string;
    Length: Single;
    XY: TVector2Single;
    Scale: TVector2Single;
    Rotation: Single;
    InheritScale, InheritRotation: boolean;
    Parent: TBone;
    Node: TTransformNode;
    ResetTranslation: TPositionInterpolatorNode;
    ResetScale: TPositionInterpolatorNode;
    ResetRotation: TCubicBezier2DOrientationInterpolatorNode;
    NodeUsedAsChild: boolean;
    destructor Destroy; override;
    procedure Parse(const Json: TJSONObject;
      const PossibleParents: TBoneList; const ExpectedParent: boolean);
    procedure BuildNodes(const BaseUrl: string;
      const BonesAnimated: TBonesAnimated);
    function ToWorldSpace(const V: TVector2Single): TVector2Single;
    function ToLocalSpace(V: TVector2Single): TVector2Single;
    function ToLocalSpace(const V: TVector3Single): TVector3Single;
  end;

  TBoneList = class(specialize TFPGObjectList<TBone>)
    { Find bone by name.
      @raises ESpineReadError If bone does not exist. }
    function Find(const BoneName: string): TBone;
    procedure Parse(const Json: TJSONObject; var Root: TBone);
    procedure BuildNodes(const BaseUrl: string;
      const BonesAnimated: TBonesAnimated);
  end;
{$endif}

{$ifdef read_implementation}

{ TBonesAnimated ------------------------------------------------------------- }

constructor TBonesAnimated.Create;
begin
  inherited;
  Translation := TBoneList.Create(false);
  Scale := TBoneList.Create(false);
  Rotation := TBoneList.Create(false);
end;

destructor TBonesAnimated.Destroy;
begin
  FreeAndNil(Translation);
  FreeAndNil(Scale);
  FreeAndNil(Rotation);
  inherited;
end;

procedure TBonesAnimated.Assign(const Source: TBonesAnimated);
begin
  Translation.Assign(Source.Translation);
  Scale.Assign(Source.Scale);
  Rotation.Assign(Source.Rotation);
end;

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
  InheritScale := Json.Get('inheritScale', true);
  InheritRotation := Json.Get('inheritRotation', true);

  if (not InheritScale) or (not InheritRotation) then
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

procedure TBone.BuildNodes(const BaseUrl: string;
  const BonesAnimated: TBonesAnimated);

  { Prepare interpolators that allow to reset bone to initial transformation
    after some animations. }

  procedure AddResetTranslation;
  var
    Route: TX3DRoute;
  begin
    if BonesAnimated.Translation.IndexOf(Self) = -1 then Exit;

    ResetTranslation := TPositionInterpolatorNode.Create('BoneReset_translate_' + ToX3DName(Name));
    ResetTranslation.FdKey.Items.Add(0);
    ResetTranslation.FdKeyValue.Items.Add(Node.FdTranslation.Value);
    Node.FdChildren.Add(ResetTranslation);

    Route := TX3DRoute.Create;
    Route.SetSourceDirectly(ResetTranslation.EventValue_changed);
    Route.SetDestinationDirectly(Node.FdTranslation.EventIn);
    Node.AddRoute(Route);
  end;

  procedure AddResetScale;
  var
    Route: TX3DRoute;
  begin
    if BonesAnimated.Scale.IndexOf(Self) = -1 then Exit;

    ResetScale := TPositionInterpolatorNode.Create('BoneReset_scale_' + ToX3DName(Name));
    ResetScale.FdKey.Items.Add(0);
    ResetScale.FdKeyValue.Items.Add(Node.FdScale.Value);
    Node.FdChildren.Add(ResetScale);

    Route := TX3DRoute.Create;
    Route.SetSourceDirectly(ResetScale.EventValue_changed);
    Route.SetDestinationDirectly(Node.FdScale.EventIn);
    Node.AddRoute(Route);
  end;

  procedure AddResetRotation;
  var
    Route: TX3DRoute;
  begin
    if BonesAnimated.Rotation.IndexOf(Self) = -1 then Exit;

    ResetRotation := TCubicBezier2DOrientationInterpolatorNode.Create('BoneReset_rotate_' + ToX3DName(Name));
    ResetRotation.FdKey.Items.Add(0);
    ResetRotation.FdKeyValue.Items.Add(Node.FdRotation.Value[3]);
    Node.FdChildren.Add(ResetRotation);

    Route := TX3DRoute.Create;
    Route.SetSourceDirectly(ResetRotation.EventValue_changed);
    Route.SetDestinationDirectly(Node.FdRotation.EventIn);
    Node.AddRoute(Route);
  end;

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

  AddResetTranslation;
  AddResetScale;
  AddResetRotation;
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
    WorldXY := Vector2Single(
      WorldXY[0] * Parent.WorldMatrix[0][0] + WorldXY[1] * Parent.WorldMatrix[0][1],
      WorldXY[0] * Parent.WorldMatrix[1][0] + WorldXY[1] * Parent.WorldMatrix[1][1]);
    WorldXY += Parent.WorldXY;
    if InheritScale then
      WorldScale := WorldScale * Parent.WorldScale;
    if InheritRotation then
      WorldRotation += Parent.WorldRotation;
  end;

  SinCos(DegToRad(WorldRotation), S, C);
  WorldMatrix[0][0] :=  C * WorldScale[0];
  WorldMatrix[0][1] := -S * WorldScale[1];
  WorldMatrix[1][0] :=  S * WorldScale[0];
  WorldMatrix[1][1] :=  C * WorldScale[1];

  { inverse of 2D rotation matrix is it's transposition. Also, invert scale }
  WorldMatrixInverse[0][0] :=  C / WorldScale[0];
  WorldMatrixInverse[1][0] := -S / WorldScale[1];
  WorldMatrixInverse[0][1] :=  S / WorldScale[0];
  WorldMatrixInverse[1][1] :=  C / WorldScale[1];
end;

function TBone.ToWorldSpace(const V: TVector2Single): TVector2Single;
begin
  Result := Vector2Single(
    V[0] * WorldMatrix[0][0] + V[1] * WorldMatrix[0][1],
    V[0] * WorldMatrix[1][0] + V[1] * WorldMatrix[1][1]);
  Result += WorldXY;
end;

function TBone.ToLocalSpace(V: TVector2Single): TVector2Single;
begin
  V -= WorldXY;
  Result := Vector2Single(
    V[0] * WorldMatrixInverse[0][0] + V[1] * WorldMatrixInverse[0][1],
    V[0] * WorldMatrixInverse[1][0] + V[1] * WorldMatrixInverse[1][1]);
end;

function TBone.ToLocalSpace(const V: TVector3Single): TVector3Single;
var
  Result2: TVector2Single absolute Result;
begin
  Result := V;
  Result2 := ToLocalSpace(Result2);
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

procedure TBoneList.BuildNodes(const BaseUrl: string;
  const BonesAnimated: TBonesAnimated);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].BuildNodes(BaseUrl, BonesAnimated);
end;
{$endif}
