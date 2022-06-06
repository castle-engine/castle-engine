{
  Copyright 2022-2022 Michalis Kamburelis.

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
  { Attaches a gizmo to visualize parent transform.
    Create this only if you need it (usually check "if CastleDesignMode ..."). }
  TInternalCastleEditorGizmo = class(TCastleTransform)
  strict private
    GizmoSelect: TCastleBox;
    GizmoImage: TCastleImageTransform;
    GizmoBillboard: TCastleBillboard;
    GizmoScene: TCastleScene; //< Note that it may remain nil, if LoadVisualization not called
    { Set various properties for internal TCastleTransform, shown only for debug. }
    procedure InternalTransform(const T: TCastleTransform);
  protected
    function LocalRayCollision(const RayOrigin, RayDirection: TVector3;
      const TrianglesToIgnoreFunc: TTriangleIgnoreFunc): TRayCollision; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure LocalRender(const Params: TRenderParams); override;

    { You have to call it once, right after creating. }
    procedure SetParent(const AOwner: TCastleTransform);
    { Show given node graph as the gizmo visualization.
      The given RootNode will become owned by this instance. }
    procedure LoadVisualization(const RootNode: TX3DRootNode);
    { Create a line shape, suitable for node graph in LoadVisualization.
      The version that returns TCoordinateNode lets you initialize (and later update)
      lines coords explicitly. }
    function LinesShape(const Points: array of TVector3): TShapeNode; overload;
    function LinesShape(out Coord: TCoordinateNode; const VertexCount: array of Integer): TShapeNode; overload;
    { Set the image to display as icon. }
    procedure SetIconUrl(const Url: String);
  end;

{$endif read_interface}

{$ifdef read_implementation}

{ TInternalCastleEditorGizmo ------------------------------------------------- }

constructor TInternalCastleEditorGizmo.Create(AOwner: TComponent);
begin
  inherited;
  SetTransient;
end;

procedure TInternalCastleEditorGizmo.InternalTransform(const T: TCastleTransform);
begin
  T.InternalExcludeFromParentBoundingVolume := true;
  T.Pickable := false;
  T.Collides := false;
  T.CastShadowVolumes := false;
  T.ExcludeFromStatistics := true;
end;

procedure TInternalCastleEditorGizmo.SetParent(const AOwner: TCastleTransform);
const
  GizmoSize = 1.0;
begin
  // This allows user to select parent (camera, light etc.) by clicking in CGE editor
  GizmoSelect := TCastleBox.Create(Self);
  GizmoSelect.Size := CastleVectors.Vector3(GizmoSize, GizmoSize, GizmoSize);
  InternalTransform(GizmoSelect);
  GizmoSelect.Visible := false;
  // to make picking natural, restore to default these values, after InternalTransform modified them
  GizmoSelect.Pickable := true;
  GizmoSelect.InternalExcludeFromParentBoundingVolume := false;
  Add(GizmoSelect);

  GizmoImage := TCastleImageTransform.Create(Self);
  GizmoImage.Size := Vector2(GizmoSize, GizmoSize);
  GizmoImage.Mipmaps := true;
  InternalTransform(GizmoImage);
  Add(GizmoImage);

  GizmoBillboard := TCastleBillboard.Create(Self);
  GizmoBillboard.AxisOfRotation := TVector3.Zero;
  GizmoImage.AddBehavior(GizmoBillboard);
end;

procedure TInternalCastleEditorGizmo.LoadVisualization(const RootNode: TX3DRootNode);
begin
  { GizmoScene is created on demand, to avoid creating it for things that
    never call LoadVisualization. }
  if GizmoScene = nil then
  begin
    GizmoScene := TCastleScene.Create(Self);
    InternalTransform(GizmoScene);
    Add(GizmoScene);
  end;

  GizmoScene.Load(RootNode, true);
end;

function TInternalCastleEditorGizmo.LinesShape(const Points: array of TVector3): TShapeNode;
var
  Coord: TCoordinateNode;
begin
  Result := LinesShape(Coord, [High(Points) + 1]);
  Coord.SetPoint(Points);
end;

function TInternalCastleEditorGizmo.LinesShape(out Coord: TCoordinateNode;
  const VertexCount: array of Integer): TShapeNode;
var
  LineSet: TLineSetNode;
  Material: TUnlitMaterialNode;
  Appearance: TAppearanceNode;
begin
  Coord := TCoordinateNode.Create;

  LineSet := TLineSetNode.Create;
  LineSet.Coord := Coord;
  LineSet.SetVertexCount(VertexCount);

  Material := TUnlitMaterialNode.Create;
  Material.EmissiveColor := HexToColorRGB('FC9C2D');

  Appearance := TAppearanceNode.Create;
  Appearance.Material := Material;

  Result := TShapeNode.Create;
  Result.Geometry := LineSet;
  Result.Appearance := Appearance;
end;

const
  DistanceToExistGizmo = 1;

procedure TInternalCastleEditorGizmo.LocalRender(const Params: TRenderParams);
var
  DistanceToCameraSqr: Single;
  GizmoShouldExist: Boolean;
begin
  DistanceToCameraSqr := PointsDistanceSqr(
    Params.Transform^.MultPoint(TVector3.Zero),
    Params.RenderingCamera.Position
  );
  GizmoShouldExist := DistanceToCameraSqr > Sqr(DistanceToExistGizmo);
  if not GizmoShouldExist then
    Exit; // do not show gizmo
  inherited;
end;

function TInternalCastleEditorGizmo.LocalRayCollision(const RayOrigin, RayDirection: TVector3;
  const TrianglesToIgnoreFunc: TTriangleIgnoreFunc): TRayCollision;
var
  DistanceToCameraSqr: Single;
  GizmoShouldExist: Boolean;
begin
  { Do not collide with FGizmoSelect.
    This is esp. important for TCastleCamera gizmo,
    without this code we would prevent selecting any transform when we look from
    some non-design camera,
    as camera's GizmoSelect would collide with all rays cast from camera position. }
  DistanceToCameraSqr := PointsDistanceSqr(
    TVector3.Zero,
    RayOrigin
  );
  GizmoShouldExist := DistanceToCameraSqr > Sqr(DistanceToExistGizmo);
  if not GizmoShouldExist then
    Exit(nil); // do not pick with gizmo with raycast
  Result := inherited;
end;

procedure TInternalCastleEditorGizmo.SetIconUrl(const Url: String);
begin
  GizmoImage.Url := Url;
end;

{$endif read_implementation}
