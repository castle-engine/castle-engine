{%MainUnit castlescene.pas}
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
    Create this only if you need it (usually check "if CastleDesignMode ...").
    @exclude }
  TInternalCastleEditorGizmo = class(TCastleTransform)
  strict private
    GizmoSelect: TCastleBox;
    GizmoImage: TCastleImageTransform;
    GizmoBillboard: TCastleBillboard;
    GizmoScene: TCastleScene; //< Note that it may remain nil, if LoadVisualization not called

    const
      DistanceToExistGizmo = 1;

    { Set various properties for internal TCastleTransform, shown only for debug. }
    procedure InternalTransform(const T: TCastleTransform);
    function GetSelectBoxTranslation: TVector3;
    procedure SetSelectBoxTranslation(const Value: TVector3);
  protected
    function LocalRayCollision(const RayOrigin, RayDirection: TVector3;
      const TrianglesToIgnoreFunc: TTriangleIgnoreFunc): TRayCollision; override;
  public
    class var
      { When non-zero, LocalBoundingBox and BoundingBox of it is empty. }
      EmptyBoundingBox: Cardinal;

    constructor Create(AOwner: TComponent); override;
    procedure LocalRender(const Params: TRenderParams); override;
    function LocalBoundingBox: TBox3D; override;

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
    property SelectBoxTranslation: TVector3 read GetSelectBoxTranslation write SetSelectBoxTranslation;
  end;

{$endif read_interface}

{$ifdef read_implementation}

{ TInternalCastleEditorGizmo ------------------------------------------------- }

constructor TInternalCastleEditorGizmo.Create(AOwner: TComponent);
const
  GizmoSize = 1.0;
begin
  inherited;
  SetTransient;

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
  GizmoImage.Exists := false; // will change to true in SetIconUrl, just a tiny optimization to not use GizmoImage when no icon
  InternalTransform(GizmoImage);
  Add(GizmoImage);

  GizmoBillboard := TCastleBillboard.Create(Self);
  GizmoBillboard.AxisOfRotation := TVector3.Zero;
  GizmoImage.AddBehavior(GizmoBillboard);
end;

procedure TInternalCastleEditorGizmo.InternalTransform(const T: TCastleTransform);
begin
  T.InternalExcludeFromParentBoundingVolume := true;
  T.Pickable := false;
  T.Collides := false;
  T.CastShadows := false;
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
  ThisVertexCount: Integer;
  TotalVertexCount: Cardinal;
  InitialVertexes: array of TVector3;
begin
  TotalVertexCount := 0;
  for ThisVertexCount in VertexCount do
    TotalVertexCount := TotalVertexCount + ThisVertexCount;

  Coord := TCoordinateNode.Create;
  // Initialize with zero vectors, to allow to initialize VBO and avoid warnings about not enough coordinates.
  SetLength(InitialVertexes, TotalVertexCount);
  Coord.SetPoint(InitialVertexes);

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

function TInternalCastleEditorGizmo.LocalBoundingBox: TBox3D;
begin
  if EmptyBoundingBox <> 0 then
    Exit(TBox3D.Empty)
  else
    Result := inherited LocalBoundingBox;
end;

procedure TInternalCastleEditorGizmo.LocalRender(const Params: TRenderParams);
var
  DistanceToCameraSqr: Single;
  GizmoShouldExist: Boolean;
begin
  DistanceToCameraSqr := PointsDistanceSqr(
    Params.Transform^.MultPoint(TVector3.Zero),
    Params.RenderingCamera.View.Translation
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
  GizmoImage.Exists := Url <> '';
end;

function TInternalCastleEditorGizmo.GetSelectBoxTranslation: TVector3;
begin
  Result := GizmoSelect.Translation;
end;

procedure TInternalCastleEditorGizmo.SetSelectBoxTranslation(const Value: TVector3);
begin
  GizmoSelect.Translation := Value;
end;

{$endif read_implementation}
