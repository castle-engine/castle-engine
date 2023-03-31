{
  Copyright 2006-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Helpers to visualize debug information about transformation. }
unit CastleDebugTransform;

interface

uses Classes,
  CastleTransform, CastleBoxes, X3DNodes, CastleScene, CastleVectors, CastleColors;

type
  { 3D axis, as an X3D node, to easily visualize debug things.
    This is useful in connection with your custom TDebugTransform descendants,
    to show an axis to visualize something.

    Create it and add the @link(Root) to your X3D scene graph
    within some @link(TCastleSceneCore.RootNode).
    You can change properties like @link(Position) at any time
    (before and after adding the @link(TCastleSceneCore.RootNode)
    to some graph). }
  TDebugAxis = class(TComponent)
  strict private
    FShape: TShapeNode;
    FGeometry: TLineSetNode;
    FCoord: TCoordinateNode;
    FTransform: TTransformNode;
    function GetRender: boolean;
    procedure SetRender(const Value: boolean);
    procedure SetPosition(const Value: TVector3);
    procedure SetScaleFromBox(const Value: TBox3D);
  public
    constructor Create(const AOwner: TComponent; const Color: TCastleColorRGB); reintroduce;
    property Root: TTransformNode read FTransform;
    property Render: boolean read GetRender write SetRender;
    property Position: TVector3 {read GetPosition} {} write SetPosition;
    property ScaleFromBox: TBox3D {read GetScale} {} write SetScaleFromBox;
  end;

  { 3D box, as an X3D node, to easily visualize debug things.
    This is a ready construction using X3D TBoxNode, TShapeNode, TTransformNode
    to give you a comfortable box visualization.

    This is useful in connection with your custom TDebugTransform descendants,
    to show an axis to visualize something.

    Create it and add the @link(Root) to your X3D scene graph
    within some @link(TCastleSceneCore.RootNode).
    You can change properties like @link(Box) at any time
    (before and after adding the @link(TCastleSceneCore.RootNode)
    to some graph). }
  TDebugBox = class(TComponent)
  strict private
    FColor: TCastleColor;
    FTransform: TTransformNode;
    FShape: TShapeNode;
    FGeometry: TBoxNode;
    FMaterial: TUnlitMaterialNode;
    procedure SetBox(const Value: TBox3D);
    procedure SetColor(const AValue: TCastleColor);
  public
    constructor Create(AOwner: TComponent); overload; override;
    constructor Create(const AOwner: TComponent; const AColor: TCastleColorRGB); reintroduce; overload;
      deprecated 'use Create(AOwner) and adjust Color property';
    property Root: TTransformNode read FTransform;
    property Box: TBox3D {read GetBox} {} write SetBox;
    property Color: TCastleColor read FColor write SetColor;
  end;

  { 3D sphere, as an X3D node, to easily visualize debug things.
    This is a ready construction using X3D TSphereNode, TShapeNode, TTransformNode
    to give you a comfortable sphere visualization.

    This is useful in connection with your custom TDebugTransform descendants,
    to show an axis to visualize something.

    Create it and add the @link(Root) to your X3D scene graph
    within some @link(TCastleSceneCore.RootNode).
    You can change properties like @link(Position) at any time
    (before and after adding the @link(TCastleSceneCore.RootNode)
    to some graph). }
  TDebugSphere = class(TComponent)
  strict private
    FTransform: TTransformNode;
    FShape: TShapeNode;
    FGeometry: TSphereNode;
    function GetRender: boolean;
    procedure SetRender(const Value: boolean);
    procedure SetPosition(const Value: TVector3);
    procedure SetRadius(const Value: Single);
  public
    constructor Create(const AOwner: TComponent; const Color: TCastleColorRGB); reintroduce;
    property Root: TTransformNode read FTransform;
    property Render: boolean read GetRender write SetRender;
    property Position: TVector3 {read GetPosition} {} write SetPosition;
    property Radius: Single {read GetRadius} {} write SetRadius;
  end;

  { 3D arrow, as an X3D node, to easily visualize debug things.

    This is useful in connection with your custom TDebugTransform descendants,
    to show an arrow to visualize something.

    Create it and add the @link(Root) to your X3D scene graph
    within some @link(TCastleSceneCore.RootNode). }
  TDebugArrow = class(TComponent)
  strict private
    FTransform: TTransformNode;
    FShape: TShapeNode;
    FOrigin, FDirection: TVector3;
    Coord: TCoordinateNode;
    procedure SetOrigin(const Value: TVector3);
    procedure SetDirection(const Value: TVector3);
    procedure UpdateGeometry;
    function GetRender: boolean;
    procedure SetRender(const Value: boolean);
  public
    constructor Create(const AOwner: TComponent; const Color: TCastleColorRGB); reintroduce;
    property Root: TTransformNode read FTransform;
    property Origin: TVector3 read FOrigin write SetOrigin;
    property Direction: TVector3 read FDirection write SetDirection;
    property Render: boolean read GetRender write SetRender;
  end;

  { Visualization of a bounding volume of a TCastleTransform instance.
    After constructing this, set @link(Parent) to attach this to some
    @link(TCastleTransform) instance.

    Then set @link(Exists) (which is by default @false) to control whether the debug visualization
    should actually be shown. We take care to only actually construct
    internal TCastleScene when the @link(Exists) becomes @true,
    so you can construct TDebugTransform instance always, even in release mode --
    it does not take up resources if never visible. }
  TDebugTransformBox = class(TComponent)
  strict private
    type
      TInternalScene = class(TCastleScene)
        Container: TDebugTransformBox;
        procedure Update(const SecondsPassed: Single; var RemoveMe: TRemoveType); override;
        procedure LocalRender(const Params: TRenderParams); override;
      end;
    var
      FBox: TDebugBox;
      FTransform: TMatrixTransformNode;
      FParentSpace: TAbstractGroupingNode;
      FParent: TCastleTransform;
      FScene: TInternalScene;
      FExists: boolean;
      FBoxColor: TCastleColor;
    procedure SetBoxColor(const AValue: TCastleColor);
    procedure UpdateSafe;
    procedure SetExists(const Value: boolean);
    procedure SetParent(const Value: TCastleTransform);
    procedure Initialize;
  strict protected
    { Called when internal scene is constructed.
      You can override it in desdendants to e.g. add more stuff to ParentSpace. }
    procedure InitializeNodes; virtual;

    { Called continuosly when internal scene should be updated.
      You can override it in desdendants to e.g. update the things you added
      in @link(Initialize). }
    procedure Update; virtual;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Attach(const AParent: TCastleTransform); deprecated 'set Parent instead';
    { Determines what is visualizated by this component.
      May be @nil, which means that nothing is visualized. }
    property Parent: TCastleTransform read FParent write SetParent;
    { Is the debug visualization visible. }
    property Exists: boolean read FExists write SetExists default false;
    { Add to this additional things that are expressed in parent coordinate-space.
      Be sure to call @link(ChangedScene) afterwards, unless you do it in InitializeNodes
      (then @link(ChangedScene) is not necessary). }
    property ParentSpace: TAbstractGroupingNode read FParentSpace;
    property BoxColor: TCastleColor read FBoxColor write SetBoxColor;
    procedure ChangedScene;
  end;

  { Like TDebugTransformBox, but visualizes also additional properties.

    Adds visualization of:
    - TCastleTransform.Middle
    - TCastleTransform.Sphere
    - TCastleTransform.Direction }
  TDebugTransform = class(TDebugTransformBox)
  strict private
    FDirectionArrow: TDebugArrow;
    FSphere: TDebugSphere;
    FMiddleAxis: TDebugAxis;
  strict protected
    procedure InitializeNodes; override;
    procedure Update; override;
  end;

implementation

uses CastleLog;

{ TDebugAxis ----------------------------------------------------------------- }

constructor TDebugAxis.Create(const AOwner: TComponent; const Color: TCastleColorRGB);
var
  Material: TUnlitMaterialNode;
  Appearance: TAppearanceNode;
begin
  inherited Create(AOwner);

  FCoord := TCoordinateNode.Create;
  FCoord.SetPoint([
    Vector3(-1,  0,  0), Vector3(1, 0, 0),
    Vector3( 0, -1,  0), Vector3(0, 1, 0),
    Vector3( 0,  0, -1), Vector3(0, 0, 1)
  ]);

  FGeometry := TLineSetNode.Create;
  FGeometry.Mode := lmPair;
  FGeometry.Coord := FCoord;

  Material := TUnlitMaterialNode.Create;
  Material.EmissiveColor := Color;

  Appearance := TAppearanceNode.Create;
  Appearance.ShadowCaster := false;
  Appearance.Material := Material;

  FShape := TShapeNode.Create;
  FShape.Geometry := FGeometry;
  FShape.Appearance := Appearance;

  FTransform := TTransformNode.Create;
  FTransform.AddChildren(FShape);
end;

function TDebugAxis.GetRender: boolean;
begin
  Result := FShape.Render;
end;

procedure TDebugAxis.SetRender(const Value: boolean);
begin
  FShape.Render := Value;
end;

procedure TDebugAxis.SetPosition(const Value: TVector3);
begin
  FTransform.Translation := Value;
end;

procedure TDebugAxis.SetScaleFromBox(const Value: TBox3D);
var
  ScaleFactor: Single;
begin
  ScaleFactor := Value.AverageSize(true, 1) / 2;
  FTransform.Scale := Vector3(ScaleFactor, ScaleFactor, ScaleFactor);
end;

{ TDebugBox ----------------------------------------------------------------- }

constructor TDebugBox.Create(const AOwner: TComponent; const AColor: TCastleColorRGB);
begin
  Create(AOwner);
  Color := Vector4(AColor, 1);
end;

constructor TDebugBox.Create(AOwner: TComponent);
var
  Appearance: TAppearanceNode;
begin
  inherited Create(AOwner);

  FColor := White;

  FGeometry := TBoxNode.Create;

  FMaterial := TUnlitMaterialNode.Create;
  FMaterial.EmissiveColor := FColor.XYZ;
  FMaterial.Transparency := 1 - FColor.W;

  Appearance := TAppearanceNode.Create;
  Appearance.ShadowCaster := false;
  Appearance.Material := FMaterial;

  FShape := TShapeNode.Create;
  FShape.Geometry := FGeometry;
  FShape.Shading := shWireframe;
  FShape.Appearance := Appearance;

  FTransform := TTransformNode.Create;
  FTransform.AddChildren(FShape);
end;

procedure TDebugBox.SetBox(const Value: TBox3D);
begin
  FShape.Render := not Value.IsEmpty;
  if FShape.Render then
  begin
    FGeometry.Size := Value.Size;
    FTransform.Translation := Value.Center;
  end;
end;

procedure TDebugBox.SetColor(const AValue: TCastleColor);
begin
  if TCastleColor.PerfectlyEquals(FColor, AValue) then Exit;
  FColor := AValue;
  FMaterial.EmissiveColor := FColor.XYZ;
  FMaterial.Transparency := 1 - FColor.W;
end;

{ TDebugSphere ----------------------------------------------------------------- }

constructor TDebugSphere.Create(const AOwner: TComponent; const Color: TCastleColorRGB);
var
  Material: TUnlitMaterialNode;
  Appearance: TAppearanceNode;
begin
  inherited Create(AOwner);

  FGeometry := TSphereNode.Create;
  FGeometry.Slices := 10;
  FGeometry.Stacks := 10;

  Material := TUnlitMaterialNode.Create;
  Material.EmissiveColor := Color;

  Appearance := TAppearanceNode.Create;
  Appearance.ShadowCaster := false;
  Appearance.Material := Material;

  FShape := TShapeNode.Create;
  FShape.Geometry := FGeometry;
  FShape.Shading := shWireframe;
  FShape.Appearance := Appearance;

  FTransform := TTransformNode.Create;
  FTransform.AddChildren(FShape);
end;

function TDebugSphere.GetRender: boolean;
begin
  Result := FShape.Render;
end;

procedure TDebugSphere.SetRender(const Value: boolean);
begin
  FShape.Render := Value;
end;

procedure TDebugSphere.SetPosition(const Value: TVector3);
begin
  FTransform.Translation := Value;
end;

procedure TDebugSphere.SetRadius(const Value: Single);
begin
  FGeometry.Radius := Value;
end;

{ TDebugArrow ----------------------------------------------------------------- }

constructor TDebugArrow.Create(const AOwner: TComponent; const Color: TCastleColorRGB);
var
  Material: TUnlitMaterialNode;
  Appearance: TAppearanceNode;
  FGeometry: TLineSetNode;
begin
  inherited Create(AOwner);

  FGeometry := TLineSetNode.CreateWithTransform(FShape, FTransform);

  Material := TUnlitMaterialNode.Create;
  Material.EmissiveColor := Color;

  Appearance := TAppearanceNode.Create;
  Appearance.ShadowCaster := false;
  Appearance.Material := Material;

  FShape.Appearance := Appearance;

  Coord := TCoordinateNode.Create;

  FGeometry.Coord := Coord;
  FGeometry.SetVertexCount([2, 2, 2, 2, 2]);

  { Make the initial geometry. Although it is useless, this will avoid warning
    "Too much lines (not enough coordinates) in LineSet". }
  UpdateGeometry;
end;

procedure TDebugArrow.SetOrigin(const Value: TVector3);
begin
  if not TVector3.PerfectlyEquals(FOrigin, Value) then
  begin
    FOrigin := Value;
    UpdateGeometry;
  end;
end;

procedure TDebugArrow.SetDirection(const Value: TVector3);
begin
  if not TVector3.PerfectlyEquals(FDirection, Value) then
  begin
    FDirection := Value;
    UpdateGeometry;
  end;
end;

procedure TDebugArrow.UpdateGeometry;
var
  OrthoDirection, OrthoDirection2: TVector3;
begin
  OrthoDirection := AnyOrthogonalVector(Direction);
  OrthoDirection2 := TVector3.CrossProduct(Direction, OrthoDirection);

  OrthoDirection := OrthoDirection.AdjustToLength(Direction.Length / 4);
  OrthoDirection2 := OrthoDirection2.AdjustToLength(Direction.Length / 4);

  Coord.SetPoint([
    Origin,
    Origin + Direction,
    Origin + Direction,
    Origin + Direction * 0.75 + OrthoDirection,
    Origin + Direction,
    Origin + Direction * 0.75 - OrthoDirection,
    Origin + Direction,
    Origin + Direction * 0.75 + OrthoDirection2,
    Origin + Direction,
    Origin + Direction * 0.75 - OrthoDirection2
  ]);
end;

function TDebugArrow.GetRender: boolean;
begin
  Result := FShape.Render;
end;

procedure TDebugArrow.SetRender(const Value: boolean);
begin
  FShape.Render := Value;
end;

{ TDebugTransform.TInternalScene ---------------------------------------------------- }

procedure TDebugTransformBox.TInternalScene.Update(const SecondsPassed: Single; var RemoveMe: TRemoveType);
begin
  inherited;
  Container.UpdateSafe;
end;

procedure TDebugTransformBox.TInternalScene.LocalRender(const Params: TRenderParams);
const
  DistanceToHide = 1;
var
  DistanceToCameraSqr: Single;
  GizmoVisible: Boolean;
begin
  { Do not render cameras debug, when their center is equal or very close
    to the rendering camera.
    This avoids weird debug display of camera on top of itself. }
  if Exists and (Parent is TCastleCamera) then
  begin
    DistanceToCameraSqr := PointsDistanceSqr(
      Params.Transform^.MultPoint(TVector3.Zero),
      Params.RenderingCamera.Position
    );
    GizmoVisible := DistanceToCameraSqr > Sqr(DistanceToHide);
    if not GizmoVisible then
      Exit;
  end;
  inherited;
end;

{ TDebugTransformBox ---------------------------------------------------- }

constructor TDebugTransformBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBoxColor := Gray;
end;

destructor TDebugTransformBox.Destroy;
begin
  { set to nil by SetParent, to detach free notification }
  Parent := nil;
  inherited;
end;

procedure TDebugTransformBox.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FParent) then
    { set to nil by SetParent, to detach free notification }
    Parent := nil;
end;

procedure TDebugTransformBox.Initialize;
var
  Root: TX3DRootNode;
begin
  FTransform := TMatrixTransformNode.Create;
  FParentSpace := FTransform;

  FBox := TDebugBox.Create(Self);
  FBox.Color := FBoxColor;
  ParentSpace.AddChildren(FBox.Root);

  InitializeNodes;

  Root := TX3DRootNode.Create;
  Root.AddChildren(FTransform);

  FScene := TInternalScene.Create(Self);
  FScene.Container := Self;
  FScene.Load(Root, true);
  FScene.Collides := false;
  FScene.Pickable := false;
  FScene.CastShadows := false;
  FScene.ExcludeFromStatistics := true;
  FScene.InternalExcludeFromParentBoundingVolume := true;
  FScene.InternalExistsOnlyInMeaningfulParents := true;
  FScene.Exists := FExists;
  FScene.SetTransient;
end;

procedure TDebugTransformBox.InitializeNodes;
begin
end;

procedure TDebugTransformBox.SetParent(const Value: TCastleTransform);
begin
  if FParent <> Value then
  begin
    if FParent <> nil then
    begin
      Assert(FScene <> nil);
      // remove self from previous parent children
      if not (csDestroying in FParent.ComponentState) then // testcase why it's needed: fps_game exit
        FParent.Remove(FScene);
      FParent.RemoveFreeNotification(Self);
    end;

    FParent := Value;

    if FParent <> nil then
    begin
      if FScene = nil then
        Initialize;
      FParent.FreeNotification(Self);
      FParent.Add(FScene);
      UpdateSafe;
    end;
  end;
end;

procedure TDebugTransformBox.Attach(const AParent: TCastleTransform);
begin
  Parent := AParent;
end;

procedure TDebugTransformBox.SetExists(const Value: boolean);
begin
  if FExists <> Value then
  begin
    FExists := Value;
    if FScene <> nil then
      FScene.Exists := Value;
    if Value then
      UpdateSafe;
  end;
end;

procedure TDebugTransformBox.UpdateSafe;
begin
  if Exists and
     (FParent <> nil) then
  begin
    if FScene = nil then
      Initialize;
    Update;
  end;
end;

procedure TDebugTransformBox.SetBoxColor(const AValue: TCastleColor);
begin
  FBoxColor := AValue;
  if FBox <> nil then
    FBox.Color := AValue;
end;

procedure TDebugTransformBox.Update;
begin
  // update FTransform to cancel parent's transformation
  FTransform.Matrix := FParent.InverseTransform;

  // show FParent.BoundingBox
  FBox.Box := FParent.BoundingBox;
end;

procedure TDebugTransformBox.ChangedScene;
begin
  FScene.ChangedAll;
end;

{ TDebugTransform ---------------------------------------------------- }

procedure TDebugTransform.InitializeNodes;
begin
  inherited;

  FDirectionArrow := TDebugArrow.Create(Self, BlueRGB);
  ParentSpace.AddChildren(FDirectionArrow.Root);

  FSphere := TDebugSphere.Create(Self, GrayRGB);
  ParentSpace.AddChildren(FSphere.Root);

  FMiddleAxis := TDebugAxis.Create(Self, YellowRGB);
  ParentSpace.AddChildren(FMiddleAxis.Root);
end;

procedure TDebugTransform.Update;
var
  R: Single;
  Visible: Boolean;
begin
  inherited;

  Visible := Parent.World <> nil;

  // when Parent.World = nil then Parent.Middle and Parent.PreferredHeight cannot be calculated
  FSphere.Render := Visible;
  FDirectionArrow.Render := Visible;
  FMiddleAxis.Render := Visible;

  if Visible then
  begin
    // show FParent.Sphere
    FSphere.Render := Parent.Sphere(R);
    if FSphere.Render then
    begin
      FSphere.Position := Parent.Middle;
      FSphere.Radius := R;
    end;

    // show FParent.Direction
    FDirectionArrow.Origin := Parent.Middle;
    FDirectionArrow.Direction := Parent.Direction;

    // show FParent.Middle
    FMiddleAxis.Position := Parent.Middle;
    FMiddleAxis.ScaleFromBox := Parent.BoundingBox;
  end;
end;

end.
