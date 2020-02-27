{
  Copyright 2006-2018 Michalis Kamburelis.

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
    FTransform: TTransformNode;
    FShape: TShapeNode;
    FGeometry: TBoxNode;
    procedure SetBox(const Value: TBox3D);
  public
    constructor Create(const AOwner: TComponent; const Color: TCastleColorRGB); reintroduce;
    property Root: TTransformNode read FTransform;
    property Box: TBox3D {read GetBox} {} write SetBox;
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

  { Visualization of a bounding volume (and maybe other properties)
    of a TCastleTransform instance.
    After constructing this, call @link(Attach) to attach this to some
    @link(TCastleTransform) instance.

    Then set @link(Exists) to control whether the debug visualization
    should actually be shown. We take care to only actually construct
    internal TCastleScene when the @link(Exists) becomes @true,
    so you can construct TDebugTransform instance always, even in release mode --
    it does not take up resources if never visible. }
  TDebugTransform = class(TComponent)
  strict private
    type
      TInternalScene = class(TCastleScene)
        Container: TDebugTransform;
        procedure Update(const SecondsPassed: Single; var RemoveMe: TRemoveType); override;
      end;
    var
      FBox: TDebugBox;
      FTransform: TMatrixTransformNode;
      FWorldSpace: TAbstractX3DGroupingNode;
      FSphere: TDebugSphere;
      FMiddleAxis: TDebugAxis;
      FParent: TCastleTransform;
      FScene: TInternalScene;
      FExists: boolean;
    procedure UpdateSafe;
    procedure SetExists(const Value: boolean);
  strict protected
    { Called when internal scene is constructed.
      You can override it in desdendants to e.g. add more stuff to WorldSpace. }
    procedure Initialize; virtual;

    { Called continuosly when internal scene should be updated.
      You can override it in desdendants to e.g. update the things you added
      in @link(Initialize). }
    procedure Update; virtual;
  public
    procedure Attach(const AParent: TCastleTransform);
    { Is the debug visualization visible. }
    property Exists: boolean read FExists write SetExists default false;
    { Add additional things that are expressed in world-space under this transform.
      Be sure to call @link(ChangedScene) afterwards. }
    property WorldSpace: TAbstractX3DGroupingNode read FWorldSpace;
    procedure ChangedScene;
  end;

implementation

uses CastleLog;

{ TDebugAxis ----------------------------------------------------------------- }

constructor TDebugAxis.Create(const AOwner: TComponent; const Color: TCastleColorRGB);
var
  Material: TUnlitMaterialNode;
begin
  inherited Create(AOwner);

  FCoord := TCoordinateNode.Create;
  FCoord.SetPoint([
    Vector3(-1,  0,  0), Vector3(1, 0, 0),
    Vector3( 0, -1,  0), Vector3(0, 1, 0),
    Vector3( 0,  0, -1), Vector3(0, 0, 1)
  ]);

  FGeometry := TLineSetNode.Create;
  FGeometry.SetVertexCount([2, 2, 2]);
  FGeometry.Coord := FCoord;

  Material := TUnlitMaterialNode.Create;
  Material.EmissiveColor := Color;

  FShape := TShapeNode.Create;
  FShape.Geometry := FGeometry;
  FShape.Material := Material;

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

constructor TDebugBox.Create(const AOwner: TComponent; const Color: TCastleColorRGB);
var
  Material: TUnlitMaterialNode;
begin
  inherited Create(AOwner);

  FGeometry := TBoxNode.Create;

  FShape := TShapeNode.Create;
  FShape.Geometry := FGeometry;
  FShape.Shading := shWireframe;

  Material := TUnlitMaterialNode.Create;
  Material.EmissiveColor := Color;
  FShape.Material := Material;

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

{ TDebugSphere ----------------------------------------------------------------- }

constructor TDebugSphere.Create(const AOwner: TComponent; const Color: TCastleColorRGB);
var
  Material: TUnlitMaterialNode;
begin
  inherited Create(AOwner);

  FGeometry := TSphereNode.Create;
  FGeometry.Slices := 10;
  FGeometry.Stacks := 10;

  FShape := TShapeNode.Create;
  FShape.Geometry := FGeometry;
  FShape.Shading := shWireframe;

  Material := TUnlitMaterialNode.Create;
  Material.EmissiveColor := Color;
  FShape.Material := Material;

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

{ TDebugTransform.TInternalScene ---------------------------------------------------- }

procedure TDebugTransform.TInternalScene.Update(const SecondsPassed: Single; var RemoveMe: TRemoveType);
begin
  inherited;
  Container.UpdateSafe;
end;

{ TDebugTransform ---------------------------------------------------- }

procedure TDebugTransform.Initialize;
var
  Root: TX3DRootNode;
begin
  FTransform := TMatrixTransformNode.Create;
  FWorldSpace := FTransform;

  FBox := TDebugBox.Create(Self, GrayRGB);
  WorldSpace.AddChildren(FBox.Root);

  FSphere := TDebugSphere.Create(Self, GrayRGB);
  WorldSpace.AddChildren(FSphere.Root);

  FMiddleAxis := TDebugAxis.Create(Self, YellowRGB);
  WorldSpace.AddChildren(FMiddleAxis.Root);

  Root := TX3DRootNode.Create;
  Root.AddChildren(FTransform);

  FScene := TInternalScene.Create(Self);
  FScene.Container := Self;
  FScene.Load(Root, true);
  FScene.Collides := false;
  FScene.Pickable := false;
  FScene.CastShadowVolumes := false;
  FScene.ExcludeFromStatistics := true;
  FScene.InternalExcludeFromParentBoundingVolume := true;
  FScene.Exists := FExists;
end;

procedure TDebugTransform.Attach(const AParent: TCastleTransform);
begin
  if FScene = nil then
    Initialize;

  FParent := AParent;
  FParent.Add(FScene);
  UpdateSafe;
end;

procedure TDebugTransform.SetExists(const Value: boolean);
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

procedure TDebugTransform.UpdateSafe;
begin
  if Exists and
     (FParent <> nil) and
     { resign when FParent.World unset,
       as then FParent.Middle and FParent.PreferredHeight cannot be calculated }
     (FParent.World <> nil) then
  begin
    if FScene = nil then
      Initialize;
    Update;
  end;
end;

procedure TDebugTransform.Update;
var
  R: Single;
begin
  // update FTransform to cancel parent's transformation
  FTransform.Matrix := FParent.InverseTransform;

  // show FParent.BoundingBox
  FBox.Box := FParent.BoundingBox;

  // show FParent.Sphere
  FSphere.Render := FParent.Sphere(R);
  if FSphere.Render then
  begin
    FSphere.Position := FParent.Middle;
    FSphere.Radius := R;
  end;

  // show FParent.Middle
  FMiddleAxis.Position := FParent.Middle;
  FMiddleAxis.ScaleFromBox := FParent.BoundingBox;
end;

procedure TDebugTransform.ChangedScene;
begin
  FScene.ChangedAll;
end;

end.
