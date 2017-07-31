{
  Copyright 2006-2017 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Helpers to visualize debug information in 3D. }
unit CastleDebug3D;

interface

uses Classes,
  Castle3D, CastleBoxes, X3DNodes, CastleScene, CastleVectors, CastleColors;

type
  { 3D axis, as an X3D node, to easily visualize debug things.

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

  { A scene that can be added to some T3DCustomTransform
    (as it's child, not transformed any further) to visualize
    the parameters of it's parent (bounding volumes and such).

    After constructing it, you must always @link(Attach) it to some
    parent @link(T3DCustomTransform) instance.
    It will insert this scene as a child of indicated parent,
    and also it will follow the parent parameters then (updating
    itself in every Update, looking at parent properties). }
  TDebug3DCustomTransform = class(TCastleScene)
  strict private
    FBox: TDebugBox;
    FSphere: TDebugSphere;
    FMiddleAxis: TDebugAxis;
    FOuterTransform: TTransformNode;
    FTransform: TTransformNode;
    FParent: T3DCustomTransform;
    procedure UpdateParent;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Attach(const AParent: T3DCustomTransform);
    procedure Update(const SecondsPassed: Single; var RemoveMe: TRemoveType); override;
    { Add things that are expressed in world-space under this transform. }
    property RootTransform: TTransformNode read FTransform;
  end;

implementation

{ TDebugAxis ----------------------------------------------------------------- }

constructor TDebugAxis.Create(const AOwner: TComponent; const Color: TCastleColorRGB);
begin
  inherited Create(AOwner);

  FCoord := TCoordinateNode.Create;
  FCoord.FdPoint.Items.AddRange([
    Vector3(-1,  0,  0), Vector3(1, 0, 0),
    Vector3( 0, -1,  0), Vector3(0, 1, 0),
    Vector3( 0,  0, -1), Vector3(0, 0, 1)
  ]);

  FGeometry := TLineSetNode.Create;
  FGeometry.FdVertexCount.Items.AddRange([2, 2, 2]);
  FGeometry.FdCoord.Value := FCoord;

  FShape := TShapeNode.Create;
  FShape.Geometry := FGeometry;
  FShape.Material := TMaterialNode.Create;
  FShape.Material.ForcePureEmissive;
  FShape.Material.EmissiveColor := Color;

  FTransform := TTransformNode.Create;
  FTransform.FdChildren.Add(FShape);
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
begin
  inherited Create(AOwner);

  FGeometry := TBoxNode.Create;

  FShape := TShapeNode.Create;
  FShape.Geometry := FGeometry;
  FShape.Shading := shWireframe;
  FShape.Material := TMaterialNode.Create;
  FShape.Material.ForcePureEmissive;
  FShape.Material.EmissiveColor := Color;

  FTransform := TTransformNode.Create;
  FTransform.FdChildren.Add(FShape);
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
begin
  inherited Create(AOwner);

  FGeometry := TSphereNode.Create;
  FGeometry.Slices := 10;
  FGeometry.Stacks := 10;

  FShape := TShapeNode.Create;
  FShape.Geometry := FGeometry;
  FShape.Shading := shWireframe;
  FShape.Material := TMaterialNode.Create;
  FShape.Material.ForcePureEmissive;
  FShape.Material.EmissiveColor := Color;

  FTransform := TTransformNode.Create;
  FTransform.FdChildren.Add(FShape);
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

{ TDebug3DCustomTransform ---------------------------------------------------- }

constructor TDebug3DCustomTransform.Create(AOwner: TComponent);
var
  Root: TX3DRootNode;
begin
  inherited;

  FBox := TDebugBox.Create(Self, GrayRGB);
  FSphere := TDebugSphere.Create(Self, GrayRGB);
  FMiddleAxis := TDebugAxis.Create(Self, YellowRGB);

  FTransform := TTransformNode.Create;
  FTransform.FdChildren.Add(FBox.Root);
  FTransform.FdChildren.Add(FSphere.Root);
  FTransform.FdChildren.Add(FMiddleAxis.Root);

  FOuterTransform := TTransformNode.Create;
  FOuterTransform.FdChildren.Add(FTransform);

  Root := TX3DRootNode.Create;
  Root.FdChildren.Add(FOuterTransform);

  Load(Root, true);
  Collides := false;
  Pickable := false;
  CastShadowVolumes := false;
  ExcludeFromStatistics := true;
  InternalExcludeFromParentBoundingVolume := true;
end;

procedure TDebug3DCustomTransform.Attach(const AParent: T3DCustomTransform);
begin
  FParent := AParent;
  FParent.Add(Self);

  { call Update explicitly for the 1st time, to initialize everything now }
  UpdateParent;
end;

procedure TDebug3DCustomTransform.Update(const SecondsPassed: Single; var RemoveMe: TRemoveType);
begin
  inherited;
  if FParent <> nil then // do not update if not attached to parent
    UpdateParent;
end;

procedure TDebug3DCustomTransform.UpdateParent;
var
  BBox: TBox3D;
  R: Single;
begin
  { resign when FParent.World unset, then Middle and PreferredHeight
    cannot be calculated yet }
  if FParent.World = nil then Exit;

  // update FOuterTransform, FTransform to cancel parent's transformation
  FOuterTransform.Rotation := RotationNegate(FParent.Rotation);
  FTransform.Translation := -FParent.Translation;

  // show FParent.BoundingBox
  BBox := FParent.BoundingBox;
  FBox.Box := BBox;

  // show FParent.Sphere
  FSphere.Render := FParent.Sphere(R);
  if FSphere.Render then
  begin
    FSphere.Position := FParent.Middle;
    FSphere.Radius := R;
  end;

  // show FParent.Middle
  FMiddleAxis.Position := FParent.Middle;
  FMiddleAxis.ScaleFromBox := BBox;
end;

end.
