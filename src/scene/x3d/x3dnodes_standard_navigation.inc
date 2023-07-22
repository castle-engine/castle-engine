{%MainUnit x3dnodes.pas}
{
  Copyright 2002-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_interface}
  { Base type for viewpoints in X3D,
    which are locations from which the user may view the scene. }
  TAbstractViewpointNode = class(TAbstractBindableNode)
  strict private
    procedure EventSet_BindReceive(
      const Event: TX3DEvent; const Value: TX3DField; const Time: TX3DTime);
    function GetPosition: TVector3;
    procedure SetPosition(const Value: TVector3);
  strict protected
    function PositionField: TSFVec3f; virtual; abstract;
  public
    constructor Create(const AX3DName: string = ''; const ABaseUrl: string = ''); override;
    function TransformationChange: TNodeTransformationChange; override;

    { Position of the viewpoint. }
    property Position: TVector3 read GetPosition write SetPosition;

    class function ProjectionType: TProjectionType; virtual; abstract;

    { Calculate camera vectors (position, direction, up, gravity up).
      Follows VRML/X3D specification:

      @unorderedList(
        @item(position is taken directly from FdPosition field,)
        @item(direction and up are (respectively) -Z and +Y rotated by FdOrientation,)
        @item(gravity up is +Y.)
      )

      They are all then transformed by the current viewpoint transformation
      (determined by parent nodes like TTransformNode).

      One conclusion from the above is that the only way to change the gravity up
      vector (this determines in which direction viewer falls down)
      is to use the Transform node around the viewpoint node.

      Additionally, as an extension, we also look at FdDirection and FdUp
      and FdGravityUp vectors. See
      https://castle-engine.io/x3d_extensions.php#section_ext_cameras_alt_orient

      Returned CamDir, CamUp, GravityUp are @italic(always normalized).

      TODO: This may need transform to world space, not just scene space,
      just like TAbstractLightNode.GetView does.
      Test how it behaves in TCastleScene that has some transformation. }
    procedure GetView(out CamPos, CamDir, CamUp, GravityUp: TVector3); overload;
    procedure GetView(out CamView: TViewVectors); overload;

    { Description generated smart (trying to use FdDescription field,
      falling back on other information to help user identify the node). }
    function SmartDescription: string; virtual;

    { Matrices for projecting texture from this viewpoint,
      for ProjectedTextureCoordinate.
      Override ProjectionMatrix for subclasses (ModelviewMatrix
      is already correctly defined here).
      @groupBegin }
    function ProjectionMatrix: TMatrix4; virtual;
    function ModelviewMatrix: TMatrix4;
    function GetProjectorMatrix: TMatrix4;
    { @groupEnd }

  {$I auto_generated_node_helpers/x3dnodes_x3dviewpointnode.inc}
  end;

  TAbstractX3DViewpointNode = TAbstractViewpointNode deprecated 'use TAbstractViewpointNode';

  TX3DViewpointClassNode = class of TAbstractViewpointNode;

  { Grouping node that transforms the coordinate system of its children
    so that they always turn towards the viewer.
    The local Z axis of the children faces the camera,
    rotating around @link(AxisOfRotation) (it it is non-zero)
    or rotating freely in 3D (when @link(AxisOfRotation) is exactly zero). }
  TBillboardNode = class(TAbstractGroupingNode)
  strict private
    type
      TBillboardNodeTransformFunctionality = class(TTransformFunctionality)
      strict private
        FParent: TBillboardNode;
      public
        constructor Create(const AParent: TBillboardNode);
        property Parent: TBillboardNode read FParent;
        procedure ApplyTransform(var Transformation: TTransformation); override;
      end;
    var
      FTransformFunctionality: TBillboardNodeTransformFunctionality;
      FCameraVectors: TViewVectors;
      FCameraVectorsKnown: boolean;
  protected
    function DirectEnumerateActive(Func: TEnumerateChildrenFunction): Pointer; override;
  public
    constructor Create(const AX3DName: String = ''; const ABaseUrl: String = ''); override;
    function TransformationChange: TNodeTransformationChange; override;

    { Change transformation to reflect current camera. @exclude }
    procedure InternalCameraChanged(const ACameraVectors: TViewVectors);

  {$I auto_generated_node_helpers/x3dnodes_billboard.inc}
  end;

  { Grouping node that specifies the collision detection properties
    for its children. }
  TCollisionNode = class(TAbstractGroupingNode)
  protected
    function DirectEnumerateActive(Func: TEnumerateChildrenFunction): Pointer; override;
    function DirectEnumerateActiveForTraverse(
      Func: TEnumerateChildrenFunction;
      StateStack: TX3DGraphTraverseStateStack): Pointer; override;
  public
    constructor Create(const AX3DName: string = ''; const ABaseUrl: string = ''); override;

    { Setup this Collision node to collide as a Box.

      This sets the @link(Proxy) field to a simple box with given bounds.

      The @code(Enabled) field is unchanged (by default @true).

      The overloaded version with VisibleNode also sets VisibleNode
      as the only displayed child of this collision node.
      The overloaded version without VisibleNode doesn't modify the displayed children.

      It honors the case of Box being empty correctly. Proxy is then
      non-nil, but without any geometry. So the collisions are
      effectively disabled, in a consistent way (without changing the
      @code(Enabled) field).
    }
    procedure CollideAsBox(const Box: TBox3D); overload;
    procedure CollideAsBox(const VisibleNode: TX3DNode; const Box: TBox3D); overload;

  {$I auto_generated_node_helpers/x3dnodes_collision.inc}
  end;

  { Provides various levels of detail for a given object,
    only one of which will be visible at a given time.

    Note that it follows X3D naming.
    There were some differences between VRML 2.0 LOD and X3D LOD (TLODNode):
    "level" -> "children", so also "level_changed" -> "children_changed".
    In X3D "level_changed" indicates just integer.
    We don't implement VRML 97 version due to low usage.
  }
  TLODNode = class(TAbstractGroupingNode)
  protected
    function DirectEnumerateActive(Func: TEnumerateChildrenFunction): Pointer; override;
  public
    function TransformationChange: TNodeTransformationChange; override;

  {$I auto_generated_node_helpers/x3dnodes_lod.inc}
  end;

  { Describe the physical characteristics of the viewer's avatar and navigation. }
  TNavigationInfoNode = class(TAbstractBindableNode)
  strict private
    procedure EventSet_BindReceive(
      const Event: TX3DEvent; const Value: TX3DField; const Time: TX3DTime);
  public
    constructor Create(const AX3DName: string = ''; const ABaseUrl: string = ''); override;

  {$I auto_generated_node_helpers/x3dnodes_navigationinfo.inc}
  end;

  { Viewpoint that provides an orthographic view of the scene. }
  TOrthoViewpointNode = class(TAbstractViewpointNode)
  strict private
    function GetFieldOfViewDefault(const Index: Integer): Single;
    function GetFieldOfView(const Index: Integer): Single;
    procedure SetFieldOfView(const Index: Integer; const Value: Single); overload;
    { Special setter because delphi can't work with overloaded procedures in properties.
      More info:
      https://docwiki.embarcadero.com/RADStudio/Sydney/en/E2271_Property_getters_and_setters_cannot_be_overloaded_(Delphi) }
    procedure SetFieldOfViewDelphiCompat(const Index: Integer; const Value: Single);
  strict protected
    function PositionField: TSFVec3f; override;
  public
    { Field of view determines how much you see in the camera.
      Use this e.g. to zoom in/out.

      This property has comfortable getter and setter, you can
      also get and set the indexes in 0..3 range, where

      @unorderedList(
        @itemSpacing Compact
        @item 0 index is "min x" (default value -1)
        @item 1 index is "min y" (default value -1)
        @item 2 index is "max x" (default value 1)
        @item 3 index is "max y" (default value 1)
      )
    }
    property FieldOfView [const Index: Integer]: Single read GetFieldOfView write SetFieldOfView;

    { Field of view - minimum X. -1 by default. @seealso FieldOfView }
    property FieldOfViewMinX: Single index 0 read GetFieldOfView
      write SetFieldOfViewDelphiCompat;
    { Field of view - minimum Y. -1 by default. @seealso FieldOfView }
    property FieldOfViewMinY: Single index 1 read GetFieldOfView
      write SetFieldOfViewDelphiCompat;
    { Field of view - maximum X. 1 by default. @seealso FieldOfView }
    property FieldOfViewMaxX: Single index 2 read GetFieldOfView
      write SetFieldOfViewDelphiCompat;
    { Field of view - maximum Y. 1 by default. @seealso FieldOfView }
    property FieldOfViewMaxY: Single index 3 read GetFieldOfView
      write SetFieldOfViewDelphiCompat;

    class function ProjectionType: TProjectionType; override;
    function ProjectionMatrix: TMatrix4; override;

    { Calculate final field of view value, taking into account aspect ratio.
      The idea is that OrthoViewpoint.fieldOfView specifies the minimal
      extents. Depending on your window aspect ratio, you may need to make
      one extent (vertical or horizontal) larger to adjust. }
    class function InternalFieldOfView(
      const AFieldOfView: TFloatRectangle;
      const ViewportWidth, ViewportHeight: Single): TFloatRectangle;

  {$I auto_generated_node_helpers/x3dnodes_orthoviewpoint.inc}
  end;

  { Viewpoint that provides a perspective view of the scene. }
  TViewpointNode = class(TAbstractViewpointNode)
  strict protected
    function PositionField: TSFVec3f; override;
  public
    class function ProjectionType: TProjectionType; override;

    { Calculate both perspective angles (in radians),
      knowing
      @link(TCastlePerspective.FieldOfView TCastleCamera.Perspective.FieldOfView) and
      @link(TCastlePerspective.FieldOfViewAxis TCastleCamera.Perspective.FieldOfViewAxis).

      In particular when FieldOfViewAxis = faSmallest then we perform
      the calculation requested by X3D specification about
      ViewpointNode fieldOfView behavior. }
    class function InternalFieldOfView(FieldOfView: Single;
      const FieldOfViewAxis: TFieldOfViewAxis;
      const ViewportWidth, ViewportHeight: Single): TVector2;

    function ProjectionMatrix: TMatrix4; override;

  {$I auto_generated_node_helpers/x3dnodes_viewpoint.inc}
  end;

  { Group of viewpoints. You can (optionally) arrange viewpoints in groups
    to present them nicely in the X3D browser submenus. }
  TViewpointGroupNode = class(TAbstractChildNode)
  protected
    function DirectEnumerateActive(Func: TEnumerateChildrenFunction): Pointer; override;
  public
    { Description generated smart (trying to use FdDescription field,
      falling back on other information to help user identify the node). }
    function SmartDescription: string;

  {$I auto_generated_node_helpers/x3dnodes_viewpointgroup.inc}
  end;
{$endif read_interface}

{$ifdef read_implementation}

const
  ShapeSortNames: array [TShapeSort] of string =
  ('AUTO', 'NONE', '2D', '3D', '3D_VERTICAL_BILLBOARDS', '3D_ORIGIN', '3D_GROUND', 'CUSTOM');

{ TAbstractViewpointNode -------------------------------------------------- }

constructor TAbstractViewpointNode.Create(const AX3DName: string = ''; const ABaseUrl: string = '');
begin
  inherited;
  Eventset_bind.AddNotification({$ifdef FPC}@{$endif} EventSet_BindReceive);
  FdOrientation.ChangeAlways := chViewpointVectors;
  FdDirection.ChangeAlways := chViewpointVectors;
  FdUp.ChangeAlways := chViewpointVectors;
  FdGravityUp.ChangeAlways := chViewpointVectors;
end;

procedure TAbstractViewpointNode.GetView(
  out CamView: TViewVectors);
var
  IgnoredGravityUp: TVector3;
begin
  GetView(CamView.Translation, CamView.Direction, CamView.Up, IgnoredGravityUp);
end;

procedure TAbstractViewpointNode.GetView(
  out CamPos, CamDir, CamUp, GravityUp: TVector3);
begin
  CamPos := Position;

  if FdDirection.Items.Count > 0 then
  begin
    CamDir := FdDirection.Items.L[0];
    if CamDir.IsZero then
    begin
      WritelnWarning('VRML/X3D', 'Viewpoint "direction" must not be zero, assuming defaults');
      CamDir := FdOrientation.RotatedPoint( DefaultX3DCameraDirection );
    end;
  end else
    CamDir := FdOrientation.RotatedPoint( DefaultX3DCameraDirection );

  if FdUp.Items.Count > 0 then
  begin
    CamUp := FdUp.Items.L[0];
    if CamUp.IsZero then
    begin
      WritelnWarning('VRML/X3D', 'Viewpoint "up" must not be zero, assuming defaults');
      CamUp := FdOrientation.RotatedPoint( DefaultX3DCameraUp );
    end;
  end else
    CamUp := FdOrientation.RotatedPoint( DefaultX3DCameraUp );

  GravityUp := FdGravityUp.Value;
  if GravityUp.IsZero then
    GravityUp := DefaultX3DGravityUp;

  CamPos    := Transform.MultPoint(CamPos);
  { Since the Transform can contain scale,
    we need to always normalize resulting vectors,
    even if input (like CamDir) was already normalized. }
  CamDir    := Transform.MultDirection(CamDir).Normalize;
  CamUp     := Transform.MultDirection(CamUp).Normalize;

  if GravityTransform then
    GravityUp := Transform.MultDirection(GravityUp).Normalize
  else
    GravityUp := GravityUp.Normalize;

  Assert(SameValue(CamDir.LengthSqr, 1.0, 0.0001));
  Assert(SameValue(CamUp.LengthSqr, 1.0, 0.0001));
end;

procedure TAbstractViewpointNode.EventSet_BindReceive(
  const Event: TX3DEvent; const Value: TX3DField; const Time: TX3DTime);
begin
  if Scene <> nil then
    Scene.GetViewpointStack.Set_Bind(Self, (Value as TSFBool).Value);
end;

function TAbstractViewpointNode.TransformationChange: TNodeTransformationChange;
begin
  Result := ntcViewpoint;
end;

function TAbstractViewpointNode.SmartDescription: string;
begin
  Result := FdDescription.Value;
  { if node doesn't have a "description" field, or it's left empty, use node name }
  if Result = '' then
    Result := X3DName;
  { if even the node name is empty, just show node type. }
  if Result = '' then
    Result := X3DType;
end;

function TAbstractViewpointNode.GetPosition: TVector3;
begin
  Result := PositionField.Value;
end;

procedure TAbstractViewpointNode.SetPosition(const Value: TVector3);
begin
  PositionField.Send(Value);
end;

function TAbstractViewpointNode.ProjectionMatrix: TMatrix4;
begin
  Result := TMatrix4.Identity;
end;

function TAbstractViewpointNode.ModelviewMatrix: TMatrix4;
var
  CamView: TViewVectors;
begin
  GetView(CamView);
  Result := CamView.Matrix;
end;

function TAbstractViewpointNode.GetProjectorMatrix: TMatrix4;
begin
  Result := ProjectionMatrix * ModelviewMatrix;
end;

{ TBillboardNode.TBillboardNodeTransformFunctionality ---------------------------- }

constructor TBillboardNode.TBillboardNodeTransformFunctionality.Create(const AParent: TBillboardNode);
begin
  inherited Create(AParent);
  FParent := AParent;
end;

procedure TBillboardNode.TBillboardNodeTransformFunctionality.ApplyTransform(var Transformation: TTransformation);
var
  NewX, NewY, NewZ, BillboardToViewer, P1, P2, LocalDirection, LocalUp: TVector3;
  Angle: Single;
  M, IM: TMatrix4;
  PlaneAxis: TVector4;
  PlaneAxisDir: TVector3 absolute PlaneAxis;
begin
  if Parent.FCameraVectorsKnown then
  begin
    if Parent.FdAxisOfRotation.Value.IsPerfectlyZero then
    begin
      LocalDirection := Transformation.InverseTransform.MultDirection(Parent.FCameraVectors.Direction);
      LocalUp        := Transformation.InverseTransform.MultDirection(Parent.FCameraVectors.Up);
      { although FCameraVectors.Direction/Up are for sure normalized and orthogonal,
        but Transformation.InverseTransform may contain scaling,
        so be sure to normalize again the result.
        For safety, also call MakeVectorsOrthoOnTheirPlane. }
      LocalDirection := LocalDirection.Normalize;
      LocalUp := LocalUp.Normalize;
      MakeVectorsOrthoOnTheirPlane(LocalDirection, LocalUp);

      NewX := TVector3.CrossProduct(LocalDirection, LocalUp);
      NewY := LocalUp;
      NewZ := -LocalDirection;

      TransformCoordsMatrices(NewX, NewY, NewZ, M, IM);
      Transformation.Transform := Transformation.Transform * M;
      Transformation.InverseTransform := IM * Transformation.InverseTransform;
    end else
    begin
      { vector from node origin to FCameraVectors.Position, in local coords }
      BillboardToViewer := Transformation.InverseTransform.MultPoint(
        Parent.FCameraVectors.Translation);

      { plane of axisOfRotation }
      PlaneAxisDir := Parent.FdAxisOfRotation.Value;
      PlaneAxis.W := 0;

      { we want to have a rotation that changes TVector3.One[2]
        into BillboardToViewer. But the rotation axis is fixed
        to axisOfRotation (we cannot just take their TVector3.CrossProduct).
        So project both points on a plane orthogonal to axisOfRotation,
        and calculate angle there. }
      P1 := PointOnPlaneClosestToPoint(PlaneAxis, TVector3.One[2]);
      P2 := PointOnPlaneClosestToPoint(PlaneAxis, BillboardToViewer);

      if { axisOfRotation paralell to Z axis? Then nothing sensible to do. }
        P1.IsZero or
        { billboard-to-viewer vector parallel to axisOfRotation (includes
          the case when billboard-to-viewer vector is zero in local coords,
          which means that camera standing at Billboard origin)?
          Then nothing sensible to do. }
        P2.IsZero then
        Exit;

      { As https://sourceforge.net/p/castle-engine/tickets/38/ shows,
        the above checks for zero are not enough. The more precise check
        (because it replicates what happens in CosAngleBetweenVectors)
        would be to add

          (P1.LengthSqr * P2.LengthSqr < SingleEpsilon)

        But that's not really a future-proof solution, to repeat this code.
        It's safer (even if a little slower) to capture exception here. }
      try
        Angle := RotationAngleRadBetweenVectors(P1, P2, Parent.FdAxisOfRotation.Value);
      except
        on EVectorInvalidOp do Exit;
      end;

      RotationMatricesRad(Angle, Parent.FdAxisOfRotation.Value, M, IM);
      Transformation.Transform := Transformation.Transform * M;
      Transformation.InverseTransform := IM * Transformation.InverseTransform;
    end;
  end;
end;

{ TBillboardNode ------------------------------------------------------------- }

constructor TBillboardNode.Create(const AX3DName, ABaseUrl: String);
begin
  inherited;
  FTransformFunctionality := TBillboardNodeTransformFunctionality.Create(Self);
  AddFunctionality(FTransformFunctionality);
end;

function TBillboardNode.DirectEnumerateActive(Func: TEnumerateChildrenFunction): Pointer;
begin
  Result := FdChildren.Enumerate(Func);
  if Result <> nil then Exit;
end;

procedure TBillboardNode.InternalCameraChanged(const ACameraVectors: TViewVectors);
begin
  FCameraVectorsKnown := true;
  FCameraVectors := ACameraVectors;
end;

function TBillboardNode.TransformationChange: TNodeTransformationChange;
begin
  Result := ntcTransform;
end;

{ TCollisionNode ------------------------------------------------------------- }

constructor TCollisionNode.Create(const AX3DName, ABaseUrl: String);
begin
  inherited;

  { In VRML 2.0, Collision didn't descend from X3DSensorName and had
    special field "collide". In X3D, "enabled" is used for the exact
    same purpose. }
  FdEnabled.AddAlternativeName('collide', 2);
end;

function TCollisionNode.DirectEnumerateActive(Func: TEnumerateChildrenFunction): Pointer;
begin
  Result := FdProxy.Enumerate(Func);
  if Result <> nil then Exit;

  Result := FdChildren.Enumerate(Func);
  if Result <> nil then Exit;
end;

function TCollisionNode.DirectEnumerateActiveForTraverse(
  Func: TEnumerateChildrenFunction;
  StateStack: TX3DGraphTraverseStateStack): Pointer;
begin
  Result := nil;
  if FdEnabled.Value then
  begin
    if FdProxy.Value = nil then
    begin
      { Collision node doesn't do anything in this trivial case,
        children are treated just like by Group. }
      Result := FdChildren.Enumerate(Func);
      if Result <> nil then Exit;
    end else
    begin
      { This is the interesting case:
        proxy is not visible,
        children are not collidable. }

      Inc(StateStack.Top.InsideInvisible);
      try
        Result := FdProxy.Enumerate(Func);
        if Result <> nil then Exit;
      finally Dec(StateStack.Top.InsideInvisible) end;

      Inc(StateStack.Top.InsideIgnoreCollision);
      try
        Result := FdChildren.Enumerate(Func);
        if Result <> nil then Exit;
      finally Dec(StateStack.Top.InsideIgnoreCollision) end;
    end;
  end else
  begin
    { Nothing is collidable in this case. So proxy is just ignored. }
    Inc(StateStack.Top.InsideIgnoreCollision);
    try
      Result := FdChildren.Enumerate(Func);
      if Result <> nil then Exit;
    finally Dec(StateStack.Top.InsideIgnoreCollision) end;
  end;
end;

procedure TCollisionNode.CollideAsBox(const Box: TBox3D);
var
  ProxyTransform: TTransformNode;
  ProxyShape: TShapeNode;
  ProxyBox: TBoxNode;
begin
  { always create ProxyTransform, even when Box.IsEmpty,
    otherwise X3D Collision.proxy would be ignored when nil. }
  ProxyTransform := TTransformNode.Create('', BaseUrl);

  if not Box.IsEmpty then
  begin
    ProxyBox := TBoxNode.Create('', BaseUrl);
    ProxyBox.Size := Box.Size;

    ProxyShape := TShapeNode.Create('', BaseUrl);
    ProxyShape.Geometry := ProxyBox;

    ProxyTransform.Translation := Box.Center;
    ProxyTransform.AddChildren(ProxyShape);
  end;
  Proxy := ProxyTransform;
end;

procedure TCollisionNode.CollideAsBox(const VisibleNode: TX3DNode; const Box: TBox3D);
begin
  CollideAsBox(Box);
  FdChildren.Clear;
  FdChildren.Add(VisibleNode);
end;

{ TLODNode ----------------------------------------------------------- }

function TLODNode.DirectEnumerateActive(Func: TEnumerateChildrenFunction): Pointer;
begin
  Result := nil;
  { For now we simply always use the best LOD version,
    avoiding whole issue of choosing proper LOD child. }
  if FdChildren.Count >= 1 then
  begin
    Result := Func(Self, FdChildren[0]);
    if Result <> nil then Exit;
  end;
end;

function TLODNode.TransformationChange: TNodeTransformationChange;
begin
  Result := ntcLOD;
end;

{ TNavigationInfoNode -------------------------------------------------------- }

constructor TNavigationInfoNode.Create(const AX3DName, ABaseUrl: String);
begin
  inherited;
  Eventset_bind.AddNotification({$ifdef FPC}@{$endif} EventSet_BindReceive);
end;

procedure TNavigationInfoNode.EventSet_BindReceive(
  const Event: TX3DEvent; const Value: TX3DField; const Time: TX3DTime);
begin
  if Scene <> nil then
    Scene.GetNavigationInfoStack.Set_Bind(Self, (Value as TSFBool).Value);
end;

{ TOrthoViewpointNode -------------------------------------------------------- }

function TOrthoViewpointNode.PositionField: TSFVec3f;
begin
  Result := FdPosition;
end;

class function TOrthoViewpointNode.ProjectionType: TProjectionType;
begin
  Result := ptOrthographic;
end;

function TOrthoViewpointNode.ProjectionMatrix: TMatrix4;
var
  Dimensions: TFloatRectangle;
begin
  { default Dimensions, for OrthoViewpoint }
  Dimensions.Left   := -1;
  Dimensions.Bottom := -1;
  Dimensions.Width  :=  2;
  Dimensions.Height :=  2;

  if FdFieldOfView.Items.Count > 0 then Dimensions.Left   := FdFieldOfView.Items[0];
  if FdFieldOfView.Items.Count > 1 then Dimensions.Bottom := FdFieldOfView.Items[1];
  if FdFieldOfView.Items.Count > 2 then Dimensions.Width  := FdFieldOfView.Items[2] - Dimensions.Left;
  if FdFieldOfView.Items.Count > 3 then Dimensions.Height := FdFieldOfView.Items[3] - Dimensions.Bottom;

  { TODO: for currently bound viewpoint, we should honour
    fieldOfView and aspect ratio of current window,
    by calling InternalFieldOfView. }

  Result := OrthoProjectionMatrix(Dimensions,
    1, 100); { TODO: near, far projection testing values }
end;

function TOrthoViewpointNode.GetFieldOfViewDefault(const Index: Integer): Single;
begin
  if Index < 2 then
    Result := -1 else
  if Index < 4 then
    Result := 1 else
    Result := 0;
end;

function TOrthoViewpointNode.GetFieldOfView(const Index: Integer): Single;
begin
  if Index < FdFieldOfView.Items.Count then
    Result := FdFieldOfView.Items[Index] else
    Result := GetFieldOfViewDefault(Index);
end;

procedure TOrthoViewpointNode.SetFieldOfView(const Index: Integer; const Value: Single);
begin
  while Index >= FdFieldOfView.Items.Count do
    FdFieldOfView.Items.Add(GetFieldOfViewDefault(FdFieldOfView.Items.Count));
  FdFieldOfView.Items[Index] := Value;
  FdFieldOfView.Changed;
end;

procedure TOrthoViewpointNode.SetFieldOfViewDelphiCompat(const Index: Integer; const Value: Single);
begin
  SetFieldOfView(Index, Value);
end;


class function TOrthoViewpointNode.InternalFieldOfView(
  const AFieldOfView: TFloatRectangle;
  const ViewportWidth, ViewportHeight: Single): TFloatRectangle;

  { Scale the extent. Since InternalFieldOfView should only make AFieldOfView
    larger (because OrthoViewpoint.fieldOfView gives the minimal extents),
    so given here Scale should always be >= 1. }
  procedure ScaleExtent(const Scale: Single; var Min, Max: Single);
  var
    L, Middle: Single;
  begin
    Middle := (Min + Max) / 2;
    L := Max - Min;

    if L < 0 then
    begin
      WritelnLog('OrthoViewpoint', 'OrthoViewpoint.fieldOfView max extent smaller than min extent');
      Exit;
    end;

    Min := Middle - Scale * L / 2;
    Max := Middle + Scale * L / 2;
  end;

var
  FOVAspect, WindowWidthToHeight: Single;
  Min, Max: Single;
begin
  Result := AFieldOfView;

  if (Result.Width <= 0) or
     (Result.Height <= 0) then
  begin
    WritelnLog('OrthoViewpoint', 'OrthoViewpoint.fieldOfView extent (max-min) is zero');
    Exit;
  end;

  FOVAspect := Result.Width / Result.Height;
  WindowWidthToHeight := ViewportWidth / ViewportHeight;

  { The idea is to change FieldOfView, such that at the end the above
    equation would calculate FOVAspect as equal to WindowWidthToHeight.

    To do this, multiply above equation by WindowWidthToHeight / FOVAspect.
    We have to transform put this scale into either horizontal or vertical
    extent, since we only want to make FieldOfView larger (never smaller). }

  if FOVAspect > WindowWidthToHeight then
  begin
    Min := Result.Bottom;
    Max := Result.Top;
    ScaleExtent(FOVAspect / WindowWidthToHeight, Min, Max);
    Result.Bottom := Min;
    Result.Height := Max - Min;
  end else
  if FOVAspect < WindowWidthToHeight then
  begin
    Min := Result.Left;
    Max := Result.Right;
    ScaleExtent(WindowWidthToHeight / FOVAspect, Min, Max);
    Result.Left := Min;
    Result.Width := Max - Min;
  end;
end;

{ TViewpointNode ------------------------------------------------------------- }

function TViewpointNode.PositionField: TSFVec3f;
begin
  Result := FdPosition;
end;

class function TViewpointNode.ProjectionType: TProjectionType;
begin
  Result := ptPerspective;
end;

function TViewpointNode.ProjectionMatrix: TMatrix4;
begin
  { TODO: for currently bound viewpoint, we should honour
    fieldOfView and aspect ratio of current window? }
  Result := PerspectiveProjectionMatrixRad(
    FdFieldOfView.Value, 1,
    1, 100); { TODO: near, far projection testing values }
end;

class function TViewpointNode.InternalFieldOfView(FieldOfView: Single;
  const FieldOfViewAxis: TFieldOfViewAxis;
  const ViewportWidth, ViewportHeight: Single): TVector2;

  procedure CalculateFovWidth;
  begin
    Result.X := AdjustViewAngleRadToAspectRatio(
      Result.Y, ViewportWidth / ViewportHeight);
  end;

  procedure CalculateFovHeight;
  begin
    Result.Y := AdjustViewAngleRadToAspectRatio(
      Result.X, ViewportHeight / ViewportWidth);
  end;

begin
  case FieldOfViewAxis of
    faSmallest:
      begin
        { This follows X3D specification about ViewpointNode fieldOfView behavior. }
        ClampVar(FieldOfView, 0.01, Pi - 0.01);
        if ViewportWidth < ViewportHeight then
        begin
          Result.X := FieldOfView;
          CalculateFovHeight;
          if Result.Y > Pi then
          begin
            Result.Y := Pi;
            CalculateFovWidth;
          end;
        end else
        begin
          Result.Y := FieldOfView;
          CalculateFovWidth;
          if Result.X > Pi then
          begin
            Result.X := Pi;
            CalculateFovHeight;
          end;
        end;
      end;
    faLargest:
      begin
        if ViewportWidth > ViewportHeight then
        begin
          Result.X := FieldOfView;
          CalculateFovHeight;
        end else
        begin
          Result.Y := FieldOfView;
          CalculateFovWidth;
        end;
      end;
    faHorizontal:
      begin
        Result.X := FieldOfView;
        CalculateFovHeight;
      end;
    faVertical:
      begin
        Result.Y := FieldOfView;
        CalculateFovWidth;
      end;
    {$ifndef COMPILER_CASE_ANALYSIS}
    else raise EInternalError.Create('ViewpointAngleOfView-FieldOfViewAxis?');
    {$endif}
  end;
end;

{ TViewpointGroupNode -------------------------------------------------------- }

function TViewpointGroupNode.DirectEnumerateActive(Func: TEnumerateChildrenFunction): Pointer;
begin
  Result := inherited;
  if Result <> nil then Exit;

  Result := FdChildren.Enumerate(Func);
  if Result <> nil then Exit;
end;

function TViewpointGroupNode.SmartDescription: string;
begin
  Result := FdDescription.Value;
  if Result = '' then
    Result := X3DName;
  if Result = '' then
    Result := X3DType;
end;

procedure RegisterNavigationNodes;
begin
  NodesManager.RegisterNodeClasses([
    TBillboardNode,
    TCollisionNode,
    TLODNode,
    TNavigationInfoNode,
    TOrthoViewpointNode,
    TViewpointNode,
    TViewpointGroupNode
  ]);
end;
{$endif read_implementation}
