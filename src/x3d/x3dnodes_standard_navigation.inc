{
  Copyright 2002-2017 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_interface}
  { Common ancestor for both VRML 1.0 camera nodes and VRML/X3D >= 2.0 viewpoint
    nodes. }
  TAbstractViewpointNode = class(TAbstractBindableNode)
  strict private
    procedure EventSet_BindReceive(
      Event: TX3DEvent; Value: TX3DField; const Time: TX3DTime);
    function GetPosition: TVector3Single;
    procedure SetPosition(const Value: TVector3Single);
    function GetOrientation: TVector4Single;
    procedure SetOrientation(const Value: TVector4Single);
  strict protected
    function PositionField: TSFVec3f; virtual; abstract;
  public
    procedure CreateNode; override;
    function TransformationChange: TNodeTransformationChange; override;

    private FFdOrientation: TSFRotation;
    public property FdOrientation: TSFRotation read FFdOrientation;
    property Orientation: TVector4Single read GetOrientation write SetOrientation;

    private FFdDirection: TMFVec3f;
    public property FdDirection: TMFVec3f read FFdDirection;

    private FFdUp: TMFVec3f;
    public property FdUp: TMFVec3f read FFdUp;

    private FFdGravityUp: TSFVec3f;
    public property FdGravityUp: TSFVec3f read FFdGravityUp;

    private FEventCameraMatrix: TSFMatrix4fEvent;
    public property EventCameraMatrix: TSFMatrix4fEvent read FEventCameraMatrix;

    private FEventCameraInverseMatrix: TSFMatrix4fEvent;
    public property EventCameraInverseMatrix: TSFMatrix4fEvent read FEventCameraInverseMatrix;

    private FEventCameraRotationMatrix: TSFMatrix3fEvent;
    public property EventCameraRotationMatrix: TSFMatrix3fEvent read FEventCameraRotationMatrix;

    private FEventCameraRotationInverseMatrix: TSFMatrix3fEvent;
    public property EventCameraRotationInverseMatrix: TSFMatrix3fEvent read FEventCameraRotationInverseMatrix;

    private FFdCameraMatrixSendAlsoOnOffscreenRendering: TSFBool;
    public property FdCameraMatrixSendAlsoOnOffscreenRendering: TSFBool read FFdCameraMatrixSendAlsoOnOffscreenRendering;

    { Position of the viewpoint. }
    property Position: TVector3Single read GetPosition write SetPosition;

    class function ProjectionType: TProjectionType; virtual; abstract;

    { Calculate camera vectors (position, direction, up, gravity up).
      Follows VRML/X3D specification:

      @unorderedList(
        @item(position is taken directly from FdPosition field,)
        @item(direction and up are (respectively) -Z and +Y rotated by FdOrientation,)
        @item(gravity up is +Y.)
      )

      They are all then transformed by the current viewpoint transformation
      (determined by parent nodes like Transform).

      One conclusion from the above is that the only way to change the gravity up
      vector (this determines in which direction viewer falls down)
      is to use the Transform node around the viewpoint node.

      Additionally, as an extension, we also look at FdDirection and FdUp
      and FdGravityUp vectors. See
      http://castle-engine.sourceforge.net/x3d_extensions.php#section_ext_cameras_alt_orient

      Returned CamDir, CamUp, GravityUp are @italic(always normalized). }
    procedure GetView(out CamPos, CamDir, CamUp, GravityUp: TVector3Single);

    private FFdDescription: TSFString;
    public property FdDescription: TSFString read FFdDescription;

    { Description generated smart (trying to use FdDescription field,
      falling back on other information to help user identify the node). }
    function SmartDescription: string; virtual;
  end;

  TX3DViewpointClassNode = class of TAbstractViewpointNode;

  { Base type for viewpoints in X3D,
    which are locations from which the user may view the scene. }
  TAbstractX3DViewpointNode = class(TAbstractViewpointNode)
  public
    procedure CreateNode; override;

    private FFdCenterOfRotation: TSFVec3f;
    public property FdCenterOfRotation: TSFVec3f read FFdCenterOfRotation;

    private FFdJump: TSFBool;
    public property FdJump: TSFBool read FFdJump;

    private FFdRetainUserOffsets: TSFBool;
    public property FdRetainUserOffsets: TSFBool read FFdRetainUserOffsets;

    { Matrices for projecting texture from this viewpoint,
      for ProjectedTextureCoordinate.
      Override ProjectionMatrix for subclasses (ModelviewMatrix
      is already correctly defined here).
      @groupBegin }
    function ProjectionMatrix: TMatrix4Single; virtual;
    function ModelviewMatrix: TMatrix4Single;
    function GetProjectorMatrix: TMatrix4Single;
    { @groupEnd }

    {$I auto_generated_node_helpers/x3dnodes_x3dviewpointnode.inc}
  end;

  { Grouping node that transforms the coordinate system of its children
    so that they always turn towards the viewer. }
  TBillboardNode = class(TAbstractX3DGroupingNode, ITransformNode)
  private
    FCameraPosition, FCameraDirection, FCameraUp: TVector3Single;
    FCameraViewKnown: boolean;
  protected
    function DirectEnumerateActive(Func: TEnumerateChildrenFunction): Pointer; override;
    procedure ApplyTransform(State: TX3DGraphTraverseState); override;
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
    class function URNMatching(const URN: string): boolean; override;
    function TransformationChange: TNodeTransformationChange; override;

    property CameraViewKnown: boolean read FCameraViewKnown;
    property CameraPosition: TVector3Single read FCameraPosition;
    property CameraDirection: TVector3Single read FCameraDirection;
    property CameraUp: TVector3Single read FCameraUp;

    procedure CameraChanged(const APosition, ADirection, AUp: TVector3Single);

    private FFdAxisOfRotation: TSFVec3f;
    public property FdAxisOfRotation: TSFVec3f read FFdAxisOfRotation;

    {$I auto_generated_node_helpers/x3dnodes_billboard.inc}
  end;

  { Grouping node that specifies the collision detection properties
    for its children. }
  TCollisionNode = class(TAbstractX3DGroupingNode, IAbstractSensorNode)
  protected
    function DirectEnumerateActive(Func: TEnumerateChildrenFunction): Pointer; override;
    function DirectEnumerateActiveForTraverse(
      Func: TEnumerateChildrenFunction;
      StateStack: TX3DGraphTraverseStateStack): Pointer; override;
    function GetProxy: TX3DNode;
    procedure SetProxy(const Value: TX3DNode);
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdEnabled: TSFBool;
    public property FdEnabled: TSFBool read FFdEnabled;

    { Event out } { }
    private FEventCollideTime: TSFTimeEvent;
    public property EventCollideTime: TSFTimeEvent read FEventCollideTime;

    { Event out } { }
    private FEventIsActive: TSFBoolEvent;
    public property EventIsActive: TSFBoolEvent read FEventIsActive;

    private FFdProxy: TSFNode;
    public property FdProxy: TSFNode read FFdProxy;

    { When non-nil, this will be used for collisions, instead of children. }
    property Proxy: TX3DNode read GetProxy write SetProxy;

    { Setup this Collision node to show VisibleNode, but collide as
      a Box. This sets VisibleNode as the only child of this collision node,
      and sets the @link(Proxy) field to a simple box with given bounds.
      The @code(Enabled) field is unchanged (by default @true).

      It honors the case of Box being empty correctly. Proxy is then
      non-nil, but without any geometry. So the collisions are
      effectively disabled, in a consistent way (without changing the
      @code(Enabled) field).
    }
    procedure CollideAsBox(const VisibleNode: TX3DNode; const Box: TBox3D);

    {$I auto_generated_node_helpers/x3dnodes_collision.inc}
  end;

  { Provides various levels of detail for a given object,
    only one of which will be visible at a given time.

    It's a common ancestor for VRML 2.0 LOD (TLODNode_2) and X3D LOD (TLODNode).
    Unfortunately, we cannot have a simple common class for both VRML 97
    and X3D because there would be a name clash for "level_changed" event:

    @unorderedList(
      @item(
        For VRML 2.0, main MFNode field was named "level" and so "level_changed"
        is an event reporting when MFNode changed.)

      @item(For X3D, main MFNode field is named "children", and so "children_changed"
        reports MFNode changes. "level_changed" is a new field, SFInt32,
        indicating which child is chosen.)
    )

    So level_changed has completely different meanings for VRML 97 and X3D.
    As an extension we'll add "levelIndex_changed", SFInt32, to be analogous
    to X3D "level_changed". This way both VRML 2.0 and X3D LOD nodes have
    the same capabilities, and common interface for programmer
    (programmer should use X3D event/fields names for Pascal property names),
    but for parser they will use different names.
  }
  TAbstractLODNode = class(TAbstractX3DGroupingNode)
  protected
    function DirectEnumerateActive(Func: TEnumerateChildrenFunction): Pointer; override;
    function GetCenter: TVector3Single;
    procedure SetCenter(const Value: TVector3Single);
    function GetForceTransitions: boolean;
    procedure SetForceTransitions(const Value: boolean);
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;

    private FFdCenter: TSFVec3f;
    public property FdCenter: TSFVec3f read FFdCenter;
    property Center: TVector3Single read GetCenter write SetCenter;

    private FFdRange: TMFFloat;
    public property FdRange: TMFFloat read FFdRange;

    private FFdForceTransitions: TSFBool;
    public property FdForceTransitions: TSFBool read FFdForceTransitions;
    property ForceTransitions: boolean read GetForceTransitions write SetForceTransitions;

    { Event out } { }
    private FEventLevel_changed: TSFInt32Event;
    public property EventLevel_changed: TSFInt32Event read FEventLevel_changed;

    function TransformationChange: TNodeTransformationChange; override;

    {$I auto_generated_node_helpers/x3dnodes_lod.inc}
  end;

  { Provides various levels of detail for a given object,
    only one of which will be visible at a given time, for VRML 2.0. }
  TLODNode_2 = class(TAbstractLODNode)
  public
    procedure CreateNode; override;

    class function ForVRMLVersion(const Version: TX3DVersion): boolean;
      override;
    class function URNMatching(const URN: string): boolean; override;
  end;

  { Provides various levels of detail for a given object,
    only one of which will be visible at a given time, for X3D. }
  TLODNode = class(TAbstractLODNode)
  public
    class function ForVRMLVersion(const Version: TX3DVersion): boolean;
      override;
    class function URNMatching(const URN: string): boolean; override;
  end;
  TLODNode_3 = TLODNode;

  TOptionalBlendingSort = (obsDefault, obsNone, obs2D, obs3D);

  { Describe the physical characteristics of the viewer's avatar and navigation. }
  TNavigationInfoNode = class(TAbstractBindableNode)
  private
    procedure EventSet_BindReceive(
      Event: TX3DEvent; Value: TX3DField; const Time: TX3DTime);
    function GetBlendingSort: TOptionalBlendingSort;
    procedure SetBlendingSort(const Value: TOptionalBlendingSort);
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdAvatarSize: TMFFloat;
    public property FdAvatarSize: TMFFloat read FFdAvatarSize;

    private FFdHeadlight: TSFBool;
    public property FdHeadlight: TSFBool read FFdHeadlight;

    private FFdSpeed: TSFFloat;
    public property FdSpeed: TSFFloat read FFdSpeed;

    private FFdTransitionTime: TSFTime;
    public property FdTransitionTime: TSFTime read FFdTransitionTime;

    private FFdTransitionType: TMFString;
    public property FdTransitionType: TMFString read FFdTransitionType;

    private FFdType: TMFString;
    public property FdType: TMFString read FFdType;

    private FFdVisibilityLimit: TSFFloat;
    public property FdVisibilityLimit: TSFFloat read FFdVisibilityLimit;

    private FFdBlendingSort: TSFStringEnum;
    public property FdBlendingSort: TSFStringEnum read FFdBlendingSort;
    property BlendingSort: TOptionalBlendingSort
      read GetBlendingSort write SetBlendingSort;

    { Event out } { }
    private FEventTransitionComplete: TSFBoolEvent;
    public property EventTransitionComplete: TSFBoolEvent read FEventTransitionComplete;

    {$I auto_generated_node_helpers/x3dnodes_navigationinfo.inc}
  end;

  { Viewpoint that provides an orthographic view of the scene. }
  TOrthoViewpointNode = class(TAbstractX3DViewpointNode)
  strict private
    function GetFieldOfViewDefault(const Index: Integer): Single;
    function GetFieldOfView(const Index: Integer): Single;
    procedure SetFieldOfView(const Index: Integer; const Value: Single);
  strict protected
    function PositionField: TSFVec3f; override;
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdFieldOfView: TMFFloat;
    public property FdFieldOfView: TMFFloat read FFdFieldOfView;

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
    property FieldOfView [Index: Integer]: Single read GetFieldOfView write SetFieldOfView;

    { Field of view - minimum X. -1 by default. @seealso FieldOfView }
    property FieldOfViewMinX: Single index 0 read GetFieldOfView write SetFieldOfView;
    { Field of view - minimum Y. -1 by default. @seealso FieldOfView }
    property FieldOfViewMinY: Single index 1 read GetFieldOfView write SetFieldOfView;
    { Field of view - maximum X. 1 by default. @seealso FieldOfView }
    property FieldOfViewMaxX: Single index 2 read GetFieldOfView write SetFieldOfView;
    { Field of view - maximum Y. 1 by default. @seealso FieldOfView }
    property FieldOfViewMaxY: Single index 3 read GetFieldOfView write SetFieldOfView;

    private FFdPosition: TSFVec3f;
    public property FdPosition: TSFVec3f read FFdPosition;

    class function ProjectionType: TProjectionType; override;
    function ProjectionMatrix: TMatrix4Single; override;

    { Fix given field of view value for window aspect ratio.
      The idea is that OrthoViewpoint.fieldOfView specifies the minimal
      extents. Depending on your window aspect ratio, you may need to make
      one extent (vertical or horizontal) larger to adjust. }
    class procedure AspectFieldOfView(
      var AFieldOfView: TVector4Single;
      const WindowWidthToHeight: Single);

    {$I auto_generated_node_helpers/x3dnodes_orthoviewpoint.inc}
  end;

  { Viewpoint that provides a perspective view of the scene. }
  TViewpointNode = class(TAbstractX3DViewpointNode)
  strict protected
    function PositionField: TSFVec3f; override;
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdFieldOfView: TSFFloat;
    public property FdFieldOfView: TSFFloat read FFdFieldOfView;

    private FFdPosition: TSFVec3f;
    public property FdPosition: TSFVec3f read FFdPosition;

    class function ProjectionType: TProjectionType; override;

    { This calculates proper angle of view for typical rectangular
      display, based on given fieldOfView field value.
      Result is in radians (just like fieldOfView VRML field).

      If you want to calculate horizontal angle of view then
      pass as ThisToOtherSizeRatio your window's width / height.
      If you want to calculate vertical angle of view then
      pass as ThisToOtherSizeRatio your window's height / width.
      For this method it doesn't really matter which is horizontal
      and which is vertical, both are treated the same.

      This works following VRML spec. So the angle of view for
      smaller window size is set to fieldOfView. The other angle
      can always be calculated by AdjustViewAngleRadToAspectRatio
      (this implements the same equation that is mentioned in VRML spec).
      The larger angle cannot be larger than Pi, and may force the
      smaller angle to be smaller than fieldOfView. }
    function AngleOfView(const ThisToOtherSizeRatio: Single): Single;

    { This is like AngleOfView, but it allows you to specify
      FieldOfView as a parameter. }
    class function ViewpointAngleOfView(
      FieldOfView: Single;
      const ThisToOtherSizeRatio: Single): Single;
    function ProjectionMatrix: TMatrix4Single; override;

    {$I auto_generated_node_helpers/x3dnodes_viewpoint.inc}
  end;

  { Group of viewpoints. You can (optionally) arrange viewpoints in groups
    to present them nicely in the X3D browser submenus. }
  TViewpointGroupNode = class(TAbstractChildNode)
  protected
    function DirectEnumerateActive(Func: TEnumerateChildrenFunction): Pointer; override;
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdCenter: TSFVec3f;
    public property FdCenter: TSFVec3f read FFdCenter;

    private FFdChildren: TMFNode;
    public property FdChildren: TMFNode read FFdChildren;

    private FFdDescription: TSFString;
    public property FdDescription: TSFString read FFdDescription;

    private FFdDisplayed: TSFBool;
    public property FdDisplayed: TSFBool read FFdDisplayed;

    private FFdRetainUserOffsets: TSFBool;
    public property FdRetainUserOffsets: TSFBool read FFdRetainUserOffsets;

    private FFdSize: TSFVec3f;
    public property FdSize: TSFVec3f read FFdSize;

    { Description generated smart (trying to use FdDescription field,
      falling back on other information to help user identify the node). }
    function SmartDescription: string;

    {$I auto_generated_node_helpers/x3dnodes_viewpointgroup.inc}
  end;
{$endif read_interface}

{$ifdef read_implementation}

{ TAbstractX3DViewpointNode -------------------------------------------------- }

procedure TAbstractViewpointNode.CreateNode;
begin
  inherited;

  { In X3D, this is part of X3DViewpointNode.
    In our engine, X3DViewpointNode descends from TAbstractViewpointNode
    inheriting "orientation" field this way. }
  FFdOrientation := TSFRotation.Create(Self, 'orientation', Vector3Single(0, 0, 1), 0);
   FdOrientation.ChangesAlways := [chViewpointVectors];
  AddField(FFdOrientation);
  { X3D specification comment: [-1,1],(-Inf,Inf) }

  FFdDirection := TMFVec3f.Create(Self, 'direction', []);
   FdDirection.ChangesAlways := [chViewpointVectors];
  AddField(FFdDirection);

  FFdUp := TMFVec3f.Create(Self, 'up', []);
   FdUp.ChangesAlways := [chViewpointVectors];
  AddField(FFdUp);

  FFdGravityUp := TSFVec3f.Create(Self, 'gravityUp', DefaultX3DGravityUp);
   FdGravityUp.ChangesAlways := [chViewpointVectors];
  AddField(FFdGravityUp);

  FEventCameraMatrix := TSFMatrix4fEvent.Create(Self, 'cameraMatrix', false);
  AddEvent(FEventCameraMatrix);

  FEventCameraInverseMatrix := TSFMatrix4fEvent.Create(Self, 'cameraInverseMatrix', false);
  AddEvent(FEventCameraInverseMatrix);

  FEventCameraRotationMatrix := TSFMatrix3fEvent.Create(Self, 'cameraRotationMatrix', false);
  AddEvent(FEventCameraRotationMatrix);

  FEventCameraRotationInverseMatrix := TSFMatrix3fEvent.Create(Self, 'cameraRotationInverseMatrix', false);
  AddEvent(FEventCameraRotationInverseMatrix);

  FFdCameraMatrixSendAlsoOnOffscreenRendering := TSFBool.Create(Self, 'cameraMatrixSendAlsoOnOffscreenRendering', false);
  AddField(FFdCameraMatrixSendAlsoOnOffscreenRendering);

  FFdDescription := TSFString.Create(Self, 'description', '');
  AddField(FFdDescription);

  Eventset_bind.OnReceive.Add(@EventSet_BindReceive);
end;

procedure TAbstractViewpointNode.GetView(
  out CamPos, CamDir, CamUp, GravityUp: TVector3Single);
begin
  CamPos := Position;

  if FdDirection.Items.Count > 0 then
  begin
    CamDir := FdDirection.Items.L[0];
    if ZeroVector(CamDir) then
    begin
      WritelnWarning('VRML/X3D', 'Viewpoint "direction" must not be zero, assuming defaults');
      CamDir := FdOrientation.RotatedPoint( DefaultX3DCameraDirection );
    end;
  end else
    CamDir := FdOrientation.RotatedPoint( DefaultX3DCameraDirection );

  if FdUp.Items.Count > 0 then
  begin
    CamUp := FdUp.Items.L[0];
    if ZeroVector(CamUp) then
    begin
      WritelnWarning('VRML/X3D', 'Viewpoint "up" must not be zero, assuming defaults');
      CamUp := FdOrientation.RotatedPoint( DefaultX3DCameraUp );
    end;
  end else
    CamUp := FdOrientation.RotatedPoint( DefaultX3DCameraUp );

  GravityUp := FdGravityUp.Value;
  if ZeroVector(GravityUp) then
    GravityUp := DefaultX3DGravityUp;

  { Niestety, macierz ponizej moze cos skalowac wiec nawet jesli powyzej
    uzylismy FdOrientation.RotatedPoint( DefaultX3DCameraDirection/Up ) i wiemy ze CamDir/Up
    jest znormalizowane - to i tak musimy je tutaj znormalizowac.
    TODO: byloby dobrze uzyc tutaj czegos jak MatrixMultDirectionNoScale }
  CamPos    := MatrixMultPoint(Transform, CamPos);
  CamDir    := Normalized( MatrixMultDirection(Transform, CamDir) );
  CamUp     := Normalized( MatrixMultDirection(Transform, CamUp) );
  GravityUp := Normalized( MatrixMultDirection(Transform, GravityUp) );

  Assert(FloatsEqual(VectorLenSqr(CamDir), 1.0, 0.0001));
  Assert(FloatsEqual(VectorLenSqr(CamUp), 1.0, 0.0001));
end;

procedure TAbstractViewpointNode.EventSet_BindReceive(
  Event: TX3DEvent; Value: TX3DField; const Time: TX3DTime);
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

function TAbstractViewpointNode.GetPosition: TVector3Single;
begin
  Result := PositionField.Value;
end;

procedure TAbstractViewpointNode.SetPosition(const Value: TVector3Single);
begin
  PositionField.Send(Value);
end;

function TAbstractViewpointNode.GetOrientation: TVector4Single;
begin
  Result := FdOrientation.Value;
end;

procedure TAbstractViewpointNode.SetOrientation(const Value: TVector4Single);
begin
  FdOrientation.Send(Value);
end;

procedure TAbstractX3DViewpointNode.CreateNode;
begin
  inherited;

  FFdCenterOfRotation := TSFVec3f.Create(Self, 'centerOfRotation', Vector3Single(0, 0, 0));
  AddField(FFdCenterOfRotation);
  { X3D specification comment: (-Inf,Inf) }

  FFdJump := TSFBool.Create(Self, 'jump', true);
  AddField(FFdJump);

  FFdRetainUserOffsets := TSFBool.Create(Self, 'retainUserOffsets', false);
  AddField(FFdRetainUserOffsets);

  DefaultContainerField := 'children';
end;

function TAbstractX3DViewpointNode.ProjectionMatrix: TMatrix4Single;
begin
  Result := IdentityMatrix4Single;
end;

function TAbstractX3DViewpointNode.ModelviewMatrix: TMatrix4Single;
var
  CamPos, CamDir, CamUp, CamGravityUp: TVector3Single;
begin
  GetView(CamPos, CamDir, CamUp, CamGravityUp);
  Result := LookDirMatrix(CamPos, CamDir, CamUp);
end;

function TAbstractX3DViewpointNode.GetProjectorMatrix: TMatrix4Single;
begin
  Result := ProjectionMatrix * ModelviewMatrix;
end;

{ TBillboardNode ------------------------------------------------------------- }

procedure TBillboardNode.CreateNode;
begin
  inherited;

  FFdAxisOfRotation := TSFVec3f.Create(Self, 'axisOfRotation', Vector3Single(0, 1, 0));
  AddField(FFdAxisOfRotation);
  { X3D specification comment: (-Inf,Inf) }

  DefaultContainerField := 'children';
end;

class function TBillboardNode.ClassX3DType: string;
begin
  Result := 'Billboard';
end;

class function TBillboardNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassX3DType) or
    (URN = URNX3DNodes + ClassX3DType);
end;

function TBillboardNode.DirectEnumerateActive(Func: TEnumerateChildrenFunction): Pointer;
begin
  Result := FdChildren.Enumerate(Func);
  if Result <> nil then Exit;
end;

procedure TBillboardNode.ApplyTransform(State: TX3DGraphTraverseState);
var
  NewX, NewY, NewZ, BillboardToViewer, P1, P2, LocalDirection, LocalUp: TVector3Single;
  Angle: Single;
  M, IM: TMatrix4Single;
  PlaneAxis: TVector4Single;
  PlaneAxisDir: TVector3Single absolute PlaneAxis;
begin
  if CameraViewKnown then
  begin
    if PerfectlyZeroVector(FdAxisOfRotation.Value) then
    begin
      LocalDirection := MatrixMultDirection(State.InvertedTransform, FCameraDirection);
      LocalUp        := MatrixMultDirection(State.InvertedTransform, FCameraUp);
      { although FCameraDirection/Up are for sure normalized and orthogonal,
        but State.InvertedTransform may contain scaling,
        so be sure to normalize again the result.
        For safety, also call MakeVectorsOrthoOnTheirPlane. }
      NormalizeVar(LocalDirection);
      NormalizeVar(LocalUp);
      MakeVectorsOrthoOnTheirPlane(LocalDirection, LocalUp);

      NewX := VectorProduct(LocalDirection, LocalUp);
      NewY := LocalUp;
      NewZ := -LocalDirection;

      TransformCoordsMatrices(NewX, NewY, NewZ, M, IM);
      State.Transform := MatrixMult(State.Transform, M);
      State.InvertedTransform := MatrixMult(IM, State.InvertedTransform);
    end else
    begin
      { vector from node origin to CameraPosition, in local coords }
      BillboardToViewer := MatrixMultPoint(State.InvertedTransform, CameraPosition);

      { plane of axisOfRotation }
      PlaneAxisDir := FdAxisOfRotation.Value;
      PlaneAxis[3] := 0;

      { we want to have a rotation that changes UnitVector3Single[2]
        into BillboardToViewer. But the rotation axis is fixed
        to axisOfRotation (we cannot just take their VectorProduct).
        So project both points on a plane orthogonal to axisOfRotation,
        and calculate angle there. }
      P1 := PointOnPlaneClosestToPoint(PlaneAxis, UnitVector3Single[2]);
      P2 := PointOnPlaneClosestToPoint(PlaneAxis, BillboardToViewer);

      if { axisOfRotation paralell to Z axis? Then nothing sensible to do. }
        ZeroVector(P1) or
        { billboard-to-viewer vector parallel to axisOfRotation (includes
          the case when billboard-to-viewer vector is zero in local coords,
          which means that camera standing at Billboard origin)?
          Then nothing sensible to do. }
        ZeroVector(P2) then
        Exit;

      { As https://sourceforge.net/p/castle-engine/tickets/38/ shows,
        the above checks for zero are not enough. The more precise check
        (because it replicates what happens in CosAngleBetweenVectors)
        would be to add

          (VectorLenSqr(P1) * VectorLenSqr(P2) < SingleEqualityEpsilon)

        But that's not really a future-proof solution, to repeat this code.
        It's safer (even if a little slower) to capture exception here. }
      try
        Angle := RotationAngleRadBetweenVectors(P1, P2, FdAxisOfRotation.Value);
      except
        on EVectorInvalidOp do Exit;
      end;

      RotationMatricesRad(Angle, FdAxisOfRotation.Value, M, IM);
      State.Transform := MatrixMult(State.Transform, M);
      State.InvertedTransform := MatrixMult(IM, State.InvertedTransform);
    end;
  end;
end;

procedure TBillboardNode.CameraChanged(
  const APosition, ADirection, AUp: TVector3Single);
begin
  FCameraViewKnown := true;
  FCameraPosition := APosition;
  FCameraDirection := ADirection;
  FCameraUp := AUp;
end;

function TBillboardNode.TransformationChange: TNodeTransformationChange;
begin
  Result := ntcTransform;
end;

{ TCollisionNode ------------------------------------------------------------- }

procedure TCollisionNode.CreateNode;
begin
  inherited;

  FFdEnabled := TSFBool.Create(Self, 'enabled', true);
  { In VRML 2.0, Collision didn't descent from X3DSensorName and had
    special field "collide". In X3D, "enabled" is used for the exact
    same purpose. }
   FdEnabled.AddAlternativeName('collide', 2);
   FdEnabled.ChangesAlways := [chEverything];
  AddField(FFdEnabled);

  FEventCollideTime := TSFTimeEvent.Create(Self, 'collideTime', false);
  AddEvent(FEventCollideTime);

  FEventIsActive := TSFBoolEvent.Create(Self, 'isActive', false);
  AddEvent(FEventIsActive);

  FFdProxy := TSFNode.Create(Self, 'proxy', IAbstractChildNode);
   FdProxy.Exposed := false;
   FdProxy.ChangesAlways := [chEverything];
  AddField(FFdProxy);

  DefaultContainerField := 'children';
end;

class function TCollisionNode.ClassX3DType: string;
begin
  Result := 'Collision';
end;

class function TCollisionNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassX3DType) or
    (URN = URNX3DNodes + ClassX3DType);
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

procedure TCollisionNode.CollideAsBox(const VisibleNode: TX3DNode; const Box: TBox3D);
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
    ProxyTransform.FdChildren.Add(ProxyShape);
  end;
  Proxy := ProxyTransform;

  FdChildren.Clear;
  FdChildren.Add(VisibleNode);
end;

function TCollisionNode.GetProxy: TX3DNode;
begin
  Result := FdProxy.Value;
end;

procedure TCollisionNode.SetProxy(const Value: TX3DNode);
begin
  FdProxy.Send(Value);
end;

{ TAbstractLODNode ----------------------------------------------------------- }

procedure TAbstractLODNode.CreateNode;
begin
  inherited;

  FFdCenter := TSFVec3f.Create(Self, 'center', ZeroVector3Single);
   FdCenter.Exposed := false;
   { Just redisplay, and new appropriate LOD children will be displayed. }
   FdCenter.ChangesAlways := [chRedisplay];
  AddField(FFdCenter);

  FFdRange := TMFFloat.Create(Self, 'range', []);
   FdRange.Exposed := false;
   { Just redisplay, and new appropriate LOD children will be displayed. }
   FdRange.ChangesAlways := [chRedisplay];
  AddField(FFdRange);
  { X3D specification comment: [0,Inf) or -1 }

  FFdForceTransitions := TSFBool.Create(Self, 'forceTransitions', false);
  FFdForceTransitions.Exposed := false;
  AddField(FFdForceTransitions);

  FEventLevel_changed := TSFInt32Event.Create(Self, 'level_changed', false);
  AddEvent(FEventLevel_changed);

  DefaultContainerField := 'children';
end;

class function TAbstractLODNode.ClassX3DType: string;
begin
  Result := 'LOD';
end;

function TAbstractLODNode.DirectEnumerateActive(Func: TEnumerateChildrenFunction): Pointer;
begin
  Result := nil;
  { For now we simply always use the best LOD version,
    avoiding whole issue of choosing proper LOD child. }
  if FdChildren.Items.Count >= 1 then
  begin
    Result := Func(Self, FdChildren[0]);
    if Result <> nil then Exit;
  end;
end;

function TAbstractLODNode.TransformationChange: TNodeTransformationChange;
begin
  Result := ntcLOD;
end;

function TAbstractLODNode.GetCenter: TVector3Single;
begin
  Result := FdCenter.Value;
end;

procedure TAbstractLODNode.SetCenter(const Value: TVector3Single);
begin
  FdCenter.Send(Value);
end;

function TAbstractLODNode.GetForceTransitions: boolean;
begin
  Result := FdForceTransitions.Value;
end;

procedure TAbstractLODNode.SetForceTransitions(const Value: boolean);
begin
  FdForceTransitions.Send(Value);
end;

{ TLODNode_2 ----------------------------------------------------------------- }

procedure TLODNode_2.CreateNode;
begin
  inherited;
  FdChildren.AddAlternativeName('level', 2);
  Eventlevel_changed.AddAlternativeName('levelIndex_changed', 2);
end;

class function TLODNode_2.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassX3DType);
end;

class function TLODNode_2.ForVRMLVersion(const Version: TX3DVersion): boolean;
begin
  Result := Version.Major = 2;
end;

{ TLODNode ------------------------------------------------------------------- }

class function TLODNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassX3DType);
end;

class function TLODNode.ForVRMLVersion(const Version: TX3DVersion): boolean;
begin
  Result := Version.Major >= 3;
end;

{ TNavigationInfoNode -------------------------------------------------------- }

procedure TNavigationInfoNode.CreateNode;
const
  BlendingSortNames: array [TOptionalBlendingSort] of string =
  ('DEFAULT', 'NONE', '2D', '3D');
begin
  inherited;

  FFdAvatarSize := TMFFloat.Create(Self, 'avatarSize', [0.25, 1.6, 0.75]);
   FdAvatarSize.ChangesAlways := [chNavigationInfo];
  AddField(FFdAvatarSize);
  { X3D specification comment: [0,Inf) }

  FFdHeadlight := TSFBool.Create(Self, 'headlight', true);
   FdHeadlight.ChangesAlways := [chHeadLightOn];
  AddField(FFdHeadlight);

  FFdSpeed := TSFFloat.Create(Self, 'speed', 1.0);
   FdSpeed.ChangesAlways := [chNavigationInfo];
  AddField(FFdSpeed);
  { X3D specification comment: [0,Inf) }

  FFdTransitionTime := TSFTime.Create(Self, 'transitionTime', 1.0);
  AddField(FFdTransitionTime);
  { X3D specification comment: [0, Inf) }

  FFdTransitionType := TMFString.Create(Self, 'transitionType', ['LINEAR']);
  AddField(FFdTransitionType);
  { X3D specification comment: ["TELEPORT","LINEAR","ANIMATE",...] }

  { TODO: default value was ["WALK", "ANY"] in VRML 97.
    X3D changed default value. }
  FFdType := TMFString.Create(Self, 'type', ['EXAMINE', 'ANY']);
   FdType.ChangesAlways := [chNavigationInfo];
  AddField(FFdType);
  { X3D specification comment: ["ANY","WALK","EXAMINE","FLY","LOOKAT","NONE",...] }

  FFdVisibilityLimit := TSFFloat.Create(Self, 'visibilityLimit', 0.0);
  AddField(FFdVisibilityLimit);
  { X3D specification comment: [0,Inf) }

  FEventTransitionComplete := TSFBoolEvent.Create(Self, 'transitionComplete', false);
  AddEvent(FEventTransitionComplete);

  FFdBlendingSort := TSFStringEnum.Create(Self, 'blendingSort', BlendingSortNames, Ord(obsDefault));
   FdBlendingSort.ChangesAlways := [];
  AddField(FFdBlendingSort);

  DefaultContainerField := 'children';

  Eventset_bind.OnReceive.Add(@EventSet_BindReceive);
end;

class function TNavigationInfoNode.ClassX3DType: string;
begin
  Result := 'NavigationInfo';
end;

class function TNavigationInfoNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassX3DType) or
    (URN = URNX3DNodes + ClassX3DType);
end;

procedure TNavigationInfoNode.EventSet_BindReceive(
  Event: TX3DEvent; Value: TX3DField; const Time: TX3DTime);
begin
  if Scene <> nil then
    Scene.GetNavigationInfoStack.Set_Bind(Self, (Value as TSFBool).Value);
end;

function TNavigationInfoNode.GetBlendingSort: TOptionalBlendingSort;
begin
  Result := TOptionalBlendingSort(FdBlendingSort.EnumValue);
end;

procedure TNavigationInfoNode.SetBlendingSort(const Value: TOptionalBlendingSort);
begin
  FdBlendingSort.SendEnumValue(Ord(Value));
end;

{ TOrthoViewpointNode -------------------------------------------------------- }

procedure TOrthoViewpointNode.CreateNode;
begin
  inherited;

  FFdFieldOfView := TMFFloat.Create(Self, 'fieldOfView', [-1, -1, 1, 1]);
   FdFieldOfView.ChangesAlways := [chViewpointProjection];
  AddField(FFdFieldOfView);
  { X3D specification comment:  (-Inf,Inf) }

  FFdPosition := TSFVec3f.Create(Self, 'position', Vector3Single(0, 0, 10));
   FdPosition.ChangesAlways := [chViewpointVectors];
  AddField(FFdPosition);
  { X3D specification comment: (-Inf,Inf) }

  DefaultContainerField := 'children';
end;

class function TOrthoViewpointNode.ClassX3DType: string;
begin
  Result := 'OrthoViewpoint';
end;

class function TOrthoViewpointNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassX3DType);
end;

function TOrthoViewpointNode.PositionField: TSFVec3f;
begin
  Result := FdPosition;
end;

class function TOrthoViewpointNode.ProjectionType: TProjectionType;
begin
  Result := ptOrthographic;
end;

function TOrthoViewpointNode.ProjectionMatrix: TMatrix4Single;
var
  OrthoDimensions: TVector4Single;
begin
  { default OrthoDimensions, for OrthoViewpoint }
  OrthoDimensions[0] := -1;
  OrthoDimensions[1] := -1;
  OrthoDimensions[2] :=  1;
  OrthoDimensions[3] :=  1;

  if FdFieldOfView.Items.Count > 0 then OrthoDimensions[0] := FdFieldOfView.Items[0];
  if FdFieldOfView.Items.Count > 1 then OrthoDimensions[1] := FdFieldOfView.Items[1];
  if FdFieldOfView.Items.Count > 2 then OrthoDimensions[2] := FdFieldOfView.Items[2];
  if FdFieldOfView.Items.Count > 3 then OrthoDimensions[3] := FdFieldOfView.Items[3];

  { TODO: for currently bound viewpoint, we should honour
    fieldOfView and aspect ratio of current window,
    by calling AspectFieldOfView. }

  Result := OrthoProjMatrix(
    { Beware: order of OrthoViewpoint.fieldOfView and OrthoDimensions
      is different than typical OpenGL and our OrthoProjMatrix params. }
    OrthoDimensions[0],
    OrthoDimensions[2],
    OrthoDimensions[1],
    OrthoDimensions[3],
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

class procedure TOrthoViewpointNode.AspectFieldOfView(
  var AFieldOfView: TVector4Single; const WindowWidthToHeight: Single);

  { Scale the extent. Since AspectFieldOfView should only make AFieldOfView
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
      if Log then
        WritelnLog('OrthoViewpoint', 'OrthoViewpoint.fieldOfView max extent smaller than min extent');
      Exit;
    end;

    Min := Middle - Scale * L / 2;
    Max := Middle + Scale * L / 2;
  end;

var
  FOVAspect: Single;
begin
  if FloatsEqual(AFieldOfView[2], AFieldOfView[0]) or
     FloatsEqual(AFieldOfView[3], AFieldOfView[1]) then
  begin
    if Log then
      WritelnLog('OrthoViewpoint', 'OrthoViewpoint.fieldOfView extent (max-min) is zero');
    Exit;
  end;

  FOVAspect :=
    (AFieldOfView[2] - AFieldOfView[0]) /
    (AFieldOfView[3] - AFieldOfView[1]);

  { The idea is to change FieldOfView, such that at the end the above
    equation would calculate FOVAspect as equal to WindowWidthToHeight.

    To do this, multiply above equation by WindowWidthToHeight / FOVAspect.
    We have to transform put this scale into either horizontal or vertical
    extent, since we only want to make FieldOfView larger (never smaller). }

  if FOVAspect > WindowWidthToHeight then
  begin
    ScaleExtent(FOVAspect / WindowWidthToHeight, AFieldOfView[1], AFieldOfView[3]);
  end else
  if FOVAspect < WindowWidthToHeight then
  begin
    ScaleExtent(WindowWidthToHeight / FOVAspect, AFieldOfView[0], AFieldOfView[2]);
  end;
end;

{ TViewpointNode ------------------------------------------------------------- }

procedure TViewpointNode.CreateNode;
begin
  inherited;

  FFdFieldOfView := TSFFloat.Create(Self, 'fieldOfView', DefaultViewpointFieldOfView);
   FdFieldOfView.ChangesAlways := [chViewpointProjection];
  AddField(FFdFieldOfView);
  { X3D specification comment: (0,Pi) }

  FFdPosition := TSFVec3f.Create(Self, 'position', Vector3Single(0, 0, 10));
   FdPosition.ChangesAlways := [chViewpointVectors];
  AddField(FFdPosition);
  { X3D specification comment: (-Inf,Inf) }

  DefaultContainerField := 'children';
end;

class function TViewpointNode.ClassX3DType: string;
begin
  Result := 'Viewpoint';
end;

class function TViewpointNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassX3DType) or
    (URN = URNX3DNodes + ClassX3DType);
end;

function TViewpointNode.PositionField: TSFVec3f;
begin
  Result := FdPosition;
end;

class function TViewpointNode.ProjectionType: TProjectionType;
begin
  Result := ptPerspective;
end;

function TViewpointNode.AngleOfView(
  const ThisToOtherSizeRatio: Single): Single;
begin
  Result := ViewpointAngleOfView(FdFieldOfView.Value, ThisToOtherSizeRatio);
end;

class function TViewpointNode.ViewpointAngleOfView(
  FieldOfView: Single;
  const ThisToOtherSizeRatio: Single): Single;
var
  OtherAngle: Single;
begin
  ClampVar(FieldOfView, 0.01, Pi - 0.01);

  if ThisToOtherSizeRatio < 1 then
  begin
    { So the resulting angle is the smaller one. }
    Result := FieldOfView;
    OtherAngle :=
      AdjustViewAngleRadToAspectRatio(Result, 1 / ThisToOtherSizeRatio);
    if OtherAngle > Pi then
      Result := AdjustViewAngleRadToAspectRatio(Pi, ThisToOtherSizeRatio);
  end else
  begin
    { So the resulting angle is the larger one. }
    OtherAngle := FieldOfView;
    Result :=
      AdjustViewAngleRadToAspectRatio(OtherAngle, ThisToOtherSizeRatio);
    if Result > Pi then
      Result := Pi;
  end;
end;

function TViewpointNode.ProjectionMatrix: TMatrix4Single;
begin
  { TODO: for currently bound viewpoint, we should honour
    fieldOfView and aspect ratio of current window? }
  Result := PerspectiveProjMatrixRad(
    FdFieldOfView.Value, 1,
    1, 100); { TODO: near, far projection testing values }
end;

{ TViewpointGroupNode -------------------------------------------------------- }

procedure TViewpointGroupNode.CreateNode;
begin
  inherited;

  FFdCenter := TSFVec3f.Create(Self, 'center', Vector3Single(0, 0, 0));
  AddField(FFdCenter);
  { X3D specification comment: (-Inf,Inf) }

  FFdChildren := TMFNode.Create(Self, 'children', [TAbstractX3DViewpointNode, TViewpointGroupNode]);
  AddField(FFdChildren);

  FFdDescription := TSFString.Create(Self, 'description', '');
  AddField(FFdDescription);

  FFdDisplayed := TSFBool.Create(Self, 'displayed', true);
  AddField(FFdDisplayed);

  FFdRetainUserOffsets := TSFBool.Create(Self, 'retainUserOffsets', false);
  AddField(FFdRetainUserOffsets);

  FFdSize := TSFVec3f.Create(Self, 'size', Vector3Single(0, 0, 0));
  AddField(FFdSize);
  { X3D specification comment: (-Inf,Inf) }

  DefaultContainerField := 'children';
end;

class function TViewpointGroupNode.ClassX3DType: string;
begin
  Result := 'ViewpointGroup';
end;

class function TViewpointGroupNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassX3DType);
end;

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
    TLODNode_2,
    TLODNode,
    TNavigationInfoNode,
    TOrthoViewpointNode,
    TViewpointNode,
    TViewpointGroupNode
  ]);
end;
{$endif read_implementation}
