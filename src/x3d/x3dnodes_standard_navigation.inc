{
  Copyright 2002-2018 Michalis Kamburelis.

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
    function GetPosition: TVector3;
    procedure SetPosition(const Value: TVector3);
    function GetOrientation: TVector4;
    procedure SetOrientation(const Value: TVector4);
  strict protected
    function PositionField: TSFVec3f; virtual; abstract;
  public
    procedure CreateNode; override;
    function TransformationChange: TNodeTransformationChange; override;

    strict private FFdOrientation: TSFRotation;
    public property FdOrientation: TSFRotation read FFdOrientation;
    property Orientation: TVector4 read GetOrientation write SetOrientation;

    strict private FFdDirection: TMFVec3f;
    public property FdDirection: TMFVec3f read FFdDirection;

    strict private FFdUp: TMFVec3f;
    public property FdUp: TMFVec3f read FFdUp;

    strict private FFdGravityUp: TSFVec3f;
    public property FdGravityUp: TSFVec3f read FFdGravityUp;

    strict private FEventCameraMatrix: TSFMatrix4fEvent;
    public property EventCameraMatrix: TSFMatrix4fEvent read FEventCameraMatrix;

    strict private FEventCameraInverseMatrix: TSFMatrix4fEvent;
    public property EventCameraInverseMatrix: TSFMatrix4fEvent read FEventCameraInverseMatrix;

    strict private FEventCameraRotationMatrix: TSFMatrix3fEvent;
    public property EventCameraRotationMatrix: TSFMatrix3fEvent read FEventCameraRotationMatrix;

    strict private FEventCameraRotationInverseMatrix: TSFMatrix3fEvent;
    public property EventCameraRotationInverseMatrix: TSFMatrix3fEvent read FEventCameraRotationInverseMatrix;

    strict private FFdCameraMatrixSendAlsoOnOffscreenRendering: TSFBool;
    public property FdCameraMatrixSendAlsoOnOffscreenRendering: TSFBool read FFdCameraMatrixSendAlsoOnOffscreenRendering;

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
      (determined by parent nodes like Transform).

      One conclusion from the above is that the only way to change the gravity up
      vector (this determines in which direction viewer falls down)
      is to use the Transform node around the viewpoint node.

      Additionally, as an extension, we also look at FdDirection and FdUp
      and FdGravityUp vectors. See
      https://castle-engine.io/x3d_extensions.php#section_ext_cameras_alt_orient

      Returned CamDir, CamUp, GravityUp are @italic(always normalized). }
    procedure GetView(out CamPos, CamDir, CamUp, GravityUp: TVector3);

    strict private FFdDescription: TSFString;
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

    strict private FFdCenterOfRotation: TSFVec3f;
    public property FdCenterOfRotation: TSFVec3f read FFdCenterOfRotation;

    strict private FFdJump: TSFBool;
    public property FdJump: TSFBool read FFdJump;

    strict private FFdRetainUserOffsets: TSFBool;
    public property FdRetainUserOffsets: TSFBool read FFdRetainUserOffsets;

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

  TCameraVectors = record
    Position, Direction, Up: TVector3;
  end;

  { Grouping node that transforms the coordinate system of its children
    so that they always turn towards the viewer. }
  TBillboardNode = class(TAbstractX3DGroupingNode, ITransformNode)
  strict private
    FCameraVectors: TCameraVectors;
    FCameraVectorsKnown: boolean;
  protected
    function DirectEnumerateActive(Func: TEnumerateChildrenFunction): Pointer; override;
    procedure ApplyTransform(State: TX3DGraphTraverseState); override;
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
    class function URNMatching(const URN: string): boolean; override;
    function TransformationChange: TNodeTransformationChange; override;

    procedure CameraChanged(const ACameraVectors: TCameraVectors);

    strict private FFdAxisOfRotation: TSFVec3f;
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
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
    class function URNMatching(const URN: string): boolean; override;

    strict private FFdEnabled: TSFBool;
    public property FdEnabled: TSFBool read FFdEnabled;

    { Event out } { }
    strict private FEventCollideTime: TSFTimeEvent;
    public property EventCollideTime: TSFTimeEvent read FEventCollideTime;

    { Event out } { }
    strict private FEventIsActive: TSFBoolEvent;
    public property EventIsActive: TSFBoolEvent read FEventIsActive;

    strict private FFdProxy: TSFNode;
    public property FdProxy: TSFNode read FFdProxy;

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
    function GetCenter: TVector3;
    procedure SetCenter(const Value: TVector3);
    function GetForceTransitions: boolean;
    procedure SetForceTransitions(const Value: boolean);
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;

    strict private FFdCenter: TSFVec3f;
    public property FdCenter: TSFVec3f read FFdCenter;
    property Center: TVector3 read GetCenter write SetCenter;

    strict private FFdRange: TMFFloat;
    public property FdRange: TMFFloat read FFdRange;

    strict private FFdForceTransitions: TSFBool;
    public property FdForceTransitions: TSFBool read FFdForceTransitions;
    property ForceTransitions: boolean read GetForceTransitions write SetForceTransitions;

    { Event out } { }
    strict private FEventLevel_changed: TSFInt32Event;
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
  strict private
    procedure EventSet_BindReceive(
      Event: TX3DEvent; Value: TX3DField; const Time: TX3DTime);
    function GetBlendingSort: TOptionalBlendingSort;
    procedure SetBlendingSort(const Value: TOptionalBlendingSort);
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
    class function URNMatching(const URN: string): boolean; override;

    strict private FFdAvatarSize: TMFFloat;
    public property FdAvatarSize: TMFFloat read FFdAvatarSize;

    strict private FFdHeadlight: TSFBool;
    public property FdHeadlight: TSFBool read FFdHeadlight;

    strict private FFdSpeed: TSFFloat;
    public property FdSpeed: TSFFloat read FFdSpeed;

    strict private FFdTransitionTime: TSFTime;
    public property FdTransitionTime: TSFTime read FFdTransitionTime;

    strict private FFdTransitionType: TMFString;
    public property FdTransitionType: TMFString read FFdTransitionType;

    strict private FFdType: TMFString;
    public property FdType: TMFString read FFdType;

    strict private FFdVisibilityLimit: TSFFloat;
    public property FdVisibilityLimit: TSFFloat read FFdVisibilityLimit;

    strict private FFdBlendingSort: TSFStringEnum;
    public property FdBlendingSort: TSFStringEnum read FFdBlendingSort;
    property BlendingSort: TOptionalBlendingSort
      read GetBlendingSort write SetBlendingSort;

    { Event out } { }
    strict private FEventTransitionComplete: TSFBoolEvent;
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

    strict private FFdFieldOfView: TMFFloat;
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

    strict private FFdPosition: TSFVec3f;
    public property FdPosition: TSFVec3f read FFdPosition;

    class function ProjectionType: TProjectionType; override;
    function ProjectionMatrix: TMatrix4; override;

    { Fix given field of view value for window aspect ratio.
      The idea is that OrthoViewpoint.fieldOfView specifies the minimal
      extents. Depending on your window aspect ratio, you may need to make
      one extent (vertical or horizontal) larger to adjust. }
    class procedure AspectFieldOfView(
      var AFieldOfView: TFloatRectangle;
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

    strict private FFdFieldOfView: TSFFloat;
    public property FdFieldOfView: TSFFloat read FFdFieldOfView;

    strict private FFdFieldOfViewForceVertical: TSFBool;
    public property FdFieldOfViewForceVertical: TSFBool read FFdFieldOfViewForceVertical;

    strict private FFdPosition: TSFVec3f;
    public property FdPosition: TSFVec3f read FFdPosition;

    class function ProjectionType: TProjectionType; override;

    { Calculate proper angle of view for typical rectangular
      display, based on given fieldOfView field value.
      Result is in radians (just like fieldOfView X3D field).

      If you want to calculate horizontal angle of view then
      pass as ThisToOtherSizeRatio your window's width / height.
      If you want to calculate vertical angle of view then
      pass as ThisToOtherSizeRatio your window's height / width.
      For this method it doesn't really matter which is horizontal
      and which is vertical, both are treated the same.

      This works following X3D spec. So the angle of view for
      smaller window size is set to fieldOfView. The other angle
      can always be calculated by AdjustViewAngleRadToAspectRatio
      (this implements the same equation that is mentioned in X3D spec).
      The larger angle cannot be larger than Pi, and may force the
      smaller angle to be smaller than fieldOfView. }
    function AngleOfView(const ThisToOtherSizeRatio: Single): Single;
      deprecated 'use only ViewpointAngleOfView for calculating angles';

    { Like AngleOfView, but allows to specify FieldOfView as a parameter. }
    class function ViewpointAngleOfView(
      FieldOfView: Single;
      const ThisToOtherSizeRatio: Single): Single;
      deprecated 'use only ViewpointAngleOfView for calculating angles';

    { Calculate horizontal and vertical angle of view,
      assuming given value of FieldOfView and FieldOfViewForceVertical fields,
      and assuming given viewport size.

      The result is in radians, like the input FieldOfView. }
    class function ViewpointAngleOfView(
      const FieldOfView: Single; const FieldOfViewForceVertical: boolean;
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
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
    class function URNMatching(const URN: string): boolean; override;

    strict private FFdCenter: TSFVec3f;
    public property FdCenter: TSFVec3f read FFdCenter;

    strict private FFdChildren: TMFNode;
    public property FdChildren: TMFNode read FFdChildren;

    strict private FFdDescription: TSFString;
    public property FdDescription: TSFString read FFdDescription;

    strict private FFdDisplayed: TSFBool;
    public property FdDisplayed: TSFBool read FFdDisplayed;

    strict private FFdRetainUserOffsets: TSFBool;
    public property FdRetainUserOffsets: TSFBool read FFdRetainUserOffsets;

    strict private FFdSize: TSFVec3f;
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
  FFdOrientation := TSFRotation.Create(Self, true, 'orientation', Vector3(0, 0, 1), 0);
   FdOrientation.ChangesAlways := [chViewpointVectors];
  AddField(FFdOrientation);
  { X3D specification comment: [-1,1],(-Inf,Inf) }

  FFdDirection := TMFVec3f.Create(Self, true, 'direction', []);
   FdDirection.ChangesAlways := [chViewpointVectors];
  AddField(FFdDirection);

  FFdUp := TMFVec3f.Create(Self, true, 'up', []);
   FdUp.ChangesAlways := [chViewpointVectors];
  AddField(FFdUp);

  FFdGravityUp := TSFVec3f.Create(Self, true, 'gravityUp', DefaultX3DGravityUp);
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

  FFdCameraMatrixSendAlsoOnOffscreenRendering := TSFBool.Create(Self, true, 'cameraMatrixSendAlsoOnOffscreenRendering', false);
  AddField(FFdCameraMatrixSendAlsoOnOffscreenRendering);

  FFdDescription := TSFString.Create(Self, true, 'description', '');
  AddField(FFdDescription);

  Eventset_bind.AddNotification({$ifdef CASTLE_OBJFPC}@{$endif} EventSet_BindReceive);
end;

procedure TAbstractViewpointNode.GetView(
  out CamPos, CamDir, CamUp, GravityUp: TVector3);
begin
  CamPos := Position;

  if FdDirection.Items.Count > 0 then
  begin
    CamDir := FdDirection.Items.List^[0];
    if CamDir.IsZero then
    begin
      WritelnWarning('VRML/X3D', 'Viewpoint "direction" must not be zero, assuming defaults');
      CamDir := FdOrientation.RotatedPoint( DefaultX3DCameraDirection );
    end;
  end else
    CamDir := FdOrientation.RotatedPoint( DefaultX3DCameraDirection );

  if FdUp.Items.Count > 0 then
  begin
    CamUp := FdUp.Items.List^[0];
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

  { Niestety, macierz ponizej moze cos skalowac wiec nawet jesli powyzej
    uzylismy FdOrientation.RotatedPoint( DefaultX3DCameraDirection/Up ) i wiemy ze CamDir/Up
    jest znormalizowane - to i tak musimy je tutaj znormalizowac.
    TODO: byloby dobrze uzyc tutaj czegos jak Transform.MultDirectionNoScale }
  CamPos    := Transform.MultPoint(CamPos);
  CamDir    := Transform.MultDirection(CamDir).Normalize;
  CamUp     := Transform.MultDirection(CamUp).Normalize;
  GravityUp := Transform.MultDirection(GravityUp).Normalize;

  Assert(SameValue(CamDir.LengthSqr, 1.0, 0.0001));
  Assert(SameValue(CamUp.LengthSqr, 1.0, 0.0001));
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

function TAbstractViewpointNode.GetPosition: TVector3;
begin
  Result := PositionField.Value;
end;

procedure TAbstractViewpointNode.SetPosition(const Value: TVector3);
begin
  PositionField.Send(Value);
end;

function TAbstractViewpointNode.GetOrientation: TVector4;
begin
  Result := FdOrientation.Value;
end;

procedure TAbstractViewpointNode.SetOrientation(const Value: TVector4);
begin
  FdOrientation.Send(Value);
end;

procedure TAbstractX3DViewpointNode.CreateNode;
begin
  inherited;

  FFdCenterOfRotation := TSFVec3f.Create(Self, true, 'centerOfRotation', Vector3(0, 0, 0));
  AddField(FFdCenterOfRotation);
  { X3D specification comment: (-Inf,Inf) }

  FFdJump := TSFBool.Create(Self, true, 'jump', true);
  AddField(FFdJump);

  FFdRetainUserOffsets := TSFBool.Create(Self, true, 'retainUserOffsets', false);
  AddField(FFdRetainUserOffsets);

  DefaultContainerField := 'children';
end;

function TAbstractX3DViewpointNode.ProjectionMatrix: TMatrix4;
begin
  Result := TMatrix4.Identity;
end;

function TAbstractX3DViewpointNode.ModelviewMatrix: TMatrix4;
var
  CamPos, CamDir, CamUp, CamGravityUp: TVector3;
begin
  GetView(CamPos, CamDir, CamUp, CamGravityUp);
  Result := LookDirMatrix(CamPos, CamDir, CamUp);
end;

function TAbstractX3DViewpointNode.GetProjectorMatrix: TMatrix4;
begin
  Result := ProjectionMatrix * ModelviewMatrix;
end;

{ TBillboardNode ------------------------------------------------------------- }

procedure TBillboardNode.CreateNode;
begin
  inherited;

  FFdAxisOfRotation := TSFVec3f.Create(Self, true, 'axisOfRotation', Vector3(0, 1, 0));
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
  NewX, NewY, NewZ, BillboardToViewer, P1, P2, LocalDirection, LocalUp: TVector3;
  Angle: Single;
  M, IM: TMatrix4;
  PlaneAxis: TVector4;
  PlaneAxisDir: TVector3 absolute PlaneAxis;
begin
  if FCameraVectorsKnown then
  begin
    if FdAxisOfRotation.Value.IsPerfectlyZero then
    begin
      LocalDirection := State.InvertedTransform.MultDirection(FCameraVectors.Direction);
      LocalUp        := State.InvertedTransform.MultDirection(FCameraVectors.Up);
      { although FCameraVectors.Direction/Up are for sure normalized and orthogonal,
        but State.InvertedTransform may contain scaling,
        so be sure to normalize again the result.
        For safety, also call MakeVectorsOrthoOnTheirPlane. }
      LocalDirection.NormalizeMe;
      LocalUp.NormalizeMe;
      MakeVectorsOrthoOnTheirPlane(LocalDirection, LocalUp);

      NewX := TVector3.CrossProduct(LocalDirection, LocalUp);
      NewY := LocalUp;
      NewZ := -LocalDirection;

      TransformCoordsMatrices(NewX, NewY, NewZ, M, IM);
      State.Transform := State.Transform * M;
      State.InvertedTransform := IM * State.InvertedTransform;
    end else
    begin
      { vector from node origin to FCameraVectors.Position, in local coords }
      BillboardToViewer := State.InvertedTransform.MultPoint(FCameraVectors.Position);

      { plane of axisOfRotation }
      PlaneAxisDir := FdAxisOfRotation.Value;
      PlaneAxis[3] := 0;

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
        Angle := RotationAngleRadBetweenVectors(P1, P2, FdAxisOfRotation.Value);
      except
        on EVectorInvalidOp do Exit;
      end;

      RotationMatricesRad(Angle, FdAxisOfRotation.Value, M, IM);
      State.Transform := State.Transform * M;
      State.InvertedTransform := IM * State.InvertedTransform;
    end;
  end;
end;

procedure TBillboardNode.CameraChanged(const ACameraVectors: TCameraVectors);
begin
  FCameraVectorsKnown := true;
  FCameraVectors := ACameraVectors;
end;

function TBillboardNode.TransformationChange: TNodeTransformationChange;
begin
  Result := ntcTransform;
end;

{ TCollisionNode ------------------------------------------------------------- }

procedure TCollisionNode.CreateNode;
begin
  inherited;

  FFdEnabled := TSFBool.Create(Self, true, 'enabled', true);
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

  FFdProxy := TSFNode.Create(Self, false, 'proxy', IAbstractChildNode);
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
    ProxyTransform.AddChildren(ProxyShape);
  end;
  Proxy := ProxyTransform;

  FdChildren.Clear;
  FdChildren.Add(VisibleNode);
end;

{ TAbstractLODNode ----------------------------------------------------------- }

procedure TAbstractLODNode.CreateNode;
begin
  inherited;

  FFdCenter := TSFVec3f.Create(Self, false, 'center', TVector3.Zero);
   { Just redisplay, and new appropriate LOD children will be displayed. }
   FdCenter.ChangesAlways := [chRedisplay];
  AddField(FFdCenter);

  FFdRange := TMFFloat.Create(Self, false, 'range', []);
   { Just redisplay, and new appropriate LOD children will be displayed. }
   FdRange.ChangesAlways := [chRedisplay];
  AddField(FFdRange);
  { X3D specification comment: [0,Inf) or -1 }

  FFdForceTransitions := TSFBool.Create(Self, false, 'forceTransitions', false);
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
  if FdChildren.Count >= 1 then
  begin
    Result := Func(Self, FdChildren[0]);
    if Result <> nil then Exit;
  end;
end;

function TAbstractLODNode.TransformationChange: TNodeTransformationChange;
begin
  Result := ntcLOD;
end;

function TAbstractLODNode.GetCenter: TVector3;
begin
  Result := FdCenter.Value;
end;

procedure TAbstractLODNode.SetCenter(const Value: TVector3);
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

  FFdAvatarSize := TMFFloat.Create(Self, true, 'avatarSize', [0.25, 1.6, 0.75]);
   FdAvatarSize.ChangesAlways := [chNavigationInfo];
  AddField(FFdAvatarSize);
  { X3D specification comment: [0,Inf) }

  FFdHeadlight := TSFBool.Create(Self, true, 'headlight', true);
   FdHeadlight.ChangesAlways := [chHeadLightOn];
  AddField(FFdHeadlight);

  FFdSpeed := TSFFloat.Create(Self, true, 'speed', 1.0);
   FdSpeed.ChangesAlways := [chNavigationInfo];
  AddField(FFdSpeed);
  { X3D specification comment: [0,Inf) }

  FFdTransitionTime := TSFTime.Create(Self, true, 'transitionTime', 1.0);
  AddField(FFdTransitionTime);
  { X3D specification comment: [0, Inf) }

  FFdTransitionType := TMFString.Create(Self, true, 'transitionType', ['LINEAR']);
  AddField(FFdTransitionType);
  { X3D specification comment: ["TELEPORT","LINEAR","ANIMATE",...] }

  { TODO: default value was ["WALK", "ANY"] in VRML 97.
    X3D changed default value. }
  FFdType := TMFString.Create(Self, true, 'type', ['EXAMINE', 'ANY']);
   FdType.ChangesAlways := [chNavigationInfo];
  AddField(FFdType);
  { X3D specification comment: ["ANY","WALK","EXAMINE","FLY","LOOKAT","NONE",...] }

  FFdVisibilityLimit := TSFFloat.Create(Self, true, 'visibilityLimit', 0.0);
  AddField(FFdVisibilityLimit);
  { X3D specification comment: [0,Inf) }

  FEventTransitionComplete := TSFBoolEvent.Create(Self, 'transitionComplete', false);
  AddEvent(FEventTransitionComplete);

  FFdBlendingSort := TSFStringEnum.Create(Self, true, 'blendingSort', BlendingSortNames, Ord(obsDefault));
   FdBlendingSort.ChangesAlways := [];
  AddField(FFdBlendingSort);

  DefaultContainerField := 'children';

  Eventset_bind.AddNotification({$ifdef CASTLE_OBJFPC}@{$endif} EventSet_BindReceive);
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

  FFdFieldOfView := TMFFloat.Create(Self, true, 'fieldOfView', [-1, -1, 1, 1]);
   FdFieldOfView.ChangesAlways := [chViewpointProjection];
  AddField(FFdFieldOfView);
  { X3D specification comment:  (-Inf,Inf) }

  FFdPosition := TSFVec3f.Create(Self, true, 'position', Vector3(0, 0, 10));
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
    by calling AspectFieldOfView. }

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

class procedure TOrthoViewpointNode.AspectFieldOfView(
  var AFieldOfView: TFloatRectangle; const WindowWidthToHeight: Single);

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
      WritelnLog('OrthoViewpoint', 'OrthoViewpoint.fieldOfView max extent smaller than min extent');
      Exit;
    end;

    Min := Middle - Scale * L / 2;
    Max := Middle + Scale * L / 2;
  end;

var
  FOVAspect: Single;
  Min, Max: Single;
begin
  if (AFieldOfView.Width <= 0) or
     (AFieldOfView.Height <= 0) then
  begin
    WritelnLog('OrthoViewpoint', 'OrthoViewpoint.fieldOfView extent (max-min) is zero');
    Exit;
  end;

  FOVAspect := AFieldOfView.Width / AFieldOfView.Height;

  { The idea is to change FieldOfView, such that at the end the above
    equation would calculate FOVAspect as equal to WindowWidthToHeight.

    To do this, multiply above equation by WindowWidthToHeight / FOVAspect.
    We have to transform put this scale into either horizontal or vertical
    extent, since we only want to make FieldOfView larger (never smaller). }

  if FOVAspect > WindowWidthToHeight then
  begin
    Min := AFieldOfView.Bottom;
    Max := AFieldOfView.Top;
    ScaleExtent(FOVAspect / WindowWidthToHeight, Min, Max);
    AFieldOfView.Bottom := Min;
    AFieldOfView.Height := Max - Min;
  end else
  if FOVAspect < WindowWidthToHeight then
  begin
    Min := AFieldOfView.Left;
    Max := AFieldOfView.Right;
    ScaleExtent(WindowWidthToHeight / FOVAspect, Min, Max);
    AFieldOfView.Left := Min;
    AFieldOfView.Width := Max - Min;
  end;
end;

{ TViewpointNode ------------------------------------------------------------- }

procedure TViewpointNode.CreateNode;
begin
  inherited;

  FFdFieldOfView := TSFFloat.Create(Self, true, 'fieldOfView', DefaultViewpointFieldOfView);
   FdFieldOfView.ChangesAlways := [chViewpointProjection];
  AddField(FFdFieldOfView);
  { X3D specification comment: (0,Pi) }

  FFdFieldOfViewForceVertical := TSFBool.Create(Self, true, 'fieldOfViewForceVertical', false);
   FdFieldOfViewForceVertical.ChangesAlways := [chViewpointProjection];
  AddField(FdFieldOfViewForceVertical);

  FFdPosition := TSFVec3f.Create(Self, true, 'position', Vector3(0, 0, 10));
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

class function TViewpointNode.ViewpointAngleOfView(
  const FieldOfView: Single; const FieldOfViewForceVertical: boolean;
  const ViewportWidth, ViewportHeight: Single): TVector2;
begin
  if FieldOfViewForceVertical then
  begin
    Result[1] := FieldOfView;
    Result[0] := AdjustViewAngleRadToAspectRatio(
      Result[1], ViewportWidth / ViewportHeight);
  end else
  begin
    {$warnings off} // knowingly using deprecated in deprecated, in the future it should be private
    Result[0] := ViewpointAngleOfView(
      FieldOfView, ViewportWidth / ViewportHeight);
    {$warnings on}
    Result[1] := AdjustViewAngleRadToAspectRatio(
      Result[0], ViewportHeight / ViewportWidth);
  end;
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

function TViewpointNode.AngleOfView(
  const ThisToOtherSizeRatio: Single): Single;
begin
  {$warnings off} // knowingly using deprecated in deprecated, in the future these both should be private
  Result := ViewpointAngleOfView(FdFieldOfView.Value, ThisToOtherSizeRatio);
  {$warnings on}
end;

function TViewpointNode.ProjectionMatrix: TMatrix4;
begin
  { TODO: for currently bound viewpoint, we should honour
    fieldOfView and aspect ratio of current window? }
  Result := PerspectiveProjectionMatrixRad(
    FdFieldOfView.Value, 1,
    1, 100); { TODO: near, far projection testing values }
end;

{ TViewpointGroupNode -------------------------------------------------------- }

procedure TViewpointGroupNode.CreateNode;
begin
  inherited;

  FFdCenter := TSFVec3f.Create(Self, true, 'center', Vector3(0, 0, 0));
  AddField(FFdCenter);
  { X3D specification comment: (-Inf,Inf) }

  FFdChildren := TMFNode.Create(Self, true, 'children', [TAbstractX3DViewpointNode, TViewpointGroupNode]);
  AddField(FFdChildren);

  FFdDescription := TSFString.Create(Self, true, 'description', '');
  AddField(FFdDescription);

  FFdDisplayed := TSFBool.Create(Self, true, 'displayed', true);
  AddField(FFdDisplayed);

  FFdRetainUserOffsets := TSFBool.Create(Self, true, 'retainUserOffsets', false);
  AddField(FFdRetainUserOffsets);

  FFdSize := TSFVec3f.Create(Self, true, 'size', Vector3(0, 0, 0));
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
