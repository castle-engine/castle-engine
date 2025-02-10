{
  Copyright 2020-2024 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Select, manipulate (move, rotate, scale) and visualize hovering over
  TCastleTransform instances.
  Use this unit to implement 3D editing operations in your application. }
unit CastleTransformManipulate;

{$I castleconf.inc}

interface

uses Classes, SysUtils, Contnrs, Generics.Collections,
  CastleColors, CastleVectors, CastleVectorsInternalSingle, CastleTransform,
  CastleDebugTransform, CastleScene, CastleCameras, CastleTriangles, CastleUtils,
  CastleClassUtils;

type
  { Visualize the TCastleTransform we hover over
    (to visualize what would be selected by clicking). }
  TCastleTransformHover = class(TComponent)
  strict private
    FCurrent: TCastleTransform;
    FCurrentObserver: TFreeNotificationObserver;
    // This entire class is mostly a wrapper around this TDebugTransformBox
    Box: TDebugTransformBox;
    procedure CurrentFreeNotification(const Sender: TFreeNotificationObserver);
    procedure SetCurrent(const AValue: TCastleTransform);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    { Current TCastleTransform instance that "we are hovering over it,
      and clicking will select it". }
    property Current: TCastleTransform read FCurrent write SetCurrent;
  end;

  { Mode of operation for @link(TCastleTransformManipulate.Mode). }
  TManipulateMode = (mmSelect, mmTranslate, mmRotate, mmScale);

  { Allow to select, move, rotate, scale a group of TCastleTransform instances.

    This component implements common 3D editing operations:

    @unorderedList(
      @item(Selection (when @link(Mode) = mmSelect).
        To use this, call @link(SetSelected) with any number of transforms.
        Multiple transform instances can be selected at once.

        This component does never itself change the selected objects.
        It relies on external code to call @link(SetSelected) whenever
        the selection changes.

        This component merely visualizes the selected objects.
      )

      @item(Move, rotate or scale a selected transformation.
        To use this, call @link(SetSelected) with exactly one transform.
        And set @link(Mode) to the desired operation, like @link(mmTranslate).

        This component fully implements moving, rotating and scaling.
        It displays a proper "gizmo" to perform this for user,
        and it modifies the selected transformation when user drags the gizmo.
        Use events like @link(OnTransformModified) and @link(OnTransformModifyEnd)
        to get notified when the transformation is modified.

        TODO: Right now we allow to move/rotate/scale only a single transformation.
        In the future, we plan to allow to move/rotate/scale multiple transforms.
      )
    )
  }
  TCastleTransformManipulate = class(TComponent)
  strict private
    type
      TGizmoScene = class(TCastleScene)
      strict private
        {.$define DEBUG_GIZMO_PICK}
        {$ifdef DEBUG_GIZMO_PICK}
        VisualizePick: TCastleScene;
        {$endif DEBUG_GIZMO_PICK}
        GizmoDragging: Boolean;
        DraggingCoord: Integer;
        LastPick: TVector3;
        LastPickAngle: Single;
        GizmoScalingAssumeScale: Boolean;
        GizmoScalingAssumeScaleValue: TVector3;
        InsideInternalCameraChanged: Boolean;
        LastIsProjectionOrthographic: Boolean;

        { Point on axis closest to given pick.
          Axis may be -1 to indicate we drag on all axes with the same amount
          or -2 to indicate we drag X and Y axes for 2D. }
        function PointOnAxis(out Intersection: TVector3;
          const Pick: TRayCollisionNode; const Axis: Integer): Boolean;

        { Angle in radians on a plane lying at given Coord
          (e.g. plane "Z = 0" when Coord = 2).
          The angle is measured using ArcTan2 on the given plane.
          Angle is in radians, from -Pi to Pi.

          It is not defined here where's the Angle = 0 exactly, as users
          of this routine in practice always want to subtract 2 values of such
          angle, so it doesn't matter "where is Angle = 0". }
        function AngleOnPlane(out Angle: Single;
          const Pick: TRayCollisionNode; const Coord: T3DAxis): Boolean;

        procedure DoTransformModified;
        procedure DoTransformModifyEnd;
        procedure UpdateSize;
      protected
        procedure ChangeWorld(const Value: TCastleAbstractRootTransform); override;
        function LocalRayCollision(const RayOrigin, RayDirection: TVector3;
          const TrianglesToIgnoreFunc: TTriangleIgnoreFunc): TRayCollision; override;
      public
        Mode: TManipulateMode;
        OnTransformModified: TNotifyEvent;
        OnTransformModifyEnd: TNotifyEvent;
        constructor Create(AOwner: TComponent); override;
        procedure InternalCameraChanged; override;
        function PointingDevicePress(const Pick: TRayCollisionNode;
          const Distance: Single): Boolean; override;
        function PointingDeviceMove(const Pick: TRayCollisionNode;
          const Distance: Single): Boolean; override;
        function PointingDeviceRelease(const Pick: TRayCollisionNode;
          const Distance: Single; const CancelAction: Boolean): Boolean; override;
        procedure LocalRender(const Params: TRenderParams); override;
      end;

    var
      FMode: TManipulateMode;
      FMainSelected: TCastleTransform;
      FMainSelectedObserver: TFreeNotificationObserver;
      FPickable: Boolean;
      Boxes: TDebugTransformBoxList;
      Gizmo: array [TManipulateMode] of TGizmoScene;
      FOnTransformModified: TNotifyEvent;
      FOnTransformModifyEnd: TNotifyEvent;
      FSelectedCount: Cardinal;

    procedure SetMode(const AValue: TManipulateMode);
    procedure SetMainSelected(const AValue: TCastleTransform);
    procedure SetPickable(const Value: Boolean);
    procedure GizmoHasTransformModified(Sender: TObject);
    procedure GizmoHasTransformModifyEnd(Sender: TObject);
    procedure MainSelectedFreeNotification(const Sender: TFreeNotificationObserver);
    procedure CreateMoreBoxes;
    function GetSelected(const Index: Integer): TCastleTransform;

    { Current TCastleTransform for potential move/rotate/scale
      (depending on @link(Mode)).
      @nil to not visualize anything.

      TODO: we'd like to remove this property at some point,
      gizmos to transform should work on multi-selection too,
      thus rely on SetSelected. }
    property MainSelected: TCastleTransform read FMainSelected write SetMainSelected;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    { Called whenever we modify the transformation of selected transformations
      by moving/rotating/scaling.
      Useful e.g. to mark the design as "modified". }
    property OnTransformModified: TNotifyEvent
      read FOnTransformModified write FOnTransformModified;

    { Called when we stop dragging (to move/rotate/scale).
      Useful e.g. to mark new state as "next undo step". }
    property OnTransformModifyEnd: TNotifyEvent
      read FOnTransformModifyEnd write FOnTransformModifyEnd;

    { Modify this property temporarily to influnce ray collisions.
      If your code uses ray collisions (non-physical) using
      e.g. @code(MyViewport.Items.WorldRay) and you don't want to ever
      pick the internal "gizmo" scenes for manipulating, then set this
      temporarily to @false. }
    property Pickable: Boolean read FPickable write SetPickable default true;

    { Current mode of operation. }
    property Mode: TManipulateMode read FMode write SetMode
      default mmSelect;

    { Pass a list of TCastleTransform instances to visualize them as selected
      and to manipulate them (move/rotate/scale).

      TODO: Right now, manipulating (move/rotate/scale) works only for a single
      transform. You cannot pass more than one item on the NewSelected list
      if you want to have move/rotate/scale working.

      The argument NewSelected may be @nil, which is treated just like passing an
      empty list.

      Instance of other classes on the NewSelected list are allowed,
      and they are just ignored.

      The list NewSelected does not become owned by this class.
      You can free it whenever you want, even immediately after calling this method,
      we don't keep any reference to it -- we copy the contents. }
    procedure SetSelected(const NewSelected: TComponentList); overload;
    procedure SetSelected(const NewSelected: array of TComponent); overload;

    { Currently selected items, use indexes from 0 to SelectedCount - 1.
      This is equivalent to the contents of last SetSelected call,
      but filtered to contain only TCastleTransform instances that
      can be manipulated. }
    property Selected[const Index: Integer]: TCastleTransform read GetSelected;

    { Count of currently selected items.
      @seealso Selected }
    function SelectedCount: Integer;
  end;

var
  { Hover and selection colors.
    They have reasonable defaults, but you can change them if you want. }
  ColorHover, ColorSelected: TCastleColor;

implementation

uses Math,
  CastleLog, CastleShapes, CastleViewport, CastleProjection,
  CastleQuaternions, X3DNodes, CastleGLUtils, CastleRenderContext,
  CastleKeysMouse, CastleRenderOptions,
  CastleInternalTransformData;

{ TCastleTransformHover ------------------------------------------------------ }

constructor TCastleTransformHover.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FCurrentObserver := TFreeNotificationObserver.Create(Self);
  FCurrentObserver.OnFreeNotification := {$ifdef FPC}@{$endif} CurrentFreeNotification;

  Box := TDebugTransformBox.Create(Self);
  Box.BoxColor := ColorOpacity(ColorHover, 0.25);
  Box.Exists := true;
end;

destructor TCastleTransformHover.Destroy;
begin
  inherited;
end;

procedure TCastleTransformHover.SetCurrent(const AValue: TCastleTransform);
begin
  if FCurrent = AValue then Exit;

  FCurrent := AValue;
  FCurrentObserver.Observed := AValue;
  Box.Parent := FCurrent;
end;

procedure TCastleTransformHover.CurrentFreeNotification(const Sender: TFreeNotificationObserver);
begin
  Current := nil;
end;

{ TCastleTransformManipulate.TGizmoScene -------------------------------------------- }

function TCastleTransformManipulate.TGizmoScene.PointOnAxis(
  out Intersection: TVector3; const Pick: TRayCollisionNode;
  const Axis: Integer): Boolean;

(*
var
  Axis1, Axis2: Integer;
begin
  Result := Pick.Triangle <> nil; // otherwise Pick.Point undefined

  Intersection := Pick.Point;

  // leave only Intersection[Axis] non-zero
  if Axis >= 0 then
  begin
    RestOf3DCoords(Axis, Axis1, Axis2);
    Intersection[Axis1] := 0;
    Intersection[Axis2] := 0;
  end;
end;
*)

var
  IntersectionScalar: Single;
  IntersectionOneAxis: TVector3;
begin
  if Axis = -1 then
  begin
    (*
    Result := Pick.Triangle <> nil; // otherwise Pick.Point undefined
    if Result then
    begin
      Intersection := Pick.Point;
      IntersectionScalar := Approximate3DScale(Intersection);
      Intersection := Vector3(IntersectionScalar, IntersectionScalar, IntersectionScalar);
    end;
    *)

    Result := true;
    IntersectionScalar := Sqrt(PointToLineDistanceSqr(TVector3.Zero, Pick.RayOrigin, Pick.RayDirection));
    Intersection := Vector3(IntersectionScalar, IntersectionScalar, IntersectionScalar);
  end else
  if Axis = -2 then
  begin
    Intersection := Vector3(0, 0, 0);
    Result := false;
    if PointOnLineClosestToLine(IntersectionOneAxis,
       TVector3.Zero, TVector3.One[0],
       Pick.RayOrigin, Pick.RayDirection) then
    begin
      Intersection := IntersectionOneAxis;
      Result := true;
    end;

    if PointOnLineClosestToLine(IntersectionOneAxis,
       TVector3.Zero, TVector3.One[1],
       Pick.RayOrigin, Pick.RayDirection) then
    begin
      Intersection := Intersection + IntersectionOneAxis;
      Result := true;
    end;
  end else
  begin
    Result := PointOnLineClosestToLine(Intersection,
      TVector3.Zero, TVector3.One[Axis],
      Pick.RayOrigin, Pick.RayDirection);

    {$ifdef DEBUG_GIZMO_PICK}
    VisualizePick.Exists := Result;
    if Result then
    begin
      // Intersection is in MainSelected coordinate space, i.e. ignores our gizmo scale
      VisualizePick.Translation := OutsideToLocal(Intersection);
      WritelnLog('VisualizePick with %s', [Intersection.ToString]);
      WritelnLog('Line 1: %s %s, line 2 %s %s', [
        TVector3.Zero.ToString,
        TVector3.One[Axis].ToString,
        Pick.RayOrigin.ToString,
        Pick.RayDirection.ToString
      ]);
    end;
    {$endif DEBUG_GIZMO_PICK}
  end;
end;

function TCastleTransformManipulate.TGizmoScene.AngleOnPlane(out Angle: Single;
  const Pick: TRayCollisionNode; const Coord: T3DAxis): Boolean;
var
  C1, C2: T3DAxis;
  PointProjected: TVector2;
  Intersection: TVector3;
begin
  if not TrySimplePlaneRayIntersection(Intersection, Coord, 0, Pick.RayOrigin, Pick.RayDirection) then
    Exit(false);
  { Use RestOf3DCoordsCycle, in the lopping order X-Y-Z.
    This results in consistent ArcTan2 results, that makes rotating around
    any coord in TCastleTransformManipulate.TGizmoScene.PointingDeviceMove
    have the same behavior (no need to invert angle sign for Y coord,
    as with RestOf3DCoords). }
  RestOf3DCoordsCycle(Coord, C1, C2);
  PointProjected.X := Intersection[C1];
  PointProjected.Y := Intersection[C2];
  Angle := ArcTan2(PointProjected.Y, PointProjected.X);
  Result := true;
end;

procedure TCastleTransformManipulate.TGizmoScene.DoTransformModified;
begin
  if Assigned(OnTransformModified) then
    OnTransformModified(Self);
end;

procedure TCastleTransformManipulate.TGizmoScene.DoTransformModifyEnd;
begin
  if Assigned(OnTransformModifyEnd) then
    OnTransformModifyEnd(Self);
end;

procedure TCastleTransformManipulate.TGizmoScene.ChangeWorld(
  const Value: TCastleAbstractRootTransform);
begin
  if Value <> World then
  begin
    inherited;
    GizmoDragging := false;
  end;
end;

constructor TCastleTransformManipulate.TGizmoScene.Create(AOwner: TComponent);
{$ifdef DEBUG_GIZMO_PICK}
var
  SphereGeometry: TSphereNode;
  SphereShape: TShapeNode;
  SphereMat: TMaterialNode;
  SphereAppearance: TAppearanceNode;
  SphereRoot: TX3DRootNode;
{$endif DEBUG_GIZMO_PICK}
begin
  inherited Create(AOwner);
  InternalExistsOnlyInMeaningfulParents := true;
  RenderLayer := rlFront;

  {$ifdef DEBUG_GIZMO_PICK}
  VisualizePick := TCastleScene.Create(Self);

  SphereGeometry := TSphereNode.CreateWithShape(SphereShape);
  SphereGeometry.Radius := 0.1;

  SphereMat := TMaterialNode.Create;
  SphereMat.DiffuseColor := RedRGB;

  SphereAppearance := TAppearanceNode.Create;
  SphereAppearance.Material := SphereMat;

  SphereShape.Appearance := SphereAppearance;

  SphereRoot := TX3DRootNode.Create;
  SphereRoot.AddChildren(SphereShape);

  VisualizePick.Load(SphereRoot, true);
  VisualizePick.Exists := false;
  Add(VisualizePick);
  {$endif DEBUG_GIZMO_PICK}
end;

procedure TCastleTransformManipulate.TGizmoScene.InternalCameraChanged;
begin
  inherited;
  { UpdateSize changes our transformation, and ChangedTransform in turn
    calls InternalCameraChanged, which means we can get into infinite
    recursion. Use InsideInternalCameraChanged to prevent it. }
  if InsideInternalCameraChanged then Exit;
  InsideInternalCameraChanged := true;
  UpdateSize;
  InsideInternalCameraChanged := false;
end;

procedure TCastleTransformManipulate.TGizmoScene.UpdateSize;

  function Projected(const V, X, Y: TVector3): TVector2;
  begin
    Result.X := TVector3.DotProduct(V, X);
    Result.Y := TVector3.DotProduct(V, Y);
  end;

var
  OldScale: TVector3;

  { Surround calls to WorldTransform in this, to account for
    GizmoScalingAssumeScale[Value]. }
  procedure BeginWorldTransform;
  begin
    if GizmoScalingAssumeScale then
    begin
      OldScale := Parent.Scale;
      Parent.Scale := GizmoScalingAssumeScaleValue;
    end;
  end;

  procedure EndWorldTransform;
  begin
    if GizmoScalingAssumeScale then
      Parent.Scale := OldScale;
  end;

  function GetPerspectiveGizmoScale(const Camera: TCastleCamera; const BaseGizmoScale: Single): Single;
  const
    AssumeNear = 1.0;
  var
    // ViewProjectionMatrix: TMatrix4;
    ZeroProjected, OneProjected: TVector2;
    OneDistance: Single;
    CameraPos, CameraDir, CameraUp: TVector3; //< camera vectors in world-coordinates
    ZeroWorld, OneWorld, OneProjected3, ZeroProjected3, CameraSide: TVector3;
    CameraNearPlane: TVector4;
    SceneSizeMultiplier: Single;
  begin
    { In theory, any value of SceneSizeMultiplier is OK,
      and it could be always 1.0.
      But on large scenes, this makes huge precision problems with calculation
      below, as OneWorld will be very close to ZeroWorld and then OneDistance is tiny.
      So we use to calculate in larger coordinates, and then scale it back to achieve the same.
      Testcase: gizmo_flickering_bug . }
    SceneSizeMultiplier := World.BoundingBox.AverageSize(false, 1.0);

    Camera.GetWorldView(CameraPos, CameraDir, CameraUp);

    BeginWorldTransform;

    { Map two points from gizmo local transformation,
      to determine correct gizmo scale.
      These points reflect the parent translation and scale.

      Note that we know that gizmo itself has never any translation,
      but it may have a scale.
    }
    Scale := Vector3(1, 1, 1); // assume gizmo scale = 1, will be changed later
    ZeroWorld := LocalToWorld(TVector3.Zero);
    { Note: We use CameraUp, not Camera.GravityUp, to work sensibly even
      when looking at world at a direction similar to +Y. }
    OneWorld := LocalToWorld(WorldToLocalDirection(CameraUp).AdjustToLength(SceneSizeMultiplier));

    EndWorldTransform;

    (* TODO: why this fails:
    ViewProjectionMatrix := Camera.ProjectionMatrix * Camera.Matrix;
    ZeroProjected := (ViewProjectionMatrix * Vector4(ZeroWorld, 1)).XY;
    OneProjected := (ViewProjectionMatrix * Vector4(OneWorld, 1)).XY;
    *)

    CameraNearPlane := Vector4(
      CameraDir,
      { plane equation should yield 0 when used with point in front of camera }
      - TVector3.DotProduct(
          CameraPos + CameraDir * AssumeNear * SceneSizeMultiplier,
          CameraDir
        )
    );
    if not TryPlaneLineIntersection(OneProjected3, CameraNearPlane, CameraPos, OneWorld - CameraPos) then
      Exit(1.0); // no valid value can be calculated
    if not TryPlaneLineIntersection(ZeroProjected3, CameraNearPlane, CameraPos, ZeroWorld - CameraPos) then
      Exit(1.0); // no valid value can be calculated

    CameraSide := TVector3.CrossProduct(CameraDir, CameraUp);
    ZeroProjected := Projected(ZeroProjected3, CameraSide, CameraUp);
    OneProjected := Projected(OneProjected3, CameraSide, CameraUp);

    // get the distance, on screen in pixels, of a 1 unit in 3D around gizmo
    OneDistance := PointsDistance(ZeroProjected, OneProjected);

    if IsZero(OneDistance) then
      Result := SceneSizeMultiplier
    else
      { Multiply by SceneSizeMultiplier 2x because
        1. it increases OneWorld
        2. it increases camera near (at AssumeNear) }
      Result := Sqr(SceneSizeMultiplier) * BaseGizmoScale / OneDistance;
  end;

const
  BaseGizmoScaleOrtho = 0.3 * 0.5;
  BaseGizmoScalePerspective = 0.25 * 0.5;
var
  Camera: TCastleCamera;
  GizmoScale, ScaleUniform: Single;
begin
  { Adjust scale to take the same space on screen. }
  if HasWorldTransform and
     (World <> nil) and
     (World.MainCamera <> nil) then
  begin
    Camera := World.MainCamera;

    if Camera.ProjectionType = ptOrthographic then
    begin
      { We just want gizmo is about 15% of effective height }
      GizmoScale := BaseGizmoScaleOrtho * Camera.Orthographic.EffectiveRect.Height;
      ScaleUniform := Parent.WorldToLocalDistance(GizmoScale);
    end else
    begin
      GizmoScale := BaseGizmoScalePerspective {TODO:* Camera.Perspective.EffectiveFieldOfViewVertical};
      ScaleUniform := GetPerspectiveGizmoScale(Camera, GizmoScale);
    end;

    Scale := Vector3(ScaleUniform, ScaleUniform, ScaleUniform);
  end;
end;

(*
// Not needed now
function TCastleTransformManipulate.TGizmoScene.Dragging: boolean;
begin
  Result := (inherited Dragging) or GizmoDragging;
end;
*)

function TCastleTransformManipulate.TGizmoScene.PointingDevicePress(
  const Pick: TRayCollisionNode; const Distance: Single): Boolean;
var
  AppearanceName: String;
  CanDrag: Boolean;

  function IsOrthographicTranslation: Boolean;
  begin
    Result := (
      (Mode = mmTranslate) and
      HasWorldTransform and
      (World <> nil) and
      (World.MainCamera <> nil) and
      (World.MainCamera.ProjectionType = ptOrthographic) and
      // TODO: should check world-space direction
      (TVector3.Equals(World.MainCamera.Direction, Vector3(0, 0, -1)))
    );
  end;

begin
  Result := inherited;
  if Result then Exit;

  { When importing glTF, Blender material name -> X3D Appearance name. }
  if (Pick.Triangle <> nil) and
     (Pick.Triangle^.ShapeNode <> nil) and
     (Pick.Triangle^.ShapeNode.Appearance <> nil) then
  begin
    AppearanceName := Pick.Triangle^.ShapeNode.Appearance.X3DName;
    if AppearanceName = 'MaterialX' then
    begin
      DraggingCoord := 0;
    end else
    if AppearanceName = 'MaterialY' then
    begin
      DraggingCoord := 1;
    end else
    if AppearanceName = 'MaterialZ' then
    begin
      { In 2D mode dragging Z axis means translate in X and Y. }
      if IsOrthographicTranslation then
        DraggingCoord := -2
      else
        DraggingCoord := 2;
    end else
    if AppearanceName = 'MaterialCenter' then
    begin
      { In 2D mode dragging center square means translate in X and Y. }
      if IsOrthographicTranslation then
        DraggingCoord := -2
      else
      if Mode = mmScale then
        DraggingCoord := -1;
    end else
      Exit; // ignore draging on other material names

    if Mode = mmRotate then
      CanDrag := AngleOnPlane(LastPickAngle, Pick, DraggingCoord)
    else
      CanDrag := PointOnAxis(LastPick, Pick, DraggingCoord);

    if CanDrag then
    begin
      if Mode = mmScale then
      begin
        { In CameraChanged, we adjust gizmo scale to make it fit within
          the screen nicely. This way, we actually "nullify" the effect
          of parent's scale on gizmo size.

          But this has to be disabled within the dragging when scaling,
          to enable scaling gizmo get smaller/larger as we drag.

          During a single drag, we behave like Scale is constant.
          Gizmo will be correctly scaled when you release. }
        GizmoScalingAssumeScale := true;
        GizmoScalingAssumeScaleValue := Parent.Scale;
      end;
      GizmoDragging := true;
      // keep tracking pointing device events, by TCastleViewport.CapturePointingDevice mechanism
      Result := true;
    end;
  end;
end;

function TCastleTransformManipulate.TGizmoScene.PointingDeviceMove(
  const Pick: TRayCollisionNode; const Distance: Single): Boolean;
var
  NewPick, Diff: TVector3;
  NewPickAngle, DiffAngle: Single;
  I: Integer;
  DragSuccess: Boolean;
begin
  Result := inherited;
  if Result then Exit;

  if GizmoDragging then
  begin
    if Mode = mmRotate then
      DragSuccess := AngleOnPlane(NewPickAngle, Pick, DraggingCoord)
    else
      DragSuccess := PointOnAxis(NewPick, Pick, DraggingCoord);
    if DragSuccess then
    begin
      {$ifdef DEBUG_GIZMO_PICK}
      if TCastleControl.MainControl.Pressed[keyShift] then
      {$endif DEBUG_GIZMO_PICK}
      case Mode of
        mmTranslate:
          begin
            Diff := NewPick - LastPick;
            { Our gizmo display and interaction is affected by existing
              Parent.Rotation although the Parent.Translation
              is applied before rotation technically.
              So we need to manually multiply Diff by curent rotation. }
            Diff := RotatePointAroundAxis(Parent.Rotation, Diff);
            if DraggingCoord >= 0 then
              { We need to apply only scale in 1 axis,
                https://forum.castle-engine.io/t/creating-a-non-linear-strategic-adventure/403/22 }
              Diff := Diff * Parent.Scale[DraggingCoord]
            else
              Diff := Diff * Parent.Scale;
            Parent.Translation := Parent.Translation + Diff;
          end;
        mmRotate:
          begin
            DiffAngle := NewPickAngle - LastPickAngle;
            Parent.Rotation := (
              QuatFromAxisAngle(Parent.Rotation) *
              QuatFromAxisAngle(TVector3.One[DraggingCoord], DiffAngle)).
              ToAxisAngle;
          end;
        mmScale:
          begin
            for I := 0 to 2 do
              if IsZero(LastPick[I]) then
                Diff.Data[I] := 1
              else
                Diff.Data[I] := NewPick[I] / LastPick[I];
            Parent.Scale := Parent.Scale * Diff;
          end;
        mmSelect:
          raise EInternalError.Create('TGizmoScene shall never be created with mmSelect, ');
      end;

      { No point in updating LastPick or LastPickAngle:
        it remains the same, as it is expressed
        in local coordinate system, which we just changed by changing
        Parent.Translation. }

      // update our gizmo size, as we moved ourselves
      UpdateSize;
      DoTransformModified;
    end;
  end;
end;

function TCastleTransformManipulate.TGizmoScene.PointingDeviceRelease(
  const Pick: TRayCollisionNode; const Distance: Single;
  const CancelAction: Boolean): Boolean;
begin
  Result := inherited;
  if Result then Exit;

  GizmoDragging := false;

  if GizmoScalingAssumeScale then
  begin
    GizmoScalingAssumeScale := false;
    UpdateSize;
  end;
  DoTransformModifyEnd;
end;

procedure TCastleTransformManipulate.TGizmoScene.LocalRender(const Params: TRenderParams);
var
  GizmoRendered: Boolean;
begin
  { Similar to TInternalCastleEditorGizmo.LocalRender, do not render when gizmo
    is over the rendering camera (happens when moving/rotating/scaling
    the camera that is aligned to view). }
  LastIsProjectionOrthographic :=
    (Params.RenderingCamera.Camera <> nil) and
    (Params.RenderingCamera.Camera.ProjectionType = ptOrthographic);
  GizmoRendered := TInternalCastleEditorGizmo.ShouldGizmoBeRendered(
    LastIsProjectionOrthographic,
    Params.Transformation^.Transform.MultPoint(TVector3.Zero),
    Params.RenderingCamera.View.Translation
  );
  if not GizmoRendered then
    Exit; // do not show gizmo
  inherited;
end;

function TCastleTransformManipulate.TGizmoScene.LocalRayCollision(
  const RayOrigin, RayDirection: TVector3;
  const TrianglesToIgnoreFunc: TTriangleIgnoreFunc): TRayCollision;
var
  GizmoRendered: Boolean;
begin
  { Similar to TInternalCastleEditorGizmo.LocalRayCollision, do not collide
    when gizmo is also hidden. }
  GizmoRendered := TInternalCastleEditorGizmo.ShouldGizmoBeRendered(
    LastIsProjectionOrthographic,
    TVector3.Zero, RayOrigin);
  if not GizmoRendered then
    Exit(nil); // do not pick with gizmo with raycast

  Result := inherited;

  { Hack to make picking of the gizmo work even when gizmo is obscured
    by other TCastleTransform (including bbox of Parent, which is what
    we actually want to transform).
    Hacking Distance to be smallest possible means that it "wins"
    when TCastleTransform.LocalRayCollision desides which collision
    is first along the ray. }
  if Result <> nil then
    Result.Distance := 0;
end;

{ TCastleTransformManipulate ------------------------------------------------------ }

constructor TCastleTransformManipulate.Create(AOwner: TComponent);

  function CreateGizmoScene: TGizmoScene;
  begin
    Result := TGizmoScene.Create(Self);
    Result.Collides := false;
    Result.Pickable := FPickable;
    Result.CastShadows := false;
    Result.InternalExcludeFromParentBoundingVolume := true;
    Result.PreciseCollisions := true;
    Result.SetTransient;
    Result.OnTransformModified := {$ifdef FPC}@{$endif} GizmoHasTransformModified;
    Result.OnTransformModifyEnd := {$ifdef FPC}@{$endif} GizmoHasTransformModifyEnd;
  end;

begin
  inherited;
  FPickable := true;

  FMainSelectedObserver := TFreeNotificationObserver.Create(Self);
  FMainSelectedObserver.OnFreeNotification := {$ifdef FPC}@{$endif} MainSelectedFreeNotification;

  Boxes := TDebugTransformBoxList.Create(true);
  CreateMoreBoxes;

  RegisterTransformManipulateData;

  // Gizmo[mmSelect] remains nil
  Gizmo[mmTranslate] := CreateGizmoScene;
  Gizmo[mmTranslate].Load('castle-internal-embedded-data:/translate_final.x3dv');
  Gizmo[mmTranslate].Mode := mmTranslate;
  Gizmo[mmRotate] := CreateGizmoScene;
  Gizmo[mmRotate].Load('castle-internal-embedded-data:/rotate_final.x3dv');
  Gizmo[mmRotate].Mode := mmRotate;
  Gizmo[mmScale] := CreateGizmoScene;
  Gizmo[mmScale].Load('castle-internal-embedded-data:/scale_final.x3dv');
  Gizmo[mmScale].Mode := mmScale;
end;

destructor TCastleTransformManipulate.Destroy;
begin
  inherited;
end;

procedure TCastleTransformManipulate.CreateMoreBoxes;
const
  IncreaseBoxes = 8;
var
  C, I: Integer;
begin
  C := Boxes.Count;
  Boxes.Count := Boxes.Count + IncreaseBoxes;
  for I := C to Boxes.Count - 1 do
  begin
    Boxes[I] := TDebugTransformBox.Create(Self);
    Boxes[I].BoxColor := ColorOpacity(ColorSelected, 0.5);
    Boxes[I].Exists := true;
  end;
end;

procedure TCastleTransformManipulate.SetSelected(const NewSelected: array of TComponent);
var
  List: TComponentList;
  C: TComponent;
begin
  List := TComponentList.Create(false);
  try
    for C in NewSelected do
      List.Add(C);
    SetSelected(List);
  finally FreeAndNil(List) end;
end;

procedure TCastleTransformManipulate.SetSelected(const NewSelected: TComponentList);
var
  I: Integer;
begin
  FSelectedCount := 0;
  if NewSelected <> nil then
    for I := 0 to NewSelected.Count - 1 do
      if (NewSelected[I] is TCastleTransform) and
         { Disallow editing TCastleAbstractRootTransform transformation.
           See InspectorFilter in CGE editor for explanation, in short:
           editing TCastleAbstractRootTransform transformation is very
           unintuitive, as you transform both objects and the camera...
           so actually nothing visible happens. }
         (not (NewSelected[I] is TCastleAbstractRootTransform)) then
      begin
        if FSelectedCount >= Boxes.Count then
          CreateMoreBoxes;
        Assert(FSelectedCount < Boxes.Count);
        Boxes[FSelectedCount].Parent := TCastleTransform(NewSelected[I]);
        Inc(FSelectedCount);
      end;

  { Note: each TDebugTransformBox already observes its Parent,
    and sets own Parent=nil if the parent is freed.
    So there's no need to worry about it here anymore. }

  { Detach remaining Boxes from any parent, to not show them }
  for I := FSelectedCount to Boxes.Count - 1 do
    Boxes[I].Parent := nil;

  { Update MainSelected }
  if SelectedCount = 1 then
    MainSelected := Selected[0]
  else
    MainSelected := nil;
end;

procedure TCastleTransformManipulate.SetMainSelected(const AValue: TCastleTransform);
begin
  if FMainSelected = AValue then Exit;

  if FMainSelected <> nil then
  begin
    if Gizmo[Mode] <> nil then
      FMainSelected.Remove(Gizmo[Mode]);
  end;

  FMainSelected := AValue;
  FMainSelectedObserver.Observed := AValue;

  if FMainSelected <> nil then
  begin
    if Gizmo[Mode] <> nil then
      FMainSelected.Add(Gizmo[Mode]);
  end;
end;

procedure TCastleTransformManipulate.MainSelectedFreeNotification(const Sender: TFreeNotificationObserver);
begin
  MainSelected := nil;
end;

procedure TCastleTransformManipulate.SetPickable(const Value: Boolean);
var
  M: TManipulateMode;
begin
  if FPickable <> Value then
  begin
    FPickable := Value;
    for M := Low(TManipulateMode) to High(TManipulateMode) do
      if Gizmo[M] <> nil then // Gizmo[mmSelect] is nil now, nothing to show
        Gizmo[M].Pickable := Value;
  end;
end;

procedure TCastleTransformManipulate.GizmoHasTransformModified(Sender: TObject);
begin
  if Assigned(OnTransformModified) then
    OnTransformModified(Self);
end;

procedure TCastleTransformManipulate.GizmoHasTransformModifyEnd(Sender: TObject);
begin
  if Assigned(OnTransformModifyEnd) then
    OnTransformModifyEnd(Self);
end;

procedure TCastleTransformManipulate.SetMode(const AValue: TManipulateMode);
begin
  if FMode = AValue then Exit;

  if (FMainSelected <> nil) and (Gizmo[Mode] <> nil) then
    FMainSelected.Remove(Gizmo[Mode]);

  FMode := AValue;

  if (FMainSelected <> nil) and (Gizmo[Mode] <> nil) then
    FMainSelected.Add(Gizmo[Mode]);
end;

function TCastleTransformManipulate.SelectedCount: Integer;
begin
  Result := FSelectedCount;
end;

function TCastleTransformManipulate.GetSelected(const Index: Integer): TCastleTransform;
begin
  if (Index >= 0) and (Index < FSelectedCount) then
  begin
    Result := Boxes[Index].Parent;
    Assert(Result <> nil);
  end else
    raise EListError.CreateFmt('Index %d out of bounds for %d items (TCastleTransformManipulate.Selected)', [
      Index,
      FSelectedCount
    ]);
end;

initialization
  ColorHover := HexToColor('fffba0'); // desaturated yellow
  ColorSelected := White;
end.
