{
  Copyright 2020-2022 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Visualize TCastleTransform selection and dragging to transform (TVisualizeTransform). }
unit DesignVisualizeTransform;

interface

uses Classes, SysUtils, Contnrs, Generics.Collections,
  CastleColors, CastleVectors, CastleVectorsInternalSingle, CastleTransform,
  CastleDebugTransform, CastleScene, CastleCameras, CastleTriangles, CastleUtils,
  CastleClassUtils;

type
  { Visualize TCastleTransform hover.
    This is really just a thin wrapper around TDebugTransformBox in this case. }
  TVisualizeTransformHover = class(TComponent)
  strict private
    FParent: TCastleTransform;
    FParentObserver: TFreeNotificationObserver;
    Box: TDebugTransformBox;
    procedure ParentFreeNotification(const Sender: TFreeNotificationObserver);
    procedure SetParent(const AValue: TCastleTransform);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    { Currently visualized TCastleTransform instance as "we are hovering over it,
      and clicking will select it". }
    property Parent: TCastleTransform read FParent write SetParent;
  end;

  TVisualizeOperation = (voSelect, voTranslate, voRotate, voScale);

  TDebugTransformBoxList = specialize TObjectList<TDebugTransformBox>;

  { Visualize TCastleTransform selection and dragging to transform. }
  TVisualizeTransformSelected = class(TComponent)
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

        const
          DistanceToExistGizmo = 1;

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

        procedure DoParentModified;
        procedure DoGizmoStopDrag;
        procedure UpdateSize;
      protected
        procedure ChangeWorld(const Value: TCastleAbstractRootTransform); override;
        function LocalRayCollision(const RayOrigin, RayDirection: TVector3;
          const TrianglesToIgnoreFunc: TTriangleIgnoreFunc): TRayCollision; override;
      public
        Operation: TVisualizeOperation;
        OnParentModified: TNotifyEvent;
        OnGizmoStopDrag: TNotifyEvent;
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
      FOperation: TVisualizeOperation;
      FParent: TCastleTransform;
      FParentObserver: TFreeNotificationObserver;
      FPickable: Boolean;
      Boxes: TDebugTransformBoxList;
      Gizmo: array [TVisualizeOperation] of TGizmoScene;
    procedure SetOperation(const AValue: TVisualizeOperation);
    procedure SetParent(const AValue: TCastleTransform);
    procedure SetPickable(const Value: Boolean);
    procedure GizmoHasModifiedParent(Sender: TObject);
    procedure GizmoStopDrag(Sender: TObject);
    procedure ParentFreeNotification(const Sender: TFreeNotificationObserver);
    procedure CreateMoreBoxes;
  public
    OnParentModified: TNotifyEvent;
    OnGizmoStopDrag: TNotifyEvent;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    { Currently visualized TCastleTransform instance for potential move/rotate/scale
      (depending on Operation).
      @nil to not visualize anything.

      TODO: ideally we'd like to remove it at one point, and gizmos to transform
      should work on multi-selection too. }
    property Parent: TCastleTransform read FParent write SetParent;

    property Pickable: Boolean read FPickable write SetPickable;
    property Operation: TVisualizeOperation read FOperation write SetOperation
      default voSelect;

    { Visualize selected parents reflecting the given list.
      Selected may be nil (equivalent to empty list).
      Only TCastleTransform descendants on Selected are taken into account here. }
    procedure SetSelectedParents(const Selected: TComponentList);
  end;

var
  ColorHover, ColorSelected, ColorHoverAndSelected: TCastleColor;

implementation

uses Math,
  ProjectUtils,
  CastleLog, CastleShapes, CastleViewport, CastleProjection,
  CastleQuaternions, X3DNodes, CastleGLUtils, CastleRenderContext,
  CastleControl, CastleKeysMouse, CastleRenderOptions;

{ TVisualizeTransformHover ------------------------------------------------------ }

constructor TVisualizeTransformHover.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FParentObserver := TFreeNotificationObserver.Create(Self);
  FParentObserver.OnFreeNotification := {$ifdef FPC}@{$endif} ParentFreeNotification;

  Box := TDebugTransformBox.Create(Self);
  Box.BoxColor := ColorOpacity(ColorHover, 0.25);
  Box.Exists := true;
end;

destructor TVisualizeTransformHover.Destroy;
begin
  inherited;
end;

procedure TVisualizeTransformHover.SetParent(const AValue: TCastleTransform);
begin
  if FParent = AValue then Exit;

  FParent := AValue;
  FParentObserver.Observed := AValue;
  Box.Parent := FParent;
end;

procedure TVisualizeTransformHover.ParentFreeNotification(const Sender: TFreeNotificationObserver);
begin
  Parent := nil;
end;

{ TVisualizeTransformSelected.TGizmoScene -------------------------------------------- }

function TVisualizeTransformSelected.TGizmoScene.PointOnAxis(
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
      // Intersection is in Parent coordinate space, i.e. ignores our gizmo scale
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

function TVisualizeTransformSelected.TGizmoScene.AngleOnPlane(out Angle: Single;
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
    any coord in TVisualizeTransformSelected.TGizmoScene.PointingDeviceMove
    have the same behavior (no need to invert angle sign for Y coord,
    as with RestOf3DCoords). }
  RestOf3DCoordsCycle(Coord, C1, C2);
  PointProjected.X := Intersection[C1];
  PointProjected.Y := Intersection[C2];
  Angle := ArcTan2(PointProjected.Y, PointProjected.X);
  Result := true;
end;

procedure TVisualizeTransformSelected.TGizmoScene.DoParentModified;
begin
  if Assigned(OnParentModified) then
    OnParentModified(Self);
end;

procedure TVisualizeTransformSelected.TGizmoScene.DoGizmoStopDrag;
begin
  if Assigned(OnGizmoStopDrag) then
    OnGizmoStopDrag(Self);
end;

procedure TVisualizeTransformSelected.TGizmoScene.ChangeWorld(
  const Value: TCastleAbstractRootTransform);
begin
  if Value <> World then
  begin
    inherited;
    GizmoDragging := false;
  end;
end;

constructor TVisualizeTransformSelected.TGizmoScene.Create(AOwner: TComponent);
{$ifdef DEBUG_GIZMO_PICK}
var
  SphereGeometry: TSphereNode;
  SphereShape: TShapeNode;
  SphereMat: TMaterialNode;
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
  SphereShape.Material := SphereMat;

  SphereRoot := TX3DRootNode.Create;
  SphereRoot.AddChildren(SphereShape);

  VisualizePick.Load(SphereRoot, true);
  VisualizePick.Exists := false;
  Add(VisualizePick);
  {$endif DEBUG_GIZMO_PICK}
end;

procedure TVisualizeTransformSelected.TGizmoScene.InternalCameraChanged;
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

procedure TVisualizeTransformSelected.TGizmoScene.UpdateSize;

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
function TVisualizeTransformSelected.TGizmoScene.Dragging: boolean;
begin
  Result := (inherited Dragging) or GizmoDragging;
end;
*)

function TVisualizeTransformSelected.TGizmoScene.PointingDevicePress(
  const Pick: TRayCollisionNode; const Distance: Single): Boolean;
var
  AppearanceName: String;
  CanDrag: Boolean;

  function IsOrthographicTranslation: Boolean;
  begin
    Result := (
       (Operation = voTranslate)
       and HasWorldTransform
       and (World <> nil)
       and (World.MainCamera <> nil)
       and (World.MainCamera.ProjectionType = ptOrthographic)
       // TODO: should check world-space direction
       and (TVector3.Equals(World.MainCamera.Direction, Vector3(0, 0, -1)))
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
    case AppearanceName of
      'MaterialX': DraggingCoord := 0;
      'MaterialY': DraggingCoord := 1;
      'MaterialZ':
        begin
          { In 2D mode dragging Z axis means translate in X and Y. }
          if IsOrthographicTranslation then
            DraggingCoord := -2
          else
            DraggingCoord := 2;
        end;
      'MaterialCenter':
        begin
          { In 2D mode dragging center square means translate in X and Y. }
          if IsOrthographicTranslation then
            DraggingCoord := -2
          else if (Operation = voScale) then
            DraggingCoord := -1;
        end;
      else Exit;
    end;

    if Operation = voRotate then
      CanDrag := AngleOnPlane(LastPickAngle, Pick, DraggingCoord)
    else
      CanDrag := PointOnAxis(LastPick, Pick, DraggingCoord);

    if CanDrag then
    begin
      if Operation = voScale then
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

function TVisualizeTransformSelected.TGizmoScene.PointingDeviceMove(
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
    if Operation = voRotate then
      DragSuccess := AngleOnPlane(NewPickAngle, Pick, DraggingCoord)
    else
      DragSuccess := PointOnAxis(NewPick, Pick, DraggingCoord);
    if DragSuccess then
    begin
      {$ifdef DEBUG_GIZMO_PICK}
      if TCastleControl.MainControl.Pressed[keyShift] then
      {$endif DEBUG_GIZMO_PICK}
      case Operation of
        voTranslate:
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
        voRotate:
          begin
            DiffAngle := NewPickAngle - LastPickAngle;
            Parent.Rotation := (
              QuatFromAxisAngle(Parent.Rotation) *
              QuatFromAxisAngle(TVector3.One[DraggingCoord], DiffAngle)).
              ToAxisAngle;
          end;
        voScale:
          begin
            for I := 0 to 2 do
              if IsZero(LastPick[I]) then
                Diff.Data[I] := 1
              else
                Diff.Data[I] := NewPick[I] / LastPick[I];
            Parent.Scale := Parent.Scale * Diff;
          end;
      end;

      { No point in updating LastPick or LastPickAngle:
        it remains the same, as it is expressed
        in local coordinate system, which we just changed by changing
        Parent.Translation. }

      // update our gizmo size, as we moved ourselves
      UpdateSize;
      DoParentModified;
    end;
  end;
end;

function TVisualizeTransformSelected.TGizmoScene.PointingDeviceRelease(
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
  DoGizmoStopDrag;
end;

procedure TVisualizeTransformSelected.TGizmoScene.LocalRender(const Params: TRenderParams);
var
  DistanceToCameraSqr: Single;
  GizmoShouldExist: Boolean;
begin
  { Similar to TInternalCastleEditorGizmo.LocalRender, do not render when gizmo is over
    the rendering camera (happens when moving/rotating/scaling the camera that is aligned to view). }
  DistanceToCameraSqr := PointsDistanceSqr(
    Params.Transform^.MultPoint(TVector3.Zero),
    Params.RenderingCamera.Position
  );
  GizmoShouldExist := DistanceToCameraSqr > Sqr(DistanceToExistGizmo);
  if not GizmoShouldExist then
    Exit; // do not show gizmo

  inherited;
end;

function TVisualizeTransformSelected.TGizmoScene.LocalRayCollision(
  const RayOrigin, RayDirection: TVector3;
  const TrianglesToIgnoreFunc: TTriangleIgnoreFunc): TRayCollision;
var
  DistanceToCameraSqr: Single;
  GizmoShouldExist: Boolean;
begin
  { Similar to TInternalCastleEditorGizmo.LocalRayCollision, do not collide
    when gizmo is also hidden. }

  DistanceToCameraSqr := PointsDistanceSqr(
    TVector3.Zero,
    RayOrigin
  );
  GizmoShouldExist := DistanceToCameraSqr > Sqr(DistanceToExistGizmo);
  if not GizmoShouldExist then
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

{ TVisualizeTransformSelected ------------------------------------------------------ }

constructor TVisualizeTransformSelected.Create(AOwner: TComponent);

  function CreateGizmoScene: TGizmoScene;
  begin
    Result := TGizmoScene.Create(Self);
    Result.Collides := false;
    Result.Pickable := FPickable;
    Result.CastShadows := false;
    Result.ExcludeFromStatistics := true;
    Result.InternalExcludeFromParentBoundingVolume := true;
    {$warnings off} // TODO: Change this to Result.PreciseCollisions := true, once tested it equally performs
    Result.Spatial := [ssDynamicCollisions];
    {$warnings on}
    Result.SetTransient;
    Result.OnParentModified := @GizmoHasModifiedParent;
    Result.OnGizmoStopDrag := @GizmoStopDrag;
  end;

begin
  inherited;
  FPickable := true;

  FParentObserver := TFreeNotificationObserver.Create(Self);
  FParentObserver.OnFreeNotification := {$ifdef FPC}@{$endif} ParentFreeNotification;

  Boxes := TDebugTransformBoxList.Create(true);
  CreateMoreBoxes;

  // Gizmo[voSelect] remains nil
  Gizmo[voTranslate] := CreateGizmoScene;
  Gizmo[voTranslate].Load(InternalCastleDesignData + 'gizmos/transform/translate_final.x3dv');
  Gizmo[voTranslate].Operation := voTranslate;
  Gizmo[voRotate] := CreateGizmoScene;
  Gizmo[voRotate].Load(InternalCastleDesignData + 'gizmos/transform/rotate_final.x3dv');
  Gizmo[voRotate].Operation := voRotate;
  Gizmo[voScale] := CreateGizmoScene;
  Gizmo[voScale].Load(InternalCastleDesignData + 'gizmos/transform/scale_final.x3dv');
  Gizmo[voScale].Operation := voScale;
end;

destructor TVisualizeTransformSelected.Destroy;
begin
  inherited;
end;

procedure TVisualizeTransformSelected.CreateMoreBoxes;
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

procedure TVisualizeTransformSelected.SetSelectedParents(const Selected: TComponentList);
var
  NextBox, I: Integer;
begin
  NextBox := 0;
  if Selected <> nil then
    for I := 0 to Selected.Count - 1 do
      if Selected[I] is TCastleTransform then
      begin
        if NextBox >= Boxes.Count then
          CreateMoreBoxes;
        Assert(NextBox < Boxes.Count);
        Boxes[NextBox].Parent := TCastleTransform(Selected[I]);
        Inc(NextBox);
      end;

  { Note: each TDebugTransformBox already observes its Parent,
    and sets own Parent=nil if the parent is freed.
    So there's no need to worry about it here anymore. }

  { Detach remaining Boxes from any parent, to not show them }
  for I := NextBox to Boxes.Count - 1 do
    Boxes[I].Parent := nil;
end;

procedure TVisualizeTransformSelected.SetParent(const AValue: TCastleTransform);
begin
  if FParent = AValue then Exit;

  if FParent <> nil then
  begin
    if Gizmo[Operation] <> nil then
      FParent.Remove(Gizmo[Operation]);
  end;

  FParent := AValue;
  FParentObserver.Observed := AValue;

  if FParent <> nil then
  begin
    if Gizmo[Operation] <> nil then
      FParent.Add(Gizmo[Operation]);
  end;
end;

procedure TVisualizeTransformSelected.ParentFreeNotification(const Sender: TFreeNotificationObserver);
begin
  Parent := nil;
end;

procedure TVisualizeTransformSelected.SetPickable(const Value: Boolean);
var
  VisualizeOperation: TVisualizeOperation;
begin
  if FPickable <> Value then
  begin
    FPickable := Value;
    for VisualizeOperation in TVisualizeOperation do
      if Gizmo[VisualizeOperation] <> nil then // Gizmo[voSelect] is nil now, nothing to show
        Gizmo[VisualizeOperation].Pickable := Value;
  end;
end;

procedure TVisualizeTransformSelected.GizmoHasModifiedParent(Sender: TObject);
begin
  if Assigned(OnParentModified) then
    OnParentModified(Self);
end;

procedure TVisualizeTransformSelected.GizmoStopDrag(Sender: TObject);
begin
  if Assigned(OnGizmoStopDrag) then
    OnGizmoStopDrag(Self);
end;

procedure TVisualizeTransformSelected.SetOperation(const AValue: TVisualizeOperation);
begin
  if FOperation = AValue then Exit;

  if (FParent <> nil) and (Gizmo[Operation] <> nil) then
    FParent.Remove(Gizmo[Operation]);

  FOperation := AValue;

  if (FParent <> nil) and (Gizmo[Operation] <> nil) then
    FParent.Add(Gizmo[Operation]);
end;

initialization
  ColorHover := HexToColor('fffba0'); // desaturated yellow
  ColorSelected := White;
  ColorHoverAndSelected := Yellow;
end.
