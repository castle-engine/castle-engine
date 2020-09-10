{
  Copyright 2020-2020 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Visualize TCastleTransform selection and dragging to transform (TVisualizeTransform). }
unit VisualizeTransform;

interface

uses Classes, SysUtils, CastleColors, CastleVectors,
  CastleVectorsInternalSingle, CastleTransform, CastleDebugTransform,
  CastleScene, CastleCameras;

type
  TVisualizeOperation = (voSelect, voTranslate, voRotate, voScale);

  { Visualize TCastleTransform selection and dragging to transform. }
  TVisualizeTransform = class(TComponent)
  strict private
    type
      TGizmoScene = class(TCastleScene)
      strict private
        GizmoDragging: Boolean;
        DraggingAxis: Integer;
        LastPick: TVector3;
        { Point on axis closest to given pick. }
        function PointOnAxis(out Intersection: TVector3;
          const Pick: TRayCollisionNode; const Axis: Integer): Boolean;
      protected
        procedure ChangeWorld(const Value: TCastleAbstractRootTransform); override;
      public
        Operation: TVisualizeOperation;
        procedure CameraChanged(const ACamera: TCastleCamera); override;
        function Dragging: boolean; override;
        function PointingDevicePress(const Pick: TRayCollisionNode;
          const Distance: Single): Boolean; override;
        function PointingDeviceMove(const Pick: TRayCollisionNode;
          const Distance: Single): Boolean; override;
        function PointingDeviceRelease(const Pick: TRayCollisionNode;
          const Distance: Single; const CancelAction: Boolean): Boolean; override;
      end;

    var
      FHover: Boolean;
      FOperation: TVisualizeOperation;
      FParent: TCastleTransform;
      Box: TDebugTransformBox;
      Gizmo: array [TVisualizeOperation] of TGizmoScene;
    procedure SetOperation(const AValue: TVisualizeOperation);
    procedure SetParent(const AValue: TCastleTransform);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent; const AHover: Boolean); reintroduce;
    destructor Destroy; override;
    { Currently visualized TCastleTransform instance.
      @nil to not visualize anything. }
    property Parent: TCastleTransform read FParent write SetParent;
    property Operation: TVisualizeOperation read FOperation write SetOperation
      default voSelect;
  end;

var
  ColorHover, ColorSelected, ColorHoverAndSelected: TCastleColor;

implementation

uses Math,
  ProjectUtils,
  CastleLog, CastleShapes, CastleViewport, CastleProjection, CastleUtils,
  CastleQuaternions;

{ TVisualizeTransform.TGizmoScene -------------------------------------------- }

function TVisualizeTransform.TGizmoScene.PointOnAxis(
  out Intersection: TVector3; const Pick: TRayCollisionNode;
  const Axis: Integer): Boolean;

(*
var
  Axis1, Axis2: Integer;
begin
  Result := Pick.Triangle <> nil; // otherwise Pick.Point undefined

  Intersection := Pick.Point;

  // leave only Intersection[Axis] non-zero
  RestOf3DCoords(Axis, Axis1, Axis2);
  Intersection[Axis1] := 0;
  Intersection[Axis2] := 0;
end;
*)

begin
  Result := PointOnLineClosestToLine(Intersection,
    TVector3.Zero, TVector3.One[Axis],
    Pick.RayOrigin, Pick.RayDirection);
end;

procedure TVisualizeTransform.TGizmoScene.ChangeWorld(
  const Value: TCastleAbstractRootTransform);
begin
  if Value <> World then
  begin
    inherited;
    GizmoDragging := false;
    // TODO: CameraChanged is not automatically called by inherited ChangeWorld, maybe it should be?
    if Value <> nil then
      CameraChanged(Value.MainCamera);
  end;
end;

procedure TVisualizeTransform.TGizmoScene.CameraChanged(
  const ACamera: TCastleCamera);
const
  AssumeNear = 1.0;
var
  W{, ViewProjectionMatrix}: TMatrix4;
  ZeroProjected, OneProjected: TVector2;
  OneDistance, ScaleUniform: Single;
  ZeroWorld, OneWorld, OneProjected3, ZeroProjected3, CameraPos: TVector3;
  CameraNearPlane: TVector4;
  GizmoScale: Single;
begin
  inherited;

  { Adjust scale to take the same space on screeen.
    We look at UniqueParent transform, not our transform,
    to ignore current Scale when measuring. }
  if (UniqueParent <> nil) and UniqueParent.HasWorldTransform then
  begin
    if ACamera.ProjectionType = ptOrthographic then
      GizmoScale := 0.001 * ACamera.Orthographic.EffectiveHeight
    else
      GizmoScale := 0.25 {TODO:* ACamera.Perspective.EffeectiveFieldOfViewVertical};

    W := UniqueParent.WorldTransform;
    ZeroWorld := W.MultPoint(TVector3.Zero);
    OneWorld := ZeroWorld + ACamera.GravityUp;

    (* TODO: why this fails:
    ViewProjectionMatrix := ACamera.ProjectionMatrix * ACamera.Matrix;
    ZeroProjected := (ViewProjectionMatrix * Vector4(ZeroWorld, 1)).XY;
    OneProjected := (ViewProjectionMatrix * Vector4(OneWorld, 1)).XY;
    *)

    CameraPos := ACamera.Position;
    CameraNearPlane.XYZ := ACamera.Direction;
    { plane equation should yield 0 when used with point in front of camera }
    CameraNearPlane.W := - TVector3.DotProduct(
      CameraPos + ACamera.Direction * AssumeNear, ACamera.Direction);
    if not TryPlaneLineIntersection(OneProjected3, CameraNearPlane, CameraPos, OneWorld - CameraPos) then
      Exit;
    if not TryPlaneLineIntersection(ZeroProjected3, CameraNearPlane, CameraPos, ZeroWorld - CameraPos) then
      Exit;

    ZeroProjected := ZeroProjected3.XY;
    OneProjected := OneProjected3.XY;
    // get the distance, on screen in pixels, of a 1 unit in 3D around gizmo
    OneDistance := PointsDistance(ZeroProjected, OneProjected);
    ScaleUniform := Max(0.01, GizmoScale / OneDistance);
    Scale := Vector3(ScaleUniform, ScaleUniform, ScaleUniform);
  end;
end;

function TVisualizeTransform.TGizmoScene.Dragging: boolean;
begin
  Result := (inherited Dragging) or GizmoDragging;
end;

function TVisualizeTransform.TGizmoScene.PointingDevicePress(
  const Pick: TRayCollisionNode; const Distance: Single): Boolean;
var
  AppearanceName: String;
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
      'MaterialX': DraggingAxis := 0;
      'MaterialY': DraggingAxis := 1;
      'MaterialZ': DraggingAxis := 2;
      else Exit;
    end;

    if PointOnAxis(LastPick, Pick, DraggingAxis) then
    begin
      GizmoDragging := true;
      // keep tracking pointing device events, by TCastleViewport.CapturePointingDevice mechanism
      Result := true;
      WritelnLog('Gizmo dragging axis %d', [DraggingAxis]);
    end;
  end;
end;

function TVisualizeTransform.TGizmoScene.PointingDeviceMove(
  const Pick: TRayCollisionNode; const Distance: Single): Boolean;
var
  NewPick, Diff: TVector3;
begin
  Result := inherited;
  if Result then Exit;

  if GizmoDragging then
  begin
    if PointOnAxis(NewPick, Pick, DraggingAxis) then
    begin
      Diff := NewPick - LastPick;
      case Operation of
        voTranslate:
          UniqueParent.Translation := UniqueParent.Translation + Diff;
        // TODO: rotate/scale are dummy tests
        //voRotate:
        //  UniqueParent.Rotation := (
        //    QuatFromAxisAngle(UniqueParent.Rotation) *
        //    QuatFromAxisAngle(TVector3.One[DraggingAxis], Diff.Length)).
        //    ToAxisAngle;
        //voScale:
        //  UniqueParent.Scale := UniqueParent.Scale + Diff;
      end;
      { No point in updating LastPick: it remains the same, as it is expressed
        in local coordinate system, which we just changed by changing
        UniqueParent.Translation. }
      WritelnLog('Gizmo: Moving pointing device, gizmo works!');

      // update our gizmo size, as we moved ourselves
      CameraChanged(World.MainCamera);
    end;
  end;
end;

function TVisualizeTransform.TGizmoScene.PointingDeviceRelease(
  const Pick: TRayCollisionNode; const Distance: Single;
  const CancelAction: Boolean): Boolean;
begin
  Result := inherited;
  if Result then Exit;

  GizmoDragging := false;
  WritelnLog('Gizmo: Released pointing device');
end;

{ TVisualizeTransform ------------------------------------------------------ }

constructor TVisualizeTransform.Create(AOwner: TComponent; const AHover: Boolean);

  function CreateGizmoScene: TGizmoScene;
  begin
    Result := TGizmoScene.Create(Self);
    Result.Collides := false;
    //Result.Pickable := false;
    Result.CastShadowVolumes := false;
    Result.ExcludeFromStatistics := true;
    Result.InternalExcludeFromParentBoundingVolume := true;
    Result.Spatial := [ssDynamicCollisions];
    Result.SetTransient;
  end;

begin
  inherited Create(AOwner);
  FHover := AHover;

  Box := TDebugTransformBox.Create(Self);
  if FHover then
    Box.BoxColor := ColorOpacity(ColorHover, 0.75)
  else
    Box.BoxColor := ColorOpacity(ColorSelected, 0.75);
  Box.Exists := true;

  // Gizmo[voSelect] remains nil
  Gizmo[voTranslate] := CreateGizmoScene;
  Gizmo[voTranslate].Load(EditorApplicationData + 'translate_final.x3dv');
  Gizmo[voTranslate].Operation := voTranslate;
  Gizmo[voRotate] := CreateGizmoScene;
  Gizmo[voRotate].Load(EditorApplicationData + 'rotate.glb');
  Gizmo[voRotate].Operation := voRotate;
  Gizmo[voScale] := CreateGizmoScene;
  Gizmo[voScale].Load(EditorApplicationData + 'scale.glb');
  Gizmo[voScale].Operation := voScale;
end;

destructor TVisualizeTransform.Destroy;
begin
  { set to nil by SetParent, to detach free notification }
  Parent := nil;
  inherited;
end;

procedure TVisualizeTransform.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FParent) then
    { set to nil by SetParent to clean all connections nicely }
    Parent := nil;
end;

procedure TVisualizeTransform.SetParent(const AValue: TCastleTransform);
begin
  if FParent = AValue then Exit;

  if FParent <> nil then
  begin
    FParent.RemoveFreeNotification(Self);
    Box.Parent := nil;
    if Gizmo[Operation] <> nil then
      FParent.Remove(Gizmo[Operation]);
  end;

  FParent := AValue;

  if FParent <> nil then
  begin
    Box.Parent := FParent;
    if Gizmo[Operation] <> nil then
      FParent.Add(Gizmo[Operation]);
    FParent.FreeNotification(Self);
  end;
end;

procedure TVisualizeTransform.SetOperation(const AValue: TVisualizeOperation);
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
