{
  Copyright 2014-2018 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Spine bone timelines. }

{$ifdef read_interface}
  TBoneTimeline = class abstract
  strict private
    FMaxTime: Single;
  protected
    IsBackup: Boolean;
  public
    Bone: TBone;
    Time: TSingleList;
    BackupTime: TSingleList;
    Curve: boolean;
    CurveControlPoints: TVector4List;
    Node: TAbstractInterpolatorNode;
    NodeUsedAsChild: boolean;
    property MaxTime: Single read FMaxTime;
    constructor Create;
    destructor Destroy; override;
    procedure Parse(const Json: TJSONArray); virtual;
    procedure ParseSingleValue(const Json: TJSONObject); virtual; abstract;
    procedure BuildNodes(const BaseUrl: string;
      const MaxAnimationTime: Single; const Container: TX3DRootNode); virtual;
    function DoingSomething: boolean; virtual; abstract;
    procedure Backup; virtual;
    procedure Restore; virtual;
  end;

  TBoneTimelineVector2 = class(TBoneTimeline)
  public
    { Position or scale values on the timeline.
      This always has the same length as @link(Time) list. }
    Vectors: TVector2List;
    BackupVectors: TVector2List;
    constructor Create;
    destructor Destroy; override;
    procedure Backup; override;
    procedure Restore; override;
  end;

  TBoneTimelineTranslate = class(TBoneTimelineVector2)
    procedure ParseSingleValue(const Json: TJSONObject); override;
    procedure BuildNodes(const BaseUrl: string;
      const MaxAnimationTime: Single; const Container: TX3DRootNode); override;
    function DoingSomething: boolean; override;
  end;

  TBoneTimelineScale = class(TBoneTimelineVector2)
    procedure ParseSingleValue(const Json: TJSONObject); override;
    procedure BuildNodes(const BaseUrl: string;
      const MaxAnimationTime: Single; const Container: TX3DRootNode); override;
    function DoingSomething: boolean; override;
  end;

  TBoneTimelineRotate = class(TBoneTimeline)
    { Angle values on the timeline.
      This always has the same length as @link(Time) list. }
    Angles: TSingleList;
    BackupAngles: TSingleList;
    constructor Create;
    destructor Destroy; override;
    procedure ParseSingleValue(const Json: TJSONObject); override;
    procedure BuildNodes(const BaseUrl: string;
      const MaxAnimationTime: Single; const Container: TX3DRootNode); override;
    function DoingSomething: boolean; override;
    procedure Backup; override;
    procedure Restore; override;
  end;

  TBoneTimelineList = class({$ifdef CASTLE_OBJFPC}specialize{$endif} TObjectList<TBoneTimeline>)
  end;
{$endif}

{$ifdef read_implementation}

const
  SSpine1KeyFrameWarning = 'Timeline on %s "%s" (animation of %s) has only 1 keyframe. We support it in a consistent way (1 keyframe means that the state is constant), but Spine shows such timeline inconsistently. To avoid confusion, set at least 2 keyframes on every timeline.';
  // SSpineMultipleTimelinesWarning = 'Multiple timelines affect %s "%s" animation of %s';

{ TBoneTimeline -------------------------------------------------------------- }

constructor TBoneTimeline.Create;
begin
  inherited;
  Time := TSingleList.Create;
  BackupTime := TSingleList.Create;
  CurveControlPoints := TVector4List.Create;
end;

destructor TBoneTimeline.Destroy;
begin
  FreeAndNil(Time);
  FreeAndNil(BackupTime);
  FreeAndNil(CurveControlPoints);
  inherited;
end;

procedure TBoneTimeline.Parse(const Json: TJSONArray);
var
  I: Integer;
  O: TJSONObject;
  NextTime: Single;
  ControlPoints: TVector4;
  CurveTypeJson: TJSONData;
begin
  Curve := false;

  for I := 0 to Json.Count - 1 do
    if Json[I] is TJSONObject then
    begin
      O := TJSONObject(Json[I]);

      NextTime := O.Get('time', 0.0);
      if (Time.Count <> 0) and (Time.Last > NextTime) then
        raise ESpineReadError.Create('Timeline must have increasing time values');
      Time.Add(NextTime);
      FMaxTime := NextTime;

      ControlPoints := Vector4(0, 0, 1, 1); // default value, in case curve is later changed from linear to curve
      CurveTypeJson := O.Find('curve');
      if CurveTypeJson <> nil then
      begin
        if CurveTypeJson is TJSONArray then
        begin
          if TJSONArray(CurveTypeJson).Count <> 4 then
            WritelnWarning('Spine', 'Curve type interpolation is an array, but does not have 4 elements (required for Bezier curve array)') else
          begin
            Curve := true;
            ControlPoints[0] := TJSONArray(CurveTypeJson).Floats[0];
            ControlPoints[1] := TJSONArray(CurveTypeJson).Floats[1];
            ControlPoints[2] := TJSONArray(CurveTypeJson).Floats[2];
            ControlPoints[3] := TJSONArray(CurveTypeJson).Floats[3];
          end;
        end;

        { For now, silently ignore warning that we don't handle curve type
          'stepped'. Spine optimizes it's curves by using 'stepped' where
          previous and next values are equal, so this is common, and we would
          flood the warnings console for many models because of this (while in fact
          models are handled Ok since 'stepped' is only for optimization). }
        { else
        if CurveTypeJson.AsString <> 'linear' then
          WritelnWarning('Spine', 'Found "' + CurveTypeJson.AsString + '" interpolation type on bone timeline of bone ' + Bone.Name + ', we do not support this interpolation type');
        }
      end;
      CurveControlPoints.Add(ControlPoints);

      ParseSingleValue(O);
    end;
  //Writeln('got timeline for bone ', Bone.Name, ' with ', Time.Count, ' items');
end;

procedure TBoneTimeline.BuildNodes(const BaseUrl: string;
  const MaxAnimationTime: Single; const Container: TX3DRootNode);
var
  I: Integer;
begin
  { We assume that descendant already created Node in overridden BuildNodes }
  Node.FdKey.Items.Clear;
  for I := 0 to Time.Count - 1 do
    Node.FdKey.Items.Add(Time[I] / MaxAnimationTime);

  NodeUsedAsChild := true;
  Container.AddChildren(Node);
end;

procedure TBoneTimeline.Backup;
begin
end;

procedure TBoneTimeline.Restore;
begin
end;

{ TBoneTimelineVector2 ------------------------------------------------------- }

constructor TBoneTimelineVector2.Create;
begin
  inherited Create;
  Vectors := TVector2List.Create;
  BackupVectors := TVector2List.Create;
end;

destructor TBoneTimelineVector2.Destroy;
begin
  FreeAndNil(Vectors);
  FreeAndNil(BackupVectors);
  inherited;
end;

procedure TBoneTimelineVector2.Backup;
begin
  if not IsBackup then
  begin
    BackupTime.Clear;
    BackupVectors.Clear;
    BackupTime.AddRange(Time);
    BackupVectors.AddRange(Vectors);
    IsBackup := True;
  end;
end;

procedure TBoneTimelineVector2.Restore;
begin
  if IsBackup then
  begin
    Time.Clear;
    Vectors.Clear;
    Time.AddRange(BackupTime);
    Vectors.AddRange(BackupVectors);
    IsBackup := False;
  end;
end;

{ TBoneTimelineTranslate ----------------------------------------------------- }

procedure TBoneTimelineTranslate.ParseSingleValue(const Json: TJSONObject);
begin
  Vectors.Add(Vector2(
    Json.Get('x', 0.0),
    Json.Get('y', 0.0)));
end;

procedure TBoneTimelineTranslate.BuildNodes(const BaseUrl: string;
  const MaxAnimationTime: Single; const Container: TX3DRootNode);
var
  I: Integer;
  N: TPositionInterpolatorNode;
  NC: TCubicBezierPositionInterpolatorNode;
  EventValueChanged: TX3DEvent;
  Route: TX3DRoute;
begin
  if Curve then
  begin
    NC := TCubicBezierPositionInterpolatorNode.Create('BoneTimeline_translate_' + Bone.Name);
    for I := 0 to Vectors.Count - 1 do
    begin
      NC.FdKeyValue.Items.Add(Vector3(
        { add setup pose transform, as bone timeline is relative to setup pose }
        Bone.Node.FdTranslation.Value[0] + Vectors[I][0],
        Bone.Node.FdTranslation.Value[1] + Vectors[I][1], 0));
      NC.FdControlPoints.Items.Add(CurveControlPoints[I]);
    end;
    NC.OptimizeControlPoints;
    EventValueChanged := NC.EventValue_changed;
    Node := NC;
    //WritelnLog('Spine', 'Using curve for translation on bone ' + Bone.Name);
  end else
  begin
    N := TPositionInterpolatorNode.Create('BoneTimeline_translate_' + Bone.Name);
    for I := 0 to Vectors.Count - 1 do
      N.FdKeyValue.Items.Add(Vector3(
        { add setup pose transform, as bone timeline is relative to setup pose }
        Bone.Node.FdTranslation.Value[0] + Vectors[I][0],
        Bone.Node.FdTranslation.Value[1] + Vectors[I][1], 0));
    EventValueChanged := N.EventValue_changed;
    Node := N;
  end;

  inherited;

  Route := TX3DRoute.Create;
  Route.SetSourceDirectly(EventValueChanged);
  Route.SetDestinationDirectly(Bone.Node.FdTranslation.EventIn);
  Container.AddRoute(Route);

  { When there's only 1 keyframe, Spine does something weird: instead of smooth
    interpolation, animation suddenly jumps from setup pose to given keyframe
    pose at given key time (unless animation is looping and it's the last frame,
    then it's visibly ignored). }
  if SpineVerboseWarnings and (Time.Count = 1) then
    WritelnWarning('Spine', Format(SSpine1KeyFrameWarning, ['bone', Bone.Name, 'translation']));
end;

function TBoneTimelineTranslate.DoingSomething: boolean;
var
  I: Integer;
begin
  for I := 0 to Vectors.Count - 1 do
    if not Vectors[I].IsPerfectlyZero then
      Exit(true);
  Result := false;
end;

{ TBoneTimelineScale --------------------------------------------------------- }

procedure TBoneTimelineScale.ParseSingleValue(const Json: TJSONObject);
begin
  Vectors.Add(Vector2(
    Json.Get('x', 1.0),
    Json.Get('y', 1.0)));
end;

procedure TBoneTimelineScale.BuildNodes(const BaseUrl: string;
  const MaxAnimationTime: Single; const Container: TX3DRootNode);

  { Combine initial node scale and 2D key scale into a final scale. }
  function FinalScale(const InitialScale: TVector3;
    const KeyScale: TVector2): TVector3;
  begin
    { multiply setup pose transformation (scale) by key scale,
      as bone timeline is relative to setup pose }
    Result[0] := InitialScale[0] * KeyScale[0];
    Result[1] := InitialScale[1] * KeyScale[1];
    { This must be 1.0, not something like (Result[0] + Result[1]) / 2,
      since scaling in Z will move the bone in front/back of the other bones. }
    Result[2] := 1.0;
  end;

var
  I: Integer;
  N: TPositionInterpolatorNode;
  NC: TCubicBezierPositionInterpolatorNode;
  EventValueChanged: TX3DEvent;
  Route: TX3DRoute;
begin
  if Curve then
  begin
    NC := TCubicBezierPositionInterpolatorNode.Create('BoneTimeline_scale_' + Bone.Name);
    for I := 0 to Vectors.Count - 1 do
    begin
      NC.FdKeyValue.Items.Add(FinalScale(Bone.Node.FdScale.Value, Vectors[I]));
      NC.FdControlPoints.Items.Add(CurveControlPoints[I]);
    end;
    NC.OptimizeControlPoints;
    EventValueChanged := NC.EventValue_changed;
    Node := NC;
    //WritelnLog('Spine', 'Using curve for scale on bone ' + Bone.Name);
  end else
  begin
    N := TPositionInterpolatorNode.Create('BoneTimeline_scale_' + Bone.Name);
    for I := 0 to Vectors.Count - 1 do
      N.FdKeyValue.Items.Add(FinalScale(Bone.Node.FdScale.Value, Vectors[I]));
    EventValueChanged := N.EventValue_changed;
    Node := N;
  end;
  inherited;

  Route := TX3DRoute.Create;
  Route.SetSourceDirectly(EventValueChanged);
  Route.SetDestinationDirectly(Bone.Node.FdScale.EventIn);
  Container.AddRoute(Route);

  { When there's only 1 keyframe, Spine does something weird: instead of smooth
    interpolation, animation suddenly jumps from setup pose to given keyframe
    pose at given key time (unless animation is looping and it's the last frame,
    then it's visibly ignored). }
  if SpineVerboseWarnings and (Time.Count = 1) then
    WritelnWarning('Spine', Format(SSpine1KeyFrameWarning, ['bone', Bone.Name, 'scale']));
end;

function TBoneTimelineScale.DoingSomething: boolean;
var
  I: Integer;
begin
  for I := 0 to Vectors.Count - 1 do
    if (Vectors[I][0] <> 1) or
       (Vectors[I][1] <> 1) then
      Exit(true);
  Result := false;
end;

{ TBoneTimelineRotate -------------------------------------------------------- }

constructor TBoneTimelineRotate.Create;
begin
  inherited;
  Angles := TSingleList.Create;
  BackupAngles := TSingleList.Create;
end;

destructor TBoneTimelineRotate.Destroy;
begin
  FreeAndNil(Angles);
  FreeAndNil(BackupAngles);
  inherited;
end;

procedure TBoneTimelineRotate.ParseSingleValue(const Json: TJSONObject);
begin
  Angles.Add(Json.Get('angle', 0.0));
end;

procedure TBoneTimelineRotate.BuildNodes(const BaseUrl: string;
  const MaxAnimationTime: Single; const Container: TX3DRootNode);
var
  I: Integer;
  N: TOrientationInterpolator2DNode;
  NC: TCubicBezier2DOrientationInterpolatorNode;
  EventValueChanged: TX3DEvent;
  Route: TX3DRoute;
  AngleValue: Single;
begin
  if Curve then
  begin
    NC := TCubicBezier2DOrientationInterpolatorNode.Create('BoneTimeline_rotate_' + Bone.Name);
    NC.FdAxis.Value := Vector3(0, 0, 1);
    for I := 0 to Angles.Count - 1 do
    begin
      { add setup pose transform, as bone timeline is relative to setup pose }
      AngleValue := Bone.Node.FdRotation.Value[3] + DegToRad(Angles[I]);
      NC.FdKeyValue.Items.Add(AngleValue);
      NC.FdControlPoints.Items.Add(CurveControlPoints[I]);
    end;
    NC.OptimizeControlPoints;
    EventValueChanged := NC.EventValue_changed;
    Node := NC;
    //WritelnLog('Spine', 'Using curve for rotation on bone ' + Bone.Name);
  end else
  begin
    { We use TOrientationInterpolator2DNode instead of
      TOrientationInterpolatorNode with constant axis.
      This is not only for optimization (TOrientationInterpolator2DNode
      does trivial lerp using AngleLerp, instead of SLerp on 3D rotations),
      it's also for correctness: TOrientationInterpolatorNode could choose
      to interpolate through a 3D space sometimes, it seems.

      Testcase: Unholy exorcist going_on_ladder_up animation hand movement.
      It could do unexpected interpolation using slerp:
      (this was before SLerp was hacked to specially honor case when Rot1Axis equals Rot2Axis).

          uses CastleVectors, CastleQuaternions;
          const
            Steps = 10;
          var
            V: TVector4;
            I: Integer;
          begin
            for I := 0 to Steps - 1 do
            begin
              V := SLerp(I / (Steps - 1),
                Vector4(0, 0, 1, -1.6584116220474243),
                Vector4(0, 0, 1, 4.6247735023498535)
              );
              Writeln(V.ToString);
            end;
          end.
    }
    N := TOrientationInterpolator2DNode.Create('BoneTimeline_rotate_' + Bone.Name);
    for I := 0 to Angles.Count - 1 do
    begin
      { add setup pose transform, as bone timeline is relative to setup pose }
      AngleValue := Bone.Node.FdRotation.Value[3] + DegToRad(Angles[I]);
      N.FdKeyValue.Items.Add(AngleValue);
    end;
    EventValueChanged := N.EventValue_changed;
    Node := N;
  end;

  inherited;

  Route := TX3DRoute.Create;
  Route.SetSourceDirectly(EventValueChanged);
  Route.SetDestinationDirectly(Bone.Node.FdRotation.EventIn);
  Container.AddRoute(Route);

  { When there's only 1 keyframe, Spine does something weird: instead of smooth
    interpolation, animation suddenly jumps from setup pose to given keyframe
    pose at given key time (unless animation is looping and it's the last frame,
    then it's visibly ignored). }
  if SpineVerboseWarnings and (Time.Count = 1) then
    WritelnWarning('Spine', Format(SSpine1KeyFrameWarning, ['bone', Bone.Name, 'rotation']));
end;

function TBoneTimelineRotate.DoingSomething: boolean;
var
  I: Integer;
begin
  for I := 0 to Angles.Count - 1 do
    if Angles[I] <> 0 then
      Exit(true);
  Result := false;
end;

procedure TBoneTimelineRotate.Backup;
begin
  if not IsBackup then
  begin
    BackupTime.Clear;
    BackupAngles.Clear;
    BackupTime.AddRange(Time);
    BackupAngles.AddRange(Angles);
    IsBackup := True;
  end;
end;

procedure TBoneTimelineRotate.Restore;
begin
  if IsBackup then
  begin
    Time.Clear;
    Angles.Clear;
    Time.AddRange(BackupTime);
    Angles.AddRange(BackupAngles);
    IsBackup := False;
  end;
end;

{$endif}
