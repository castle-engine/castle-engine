{%MainUnit x3dloadinternalspine.pas}
{
  Copyright 2014-2023 Michalis Kamburelis.

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
    procedure Parse(const Json: TJSONArray;
      const NormalizeCurvesKeyValues: Boolean); virtual;
    procedure ParseSingleValue(const Json: TJSONObject); virtual; abstract;
    procedure BuildNodes(const BaseUrl: string;
      const MaxAnimationTime: Single; const Container: TX3DRootNode); virtual;
    function DoingSomething: boolean; virtual; abstract;
    procedure ParseControlPoints(const Json: TJSONArray;
      const NormalizeCurvesKeyValues: Boolean); virtual; abstract;
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
    procedure ParseControlPoints(const Json: TJSONArray;
      const NormalizeCurvesKeyValues: Boolean); override;
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
    procedure ParseControlPoints(const Json: TJSONArray;
      const NormalizeCurvesKeyValues: Boolean); override;
  end;

  TBoneTimelineList = class({$ifdef FPC}specialize{$endif} TObjectList<TBoneTimeline>)
  end;
{$endif}

{$ifdef read_implementation}

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

procedure TBoneTimeline.Parse(const Json: TJSONArray;
  const NormalizeCurvesKeyValues: Boolean);
var
  I: Integer;
  O: TJSONObject;
  NextTime: Single;
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

      // only detect whether we have a curve at this point
      if not Curve then
        Curve := O.Find('curve') is TJSONArray;

      ParseSingleValue(O);
    end;

  if Curve then
    ParseControlPoints(Json, NormalizeCurvesKeyValues);

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

procedure TBoneTimelineVector2.ParseControlPoints(const Json: TJSONArray;
  const NormalizeCurvesKeyValues: Boolean);
var
  I: Integer;
  PreviousTime, NextTime: Single;
  PreviousValue, NextValue: TVector4;
  O: TJSONObject;
begin
  Assert(Time.Count = Vectors.Count);
  if Json.Count <> Time.Count then
  begin
    WritelnWarning('Invalid Spine JSON: Curve support requires that timeline JSON count is correct');
    Curve := false;
    Exit;
  end;

  for I := 0 to Time.Count - 2 do
  begin
    PreviousTime := Time[I];
    NextTime     := Time[I + 1];
    PreviousValue := Vector4(Vectors.L[I    ].X, Vectors.L[I    ].Y, 0, 0);
    NextValue     := Vector4(Vectors.L[I + 1].X, Vectors.L[I + 1].Y, 0, 0);

    if not (Json[I] is TJSONObject) then
    begin
      WritelnWarning('Invalid Spine JSON: Curve support requires that all JSON timeline chldren are object');
      Curve := false;
      Exit;
    end;

    O := TJSONObject(Json[I]);

    CurveControlPoints.Add(ReadCurveControlPoints('bone(2D)', O, NormalizeCurvesKeyValues, 2,
      PreviousTime, NextTime,
      PreviousValue, NextValue));
  end;

  CurveControlPoints.Add(Vector4(0, 0, 1, 1)); // TODO: add last dummy value, because our TCubicBezier* may expect it
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
    end;
    NC.SetControlPoints(CurveControlPoints);
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
    Result.X := InitialScale.X * KeyScale.X;
    Result.Y := InitialScale.Y * KeyScale.Y;
    { This must be 1.0, not something like (Result.X + Result.Y) / 2,
      since scaling in Z will move the bone in front/back of the other bones. }
    Result.Z := 1.0;
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
    end;
    NC.SetControlPoints(CurveControlPoints);
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
    if (Vectors[I].X <> 1) or
       (Vectors[I].Y <> 1) then
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
var
  Angle: Single;
begin
  // around Spine >= 4.0 they changed the name of this to "value"
  if Json.IndexOfName('angle') <> -1 then
    Angle := Json.Get('angle', 0.0)
  else
    Angle := Json.Get('value', 0.0);
  Angles.Add(Angle);
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

procedure TBoneTimelineRotate.ParseControlPoints(const Json: TJSONArray;
  const NormalizeCurvesKeyValues: Boolean);
var
  I: Integer;
  PreviousTime, NextTime: Single;
  PreviousValue, NextValue: TVector4;
  O: TJSONObject;
begin
  Assert(Time.Count = Angles.Count);
  if Json.Count <> Time.Count then
  begin
    WritelnWarning('Invalid Spine JSON: Curve support requires that timeline JSON count is correct');
    Curve := false;
    Exit;
  end;

  for I := 0 to Time.Count - 2 do
  begin
    PreviousTime := Time[I];
    NextTime     := Time[I + 1];
    PreviousValue := Vector4(Angles.L[I    ], 0, 0, 0);
    NextValue     := Vector4(Angles.L[I + 1], 0, 0, 0);

    if not (Json[I] is TJSONObject) then
    begin
      WritelnWarning('Invalid Spine JSON: Curve support requires that all JSON timeline chldren are object');
      Curve := false;
      Exit;
    end;

    O := TJSONObject(Json[I]);

    CurveControlPoints.Add(ReadCurveControlPoints('bone(rotation)', O, NormalizeCurvesKeyValues, 1,
      PreviousTime, NextTime,
      PreviousValue, NextValue));
  end;

  CurveControlPoints.Add(Vector4(0, 0, 1, 1)); // TODO: add last dummy value, because our TCubicBezier* may expect it
end;

{$endif}
